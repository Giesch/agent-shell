// Native smoke probe. Requires an existing, dedicated Emacs test server.
// EMACS_SOCKET_NAME=NAME node tests/phase2-native-probe.mjs BINARY PROVIDER MODE
// PROVIDER: codex | claude. MODE: option | other | text | cancel.
import {spawn, execFileSync} from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const [binary, provider, mode = 'option'] = process.argv.slice(2);
const socket = process.env.EMACS_SOCKET_NAME;
if (!binary || !['codex', 'claude'].includes(provider) ||
    !['option', 'other', 'text', 'cancel'].includes(mode) || !socket || socket === 'server') {
  throw new Error('Supply BINARY, PROVIDER, MODE and EMACS_SOCKET_NAME for a dedicated existing test server');
}
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'phase2-native-'));
const child = spawn(binary, [], {cwd:directory, stdio:['pipe', 'pipe', 'pipe']});
const pending = new Map();
const traffic = [];
const answers = [];
let nextId = 0, sessionId, partial = '', formCount = 0, cancelCount = 0, finalText = '';
let finished = false;
const deadline = setTimeout(() => finish('timeout'), 90000);

function write(message) {
  traffic.push({direction:'outgoing', ...message});
  child.stdin.write(JSON.stringify(message) + '\n');
}
function request(method, params) {
  return new Promise((resolve, reject) => {
    const id = ++nextId;
    pending.set(id, {resolve, reject});
    write({jsonrpc:'2.0', id, method, params});
  });
}
function answerForm(message) {
  formCount++;
  if (mode === 'cancel') {
    write({jsonrpc:'2.0', method:'$/cancel_request', params:{requestId:nextId}});
    return;
  }
  const input = path.join(directory, 'request.json');
  const output = path.join(directory, 'response.json');
  fs.writeFileSync(input, JSON.stringify(message));
  const quote = JSON.stringify;
  const expression = `
  (progn
    (load ${quote(path.join(root, 'tests/run-batch.el'))} nil t)
    (agent-shell-tests--load-candidates ${quote(root)})
    (load ${quote(path.join(root, 'tests/agent-shell-elicitation-tests.el'))} nil t)
    (agent-shell-elicitation-tests-native-response
     ${quote(input)} ${quote(output)} ${quote(mode)}))`;
  execFileSync('emacsclient', ['--alternate-editor=false', '-s', socket, '--eval', expression]);
  const response = JSON.parse(fs.readFileSync(output));
  if (response.id !== message.id || response.result?.action !== 'accept') {
    throw new Error('Native UI response did not accept the original request ID');
  }
  answers.push(...Object.values(response.result.content ?? {}).flat());
  write(response);
}
function receive(message) {
  traffic.push({direction:'incoming', ...message});
  if (message.method) {
    if (Object.hasOwn(message, 'id')) {
      if (message.method === 'elicitation/create') answerForm(message);
      else if (message.method === 'session/request_permission') {
        write({jsonrpc:'2.0', id:message.id, result:{outcome:{outcome:'cancelled'}}});
      } else write({jsonrpc:'2.0', id:message.id, error:{code:-32601, message:'Unsupported test request'}});
    } else if (message.method === '$/cancel_request') {
      cancelCount++;
      write({jsonrpc:'2.0', id:message.params.requestId, error:{code:-32800, message:'Request cancelled'}});
    } else if (message.params?.update?.sessionUpdate === 'agent_message_chunk') {
      finalText += message.params.update.content?.text ?? '';
    }
  } else if (pending.has(message.id)) {
    const waiter = pending.get(message.id);
    pending.delete(message.id);
    if (message.error) waiter.reject(new Error('Adapter returned a protocol error'));
    else waiter.resolve(message.result);
  }
}
child.stdout.on('data', data => {
  partial += data;
  let end;
  while ((end = partial.indexOf('\n')) >= 0) {
    const line = partial.slice(0, end);
    partial = partial.slice(end + 1);
    try { receive(JSON.parse(line)); }
    catch { finish('protocol-or-ui-error'); return; }
  }
});
child.stderr.on('data', () => {});
child.on('error', () => finish('process-error'));
child.on('exit', () => finish('process-exit'));
function finish(outcome) {
  if (finished) return;
  finished = true;
  clearTimeout(deadline);
  for (const waiter of pending.values()) waiter.reject(new Error('Probe finished'));
  pending.clear();
  const acknowledged = answers.length > 0 && answers.every(answer => finalText.includes(String(answer)));
  const passed = formCount > 0 && (mode === 'cancel' ? cancelCount > 0 : acknowledged);
  fs.writeFileSync(path.join(directory, 'traffic.json'), JSON.stringify(traffic, null, 2));
  console.log(JSON.stringify({provider, mode, outcome, formCount, cancelCount, acknowledged, passed, directory}));
  process.exitCode = passed ? 0 : 2;
  child.kill('SIGTERM');
  setTimeout(() => child.kill('SIGKILL'), 1000).unref();
}
try {
  const initialization = await request('initialize', {
    protocolVersion:1, clientInfo:{name:'agent-shell-phase2-test', version:'1'},
    clientCapabilities:{elicitation:{form:{}}, fs:{readTextFile:false, writeTextFile:false}},
  });
  console.log(JSON.stringify({agent:initialization.agentInfo}));
  const session = await request('session/new', {cwd:directory, mcpServers:[]});
  sessionId = session.sessionId;
  if (provider === 'codex') {
    await request('session/set_config_option', {sessionId, configId:'collaboration_mode', value:'plan'});
  }
  const tool = provider === 'codex' ? 'request_user_input' : 'AskUserQuestion';
  const question = mode === 'text'
    ? 'Ask one standalone free-text question with no options: What nickname should I use?'
    : 'Ask me to choose Alpha or Beta. Offer an Other answer if supported.';
  const result = await request('session/prompt', {sessionId, prompt:[{type:'text', text:
    `Use ${tool} now. ${question} Call the tool; do not ask in plain text. ` +
    'This is a question UI test. Do not inspect files, execute commands, or implement anything. ' +
    'After receiving the answer, repeat it exactly in your final response.'}]});
  finish(result.stopReason);
} catch { finish('request-error'); }
