// Run against installed adapter directories, without starting their CLIs.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
const root = process.argv[2];
assert(root, 'Pass the directory containing codex-acp and claude-agent-acp');
for (const adapter of ['codex-acp', 'claude-agent-acp']) {
  const sdk = `${root}/${adapter}/node_modules/@agentclientprotocol/sdk`;
  const {Connection} = await import(pathToFileURL(`${sdk}/dist/jsonrpc.js`));
  const {zCreateElicitationResponse} = await import(pathToFileURL(`${sdk}/dist/schema/zod.gen.js`));
  for (const content of [undefined, null, {}]) {
    assert(zCreateElicitationResponse.safeParse({action:'accept', ...(content === undefined ? {} : {content})}).success);
  }
  const sent = [];
  const connection = new Connection(new TransformStream(), []);
  connection.sendWireMessage = async msg => { sent.push(msg); };
  const abort = new AbortController();
  const pending = connection.sendRequest('elicitation/create', {}, undefined, {cancellationSignal:abort.signal});
  let settled = false;
  pending.then(() => { settled = true; }, () => { settled = true; });
  abort.abort();
  await Promise.resolve();
  assert.equal(settled, false);
  assert.deepEqual(sent[1], {jsonrpc:'2.0',method:'$/cancel_request',params:{requestId:sent[0].id}});
  connection.handleResponse({jsonrpc:'2.0',id:sent[0].id,error:{code:-32800,message:'Request cancelled'}});
  await assert.rejects(pending, e => e.code === -32800);
  const errors = [];
  const oldError = console.error;
  console.error = (...args) => errors.push(args);
  try { connection.handleResponse({jsonrpc:'2.0',id:sent[0].id,result:{action:'accept'}}); }
  finally { console.error = oldError; }
  assert.equal(errors.length, 1);
  connection.close();
  console.log(adapter, 'SDK', JSON.parse(fs.readFileSync(`${sdk}/package.json`)).version, 'cancellation + empty accept: PASS');
}
const source = fs.readFileSync(`${root}/codex-acp/dist/index.js`, 'utf8');
const start = source.indexOf('var CodexElicitationHandler = class {');
const end = source.indexOf('\n// src/CodexAuthMethod.ts', start);
assert(start > 0 && end > start);
let timers;
const Handler = vm.runInNewContext(`${source.slice(start,end)}; CodexElicitationHandler`, {
  AbortController, setTimeout: (fn, ms) => { timers.push({fn,ms}); return timers.length; }, clearTimeout:()=>{},
  methods:{client:{elicitation:{create:'elicitation/create'}}},
});
for (const isBlocking of [undefined, false, true]) {
  for (const autoResolutionMs of [undefined, null, 0, 25]) {
    timers = [];
    const handler = Object.create(Handler.prototype);
    handler.buildUserInputRequest = p => p;
    let signal, resolveRequest;
    handler.connection = {request: (_method, _params, opts) => {
      signal = opts?.cancellationSignal;
      return new Promise(resolve => { resolveRequest = resolve; });
    }};
    const promise = handler.requestUserInputElicitation({isBlocking, autoResolutionMs});
    if (autoResolutionMs === null) {
      assert.equal(timers.length, 0);
      resolveRequest({action:'decline'});
      assert.equal((await promise).action, 'decline');
    } else {
      assert.equal(timers.length, 1);
      assert.equal(timers[0].ms, autoResolutionMs ?? 0);
      timers[0].fn();
      assert.equal(signal.aborted, true);
      assert.equal(await promise, null);
      resolveRequest({action:'accept',content:{}});
    }
    console.log(JSON.stringify({isBlocking, autoResolutionMs, timer:timers[0]?.ms ?? null}), 'PASS');
  }
}
if (process.argv[3]) {
  const params = {
    threadId: 'phase2-source', itemId: 'source-question', turnId: 'source-turn',
    isBlocking: true, autoResolutionMs: null,
    questions: [
      {id:'text', header:'Text', question:'Enter a synthetic label.', isOther:false, isSecret:false, options:null},
      {id:'password', header:'Secret', question:'Enter a synthetic secret.', isOther:false, isSecret:true, options:null},
    ],
  };
  const request = Object.create(Handler.prototype).buildUserInputRequest(params);
  fs.writeFileSync(process.argv[3], JSON.stringify({
    provenance: {
      adapter: 'codex-acp ' + JSON.parse(fs.readFileSync(`${root}/codex-acp/package.json`)).version,
      sdk: JSON.parse(fs.readFileSync(`${root}/codex-acp/node_modules/@agentclientprotocol/sdk/package.json`)).version,
      backend: 'codex ' + JSON.parse(fs.readFileSync(`${root}/codex-acp/node_modules/@openai/codex/package.json`)).version,
      adapterBundleSha256: createHash('sha256').update(source).digest('hex'),
      generation:'Installed buildUserInputRequest with synthetic backend parameters; not a live model request',
      command:'node tests/phase2-protocol.mjs ADAPTER_ROOT OUTPUT_FILE'},
    backendInput: params,
    request:{jsonrpc:'2.0',id:'source-text',method:'elicitation/create',params:request},
  },null,2)+'\n');
}
