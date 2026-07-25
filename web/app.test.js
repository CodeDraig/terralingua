const test = require('node:test');
const assert = require('node:assert/strict');

const { reconstructWorldHistoryFromEvents } = require('./app.js');

test('reconstructs initial agents, movement, and final-step events at their own timestep', () => {
  const history = reconstructWorldHistoryFromEvents([
    { ts: 0, type: 'agent-added', tag: 'being0', name: 'Aelion', pos: { x: 0, y: 0 } },
    { ts: 0, type: 'agent-moved', tag: 'being0', pos: { x: 1, y: 0 } },
    { ts: 0, type: 'artifact-added', name: 'scroll', payload: 'first', pos: { x: 1, y: 0 } },
    { ts: 1, type: 'agent-moved', tag: 'being0', pos: { x: 2, y: 0 } },
    { ts: 2, type: 'agent-died', tag: 'being0' },
    { ts: 2, type: 'artifact-removed', name: 'scroll' }
  ], 2);

  assert.deepEqual(history[0].beings.being0.pos, { x: 1, y: 0 });
  assert.equal(history[0].artifacts.length, 1);
  assert.deepEqual(history[1].beings.being0.pos, { x: 2, y: 0 });
  assert.equal(history[2].beings.being0, undefined);
  assert.equal(history[2].artifacts.length, 0);
});
