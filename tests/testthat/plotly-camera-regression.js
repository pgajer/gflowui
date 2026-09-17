const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const { EventEmitter } = require('node:events');

async function testCamera() {
  const initial = {eye: {x: 1.25, y: 1.25, z: 1.25}};
  const rotated = {
    eye: {x: -0.6, y: 2.7, z: 0.4},
    center: {x: 0.2, y: -0.3, z: 0.1},
    up: {x: 0, y: 0, z: 1},
    projection: {type: 'perspective'}
  };
  let gd = new EventEmitter();
  gd.id = 'reference_plot';
  gd._fullLayout = {scene: {camera: initial}};
  const timers = [];
  const sent = [];
  const window = {Shiny: {setInputValue: (id, camera) => sent.push(camera)}};
  const context = {
    window,
    document: {getElementById: () => gd},
    setTimeout: fn => timers.push(fn),
    Plotly: {relayout: (plot, update) => {
      plot._fullLayout.scene.camera = update['scene.camera'];
      plot.emit('plotly_afterplot');
      plot.emit('plotly_relayout', update);
      return Promise.resolve();
    }}
  };
  const hook = vm.runInNewContext('(' + fs.readFileSync(process.argv[2], 'utf8') + ')', context);
  const same = (a, b) => assert.equal(JSON.stringify(a), JSON.stringify(b));
  const flush = async () => {
    while (timers.length) timers.shift()();
    await Promise.resolve();
    await Promise.resolve();
  };
  hook(gd, {});
  gd._fullLayout.scene.camera = rotated;
  gd.emit('plotly_relayout', {'scene.camera': rotated});
  same(sent.at(-1), rotated);

  // An endpoint redraw can briefly expose the default camera before onRender.
  gd._fullLayout.scene.camera = initial;
  gd.emit('plotly_afterplot');
  same(window.__gflowuiReferenceCamera, rotated);
  gd.emit('plotly_relayout', {autosize: true});
  same(window.__gflowuiReferenceCamera, rotated);
  hook(gd, {});
  await flush();
  same(gd._fullLayout.scene.camera, rotated);

  // Successive renders must not accumulate listeners or lose later movements.
  hook(gd, {});
  await flush();
  assert.equal(gd.listenerCount('plotly_relayout'), 1);
  assert.equal(gd.listenerCount('plotly_relayouting'), 1);
  const zoomed = {...rotated, eye: {x: -0.3, y: 1.35, z: 0.2}};
  gd._fullLayout.scene.camera = zoomed;
  gd.emit('plotly_relayouting', {'scene.camera': zoomed});
  same(window.__gflowuiReferenceCamera, zoomed);

  // Shiny's renderUI can replace the entire plot element as well.
  gd = new EventEmitter();
  gd.id = 'reference_plot';
  gd._fullLayout = {scene: {camera: initial}};
  hook(gd, {});
  await flush();
  same(gd._fullLayout.scene.camera, zoomed);
  console.log('Camera retained across redraw, resize, repeated render, and element replacement.');
}
testCamera().catch(error => { console.error(error); process.exitCode = 1; });
