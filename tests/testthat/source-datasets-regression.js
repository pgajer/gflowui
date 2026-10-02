const fs=require('fs'),vm=require('vm'),assert=require('assert'),EventEmitter=require('events');
const sent=[];
const ctx={window:{},document:{},$:()=>({on:()=>{}}),setTimeout:()=>{},
  Shiny:{setInputValue:(...args)=>sent.push(args)}};
vm.runInNewContext(fs.readFileSync('inst/app/www/source-datasets.js','utf8'),ctx);
const el=new EventEmitter();
for(let i=0;i<4;i++)ctx.window.gflowuiWithinMount(el,'test-');
assert.equal(el.listenerCount('plotly_click'),1);
assert.equal(el.listenerCount('plotly_selected'),1);
el.emit('plotly_click',{points:[{customdata:'composition_00001'}]});
assert.equal(sent.length,1);
assert.equal(sent[0][0],'test-plotly_click-within_dcst');
assert.deepEqual(JSON.parse(sent[0][1]),[{key:'composition_00001'}]);
console.log('Within-dCST event regression passed');
