// Node-only contract test: server echoes cannot become user selections.
const fs=require('fs'),vm=require('vm'),assert=require('assert');
const handlers={},messages=[],events=[],custom={};
const root={dataset:{scope:'dataset|'},querySelector:()=>select};
const select={id:'embedding',closest:()=>root,matches:()=>true,dataset:{},value:'old'};
const holder={querySelector:()=>select};
const option={dataset:{value:'new'},closest:()=>holder};
const context={window:{},CSS:{escape:x=>x},performance:{now:()=>1},
  CustomEvent:function(type,x){return {type,detail:x.detail};},
  document:{getElementById:id=>id==='gf_grouped_selectors'?root:{},
    addEventListener:(k,f)=>handlers[k]=f,dispatchEvent:e=>events.push(e)},
  Shiny:{setInputValue:(k,v)=>messages.push([k,v]),addCustomMessageHandler:(k,f)=>custom[k]=f},
  MutationObserver:function(){this.observe=()=>{};}, $:f=>{if(typeof f==='function')f();}};
vm.runInNewContext(fs.readFileSync('inst/app/www/graph-selection.js','utf8'),context);
handlers.pointerdown({target:{closest:()=>null}}); // Opening is not choosing.
handlers.change({isTrusted:false,target:select}); // Programmatic Shiny echo.
assert.equal(messages.length,0);
handlers.pointerdown({target:{closest:()=>option}});
assert.equal(messages.length,1);assert.equal(messages[0][1].value,'new');
handlers.change({isTrusted:false,target:select});
assert.equal(messages.length,1);
const keyHolder={querySelector:()=>option};
handlers.keydown({key:'Enter',target:{closest:()=>keyHolder}});
assert.equal(messages.length,2);assert(messages[1][1].seq>messages[0][1].seq);
context.window.gflowuiGraphSelection.select('embedding','saved');
assert.equal(messages[2][1].value,'saved');assert.equal(events.length,3);
console.log('Selection event regression passed');
