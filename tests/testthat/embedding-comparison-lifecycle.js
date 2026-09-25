const fs = require("fs"), vm = require("vm"), assert = require("assert");
const events = {}, callbacks = [], widths = [], attrs = {};
let inserted = false, shinyInputs = 0;
const lazyEvents = {}, sent = [];
const lazyDetails = {
  open:false,
  getAttribute:key=>key==="data-ec-open-input"?"embedding_comparison-run_details_open":null,
  addEventListener:(key,fn)=>{lazyEvents[key]=fn;}
};
const handlers = {};
const handle = {
  setAttribute:(key,value)=>{attrs[key]=value;},
  addEventListener:(key,fn)=>{(handlers[key] ||= []).push(fn);}
};
const inspector = {getBoundingClientRect:()=>({width:400})};
const split = {
  dataset:{},
  style:{setProperty:(key,value)=>{widths.push([key,value]);}},
  getBoundingClientRect:()=>({width:900,right:900}),
  querySelector:selector=>selector===".ec-divider"?handle:inspector,
  querySelectorAll:selector=>selector==="details"?[lazyDetails]:[]
};
const output = {};
const document = {
  readyState:"loading",
  getElementById:id=>id==="workspace_view"?output:null,
  querySelectorAll:()=>inserted?[split]:[],
  addEventListener:(key,fn)=>{events[key]=fn;}
};
const context = {
  document,
  MutationObserver:class {
    constructor(callback){this.callback=callback;}
    observe(target,options){
      assert.strictEqual(target,output);
      assert.deepStrictEqual(JSON.parse(JSON.stringify(options)),{childList:true});
      callbacks.push(this.callback);
    }
  },
  localStorage:{getItem:()=>null,setItem:()=>{}},
  requestAnimationFrame:fn=>fn(),
  window:{Shiny:{setInputValue:(id,value)=>{shinyInputs++;sent.push([id,value]);}}}
};
vm.runInNewContext(fs.readFileSync(process.argv[2],"utf8"),context);
events.DOMContentLoaded();
assert.strictEqual(callbacks.length,1);
assert.strictEqual(split.dataset.ecBound,undefined);
// renderUI finishes later, rather than at the shiny:value notification.
inserted=true;callbacks[0]();
assert.strictEqual(split.dataset.ecBound,"1");
assert.strictEqual(handlers.keydown.length,1);
assert.deepStrictEqual(sent,[["embedding_comparison-run_details_open",false]]);
const beforeResize = shinyInputs;
handlers.keydown[0]({key:"ArrowLeft",preventDefault:()=>{}});
assert.deepStrictEqual(widths.at(-1),["--ec-inspector-width","420px"]);
assert.strictEqual(attrs["aria-valuenow"],"420");
assert.strictEqual(shinyInputs,beforeResize);
lazyDetails.open=true;lazyEvents.toggle();
assert.deepStrictEqual(sent.at(-1),["embedding_comparison-run_details_open",true]);
lazyDetails.open=false;lazyEvents.toggle();
assert.deepStrictEqual(sent.at(-1),["embedding_comparison-run_details_open",false]);
const afterToggle=shinyInputs;
callbacks[0]();
assert.strictEqual(handlers.keydown.length,1);
assert.strictEqual(shinyInputs,afterToggle);
console.log("Delayed workspace binds once; resize emits no inputs; lazy metadata reports opening/closing.");
