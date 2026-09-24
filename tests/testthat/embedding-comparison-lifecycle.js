const fs = require("fs"), vm = require("vm"), assert = require("assert");
const events = {}, callbacks = [], widths = [], attrs = {};
let inserted = false, shinyInputs = 0;
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
  querySelectorAll:()=>[]
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
  window:{Shiny:{setInputValue:()=>{shinyInputs++;}}}
};
vm.runInNewContext(fs.readFileSync(process.argv[2],"utf8"),context);
events.DOMContentLoaded();
assert.strictEqual(callbacks.length,1);
assert.strictEqual(split.dataset.ecBound,undefined);
// renderUI finishes later, rather than at the shiny:value notification.
inserted=true;callbacks[0]();
assert.strictEqual(split.dataset.ecBound,"1");
assert.strictEqual(handlers.keydown.length,1);
handlers.keydown[0]({key:"ArrowLeft",preventDefault:()=>{}});
assert.deepStrictEqual(widths.at(-1),["--ec-inspector-width","420px"]);
assert.strictEqual(attrs["aria-valuenow"],"420");
assert.strictEqual(shinyInputs,0);
callbacks[0]();
assert.strictEqual(handlers.keydown.length,1);
console.log("Delayed workspace insertion binds once; divider changes width without Shiny inputs.");
