const fs=require('fs'),vm=require('vm'),assert=require('assert');
const handlers={},custom={},calls=[],sent=[];
const plotHandlers={};
const plot={on:(k,f)=>plotHandlers[k]=f,isConnected:true,dataset:{},data:[],_fullLayout:{scene:{camera:{eye:{x:2,y:3,z:4}}}}};
const context={window:{},console,performance:{now:()=>1},CustomEvent:function(){},
  requestAnimationFrame:f=>f(),document:{addEventListener:(k,f)=>handlers[k]=f,
    getElementById:()=>({textContent:'[]'}),dispatchEvent:()=>{}},
  Shiny:{setInputValue:(k,v)=>sent.push([k,v]),addCustomMessageHandler:(k,f)=>custom[k]=f},
  Plotly:{update:async(p,d,l,i)=>calls.push({kind:'coordinates',d,l,i}),react:async(p,d,l)=>calls.push({kind:'full',d,l})},
  $:f=>f()};
vm.runInNewContext(fs.readFileSync('inst/app/www/scene-updates.js','utf8'),context);
const turn=()=>new Promise(resolve=>setImmediate(resolve));
(async()=>{
 context.window.gflowuiScene.mount(plot,{generation:1,revision:1,scope:'a',selection_seq:0,set_id:'a'});
 custom.gflowuiSceneUpdate({generation:1,revision:2,base_revision:1,kind:'coordinates',indices:0,coordinates:{x:3,y:4,z:5},scope:'a',selection_seq:0});await turn();
 assert.deepEqual(JSON.parse(JSON.stringify(calls[0].d)),{x:[[3]],y:[[4]],z:[[5]]});
 assert.equal(calls[0].l['scene.camera'].eye.x,2);
 handlers['gflowui:selection']({detail:{seq:2,scope:'a',start:0}});
 custom.gflowuiSceneUpdate({generation:1,revision:3,kind:'full',scope:'a',selection_seq:1});await turn();
 assert.equal(calls.length,1);assert.equal(sent.length,1); // Stale scene is skipped without redundant resync.
 custom.gflowuiSceneUpdate({generation:1,revision:4,base_revision:3,kind:'coordinates',scope:'a',selection_seq:2});
 custom.gflowuiSceneUpdate({generation:1,revision:5,base_revision:4,kind:'coordinates',scope:'a',selection_seq:2});await turn();
 assert.equal(sent.filter(x=>x[0]==='gflowui_scene_resync').length,1);
 plotHandlers.plotly_relayout({'scene.camera':{eye:{x:9,y:8,z:7}}});
 custom.gflowuiSceneUpdate({generation:1,revision:6,kind:'full',scope:'a',selection_seq:2,data:{type:'scatter3d',x:1,y:2,z:3},layout:{}});await turn();
 assert.equal(calls.length,2);assert.equal(calls[1].l.scene.camera.eye.x,9);assert.deepEqual(Array.from(calls[1].d[0].x),[1]);
 console.log('Scene update regression passed');
})().catch(e=>{console.error(e);process.exitCode=1;});
