(function () {
  'use strict';
  let plot=null, generation=0, revision=0, chain=Promise.resolve(), currentIntent=null, resyncPending=false, timedIntent=null, sceneCamera=null;
  const array=x=>Array.isArray(x)?x:x==null?[]:[x];
  document.addEventListener('gflowui:selection',e=>{currentIntent=e.detail;});
  function finished(m,kind,start) {
    if(!plot||generation!==m.generation)return;
    plot.dataset.sceneRevision=String(m.revision);
    plot.dataset.sceneSet=m.set_id;
    plot.dataset.sceneSelection=String(m.selection_seq);
    plot.dataset.sceneUpdate=kind;
    const camera=plot._fullLayout&&plot._fullLayout.scene&&plot._fullLayout.scene.camera;
    plot.dataset.sceneCamera=JSON.stringify(camera||null);
    plot.dataset.sceneVertices=String(new Set((plot.data||[]).flatMap(t=>
      t.type==='scatter3d'&&String(t.mode).includes('markers')&&t.visible!=='legendonly'&&t.visible!==false ? array(t.customdata).filter(v=>Number.isFinite(v)) : [])).size);
    requestAnimationFrame(()=>requestAnimationFrame(()=>{
      const intentKey=m.scope+'|'+m.selection_seq;
      const latency=currentIntent&&currentIntent.scope===m.scope&&currentIntent.seq===m.selection_seq&&timedIntent!==intentKey ? performance.now()-currentIntent.start : null;
      if(latency!==null)timedIntent=intentKey;
      const detail={generation:generation,revision:m.revision,selection_seq:m.selection_seq,set_id:m.set_id,kind:kind,draw_ms:performance.now()-start,selection_ms:latency};
      // Compact DOM evidence makes timings inspectable without hidden app state.
      const evidence=document.getElementById('gflowui_scene_timing');
      if(evidence){let rows=JSON.parse(evidence.textContent||'[]');rows.push(detail);evidence.textContent=JSON.stringify(rows.slice(-200));}
      document.dispatchEvent(new CustomEvent('gflowui:scene-ready',{detail:detail}));
    }));
  }
  window.gflowuiScene={mount:function(el,m){
    plot=el;generation=m.generation;revision=m.revision;resyncPending=false;
    sceneCamera=null;
    if(el.on)el.on('plotly_relayout',function(ev){
      if(ev&&ev['scene.camera']){sceneCamera=JSON.parse(JSON.stringify(ev['scene.camera']));el.dataset.sceneCamera=JSON.stringify(sceneCamera);}
    });
    if(!document.getElementById('gflowui_scene_timing')){const node=document.createElement('script');node.type='application/json';node.id='gflowui_scene_timing';document.body.appendChild(node);}
    Shiny.setInputValue('gflowui_scene_mounted',m,{priority:'event'});
    finished(m,'initial',performance.now());
  }};
  $(function(){Shiny.addCustomMessageHandler('gflowuiSceneUpdate',function(m){
    // Serialize Plotly promises. Coordinates carry a base revision: never apply
    // them to the wrong trace structure after a full update or canvas remount.
    chain=chain.catch(()=>{}).then(async function(){
      if(!plot||!plot.isConnected||m.generation!==generation||m.revision<=revision)return;
      if(currentIntent&&m.scope===currentIntent.scope&&m.selection_seq<currentIntent.seq) {
        return;
      }
      if(m.kind==='coordinates'&&m.base_revision!==revision){
        if(!resyncPending){resyncPending=true;Shiny.setInputValue('gflowui_scene_resync',{generation:generation,revision:m.revision},{priority:'event'});}return;
      }
      if(m.kind==='full')resyncPending=false;
      const target=plot,start=performance.now();
      const camera=sceneCamera || window.__gflowuiReferenceCamera || (target._fullLayout&&target._fullLayout.scene&&target._fullLayout.scene.camera);
      try {
        if(m.kind==='coordinates') {
          const coords=array(m.coordinates),indices=array(m.indices);
          if(indices.length)await Plotly.update(target,{x:coords.map(t=>array(t.x)),y:coords.map(t=>array(t.y)),z:coords.map(t=>array(t.z))},camera?{'scene.camera':JSON.parse(JSON.stringify(camera))}:{},indices);
        } else if(m.kind==='full') {
          const layout=m.layout||{};
          if(camera){layout.scene=layout.scene||{};layout.scene.camera=JSON.parse(JSON.stringify(camera));}
          const data=array(m.data).map(t=>{['x','y','z'].forEach(k=>{if(k in t)t[k]=array(t[k]);});return t;});
          await Plotly.react(target,data,layout,m.config);
        }
        if(target!==plot||m.generation!==generation)return;
        revision=m.revision;finished(m,m.kind,start);
      } catch(error) {
        console.warn('gflowui scene update failed; requesting a complete scene.',error);
        Shiny.setInputValue('gflowui_scene_resync',{generation:generation,revision:m.revision},{priority:'event'});
      }
    });
  });});
})();
