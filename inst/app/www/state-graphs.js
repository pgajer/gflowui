(function(){
  function resize(){if(!window.Plotly)return;['reference_plot','state_graphs-plot','source_datasets-plot'].forEach(function(id){var el=document.getElementById(id);if(el&&el.data&&el.clientWidth)Plotly.Plots.resize(el);});}
  $(document).on('shiny:connected',function(){Shiny.addCustomMessageHandler('gflowuiStateMode',function(d){
    document.body.dataset.stateMode=d.enabled?d.mode:'samples';setTimeout(resize,100);
  });});
})();
window.gflowuiStateMount=function(el,d){
  if(el.gfStateCleanup)el.gfStateCleanup();
  el.dataset.stateIds=JSON.stringify(d.nodes||[]);el.dataset.edgeCount=d.edges;el.dataset.visibleMembers=d.visible_members;el.dataset.graphIdentity=d.graph;el.dataset.layoutIdentity=d.layout;
  var down=null,moved=false,pending=null,release=0;
  function start(e){down=[e.clientX,e.clientY];moved=false;pending=null;release=0;}
  function move(e){if(down&&Math.hypot(e.clientX-down[0],e.clientY-down[1])>5)moved=true;}
  function commit(){if(!moved&&pending)Shiny.setInputValue(d.input,{key:pending.key,shift:pending.shift,graph:d.graph,nonce:Date.now()},{priority:'event'});pending=null;}
  function end(){release=Date.now();down=null;setTimeout(commit,40);}
  function click(e){if(!e||!e.points||!e.points.length||moved)return;var p=e.points[0];if(typeof p.customdata!=='string')return;
    pending={key:p.customdata,shift:!!(e.event&&e.event.shiftKey)};if(!down&&release)setTimeout(commit,0);
  }
  function camera(e){if(e&&e['scene.camera'])Shiny.setInputValue(d.camera,{layout:d.layout,camera:e['scene.camera']},{priority:'event'});}
  el.addEventListener('pointerdown',start,true);window.addEventListener('pointermove',move,true);window.addEventListener('pointerup',end,true);
  el.on('plotly_click',click);el.on('plotly_relayout',camera);
  el.gfStateCleanup=function(){el.removeEventListener('pointerdown',start,true);window.removeEventListener('pointermove',move,true);window.removeEventListener('pointerup',end,true);el.removeListener('plotly_click',click);el.removeListener('plotly_relayout',camera);};
};
