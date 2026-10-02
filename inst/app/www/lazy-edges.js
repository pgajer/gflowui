(function () {
  'use strict';
  const arr=x=>Array.isArray(x)?x:x==null?[]:[x];
  const key=t=>t&&t.meta&&t.meta.gflowui_edges&&t.meta.gflowui_edges.key;
  // Base vertex traces precede endpoint/label traces. First occurrence wins,
  // so label offsets cannot move the endpoints of a graph edge.
  function points(data) {
    const map=new Map();
    data.forEach(t=>{
      if(!t.meta||t.meta.gflowui_vertices!==true)return;
      const ids=arr(t.customdata),x=arr(t.x),y=arr(t.y),z=arr(t.z);
      ids.forEach((id,i)=>{if(Number.isFinite(id)&&!map.has(id)&&[x[i],y[i],z[i]].every(Number.isFinite))map.set(id,[x[i],y[i],z[i]]);});
    });
    return map;
  }
  function geometry(edges,map) {
    const x=[],y=[],z=[];
    edges.a.forEach((a,i)=>{
      const u=map.get(a),v=map.get(edges.b[i]);if(!u||!v)return;
      x.push(u[0],v[0],null);y.push(u[1],v[1],null);z.push(u[2],v[2],null);
    });
    return {x:x.length?x:[null],y:y.length?y:[null],z:z.length?z:[null]};
  }
  function create(get,enqueue) {
    let generation=0,requestSeq=0,bytes=0;
    const cache=new Map(),shown=new Map(),pending=new Map();
    const empty=map=>{const p=map.values().next().value||[null,null,null];return {x:[p[0]],y:[p[1]],z:[p[2]]};};
    function diagnostics() {
      const c=get();if(!c.plot)return;
      c.plot.dataset.edgeRequestCount=String(requestSeq);
      c.plot.dataset.edgeLayers=JSON.stringify((c.plot.data||[]).filter(key).map(t=>({key:key(t),name:t.name,visible:t.visible===true,cached:cache.has(key(t)),segments:Math.floor(arr(t.x).filter(Number.isFinite).length/2)})));
    }
    function fail(key,message) {
      shown.set(key,false);get().plot.dataset.edgeError=message;
      if(Shiny.notifications)Shiny.notifications.show({id:"gflowui-edge-error",html:message,type:"error",duration:10});
    }
    function prepare(data) {
      const map=points(data);
      data.forEach(t=>{
        const k=key(t);if(!k)return;
        if(!shown.has(k))shown.set(k,t.visible===true);
        t.visible=shown.get(k)?true:'legendonly';
        Object.assign(t,shown.get(k)&&cache.has(k)?geometry(cache.get(k),map):empty(map));
      });
      return data;
    }
    function after() {
      const c=get();if(!c.plot)return;
      const active=new Set((c.plot.data||[]).map(key).filter(Boolean));
      for(const k of pending.keys())if(!active.has(k))pending.delete(k);
      (c.plot.data||[]).forEach(t=>{
        const k=key(t);if(!k||!shown.get(k)||cache.has(k)||pending.has(k))return;
        const id=++requestSeq;pending.set(k,id);
        Shiny.setInputValue('gflowui_edge_request',{generation:c.generation,key:k,request_id:id},{priority:'event'});
      });
      diagnostics();
    }
    async function redraw() {
      const c=get();if(!c.plot||!c.plot.isConnected)return;
      const data=prepare(c.plot.data.map(t=>Object.assign({},t))),indices=[];
      data.forEach((t,i)=>{if(key(t))indices.push(i);});
      if(indices.length)await Plotly.update(c.plot,{
        x:indices.map(i=>data[i].x),y:indices.map(i=>data[i].y),z:indices.map(i=>data[i].z),
        visible:indices.map(i=>data[i].visible)
      },c.camera?{'scene.camera':JSON.parse(JSON.stringify(c.camera))}:{},indices);
      after();
    }
    function mount(el,gen) {
      if(generation!==gen){cache.clear();shown.clear();pending.clear();bytes=0;requestSeq=0;generation=gen;}
      (el.data||[]).forEach(t=>{if(key(t)&&!shown.has(key(t)))shown.set(key(t),t.visible===true);});
      if(el.on){
        if(el.__gfEdgeClick)el.removeListener('plotly_legendclick',el.__gfEdgeClick);
        if(el.__gfEdgeDouble)el.removeListener('plotly_legenddoubleclick',el.__gfEdgeDouble);
        if(el.__gfEdgeRestyle)el.removeListener('plotly_restyle',el.__gfEdgeRestyle);
        el.__gfEdgeClick=function(ev){
          const k=key(el.data[ev.curveNumber]);if(!k)return;
          shown.set(k,!shown.get(k));enqueue(redraw);return false;
        };
        el.__gfEdgeDouble=function(ev){if(key(el.data[ev.curveNumber]))return false;};
        el.__gfEdgeRestyle=function(ev){
          if(!ev||!ev[0]||!('visible' in ev[0]))return;
          let changed=false;
          (el.data||[]).forEach(t=>{const k=key(t);if(k&&shown.get(k)!==(t.visible===true)){shown.set(k,t.visible===true);changed=true;}});
          if(changed)enqueue(redraw);
        };
        el.on('plotly_legendclick',el.__gfEdgeClick);el.on('plotly_legenddoubleclick',el.__gfEdgeDouble);el.on('plotly_restyle',el.__gfEdgeRestyle);
      }
      enqueue(redraw);
    }
    function patch(data,indices,coords) {
      const future=data.map(t=>Object.assign({},t));
      indices.forEach((index,i)=>Object.assign(future[index],coords[i]));
      prepare(future);
      future.forEach((t,i)=>{
        if(!key(t))return;
        let pos=indices.indexOf(i);if(pos<0){pos=indices.length;indices.push(i);}
        coords[pos]={x:t.x,y:t.y,z:t.z};
      });
    }
    function receive(m) {
      enqueue(async function(){
        const c=get();if(m.generation!==c.generation||pending.get(m.key)!==m.request_id)return;
        pending.delete(m.key);
        if(m.error){fail(m.key,m.error);await redraw();return;}
        const a=arr(m.a),b=arr(m.b),size=(a.length+b.length)*8;
        if(a.length!==b.length||size>96*1024*1024){fail(m.key,'Edge layer exceeds the browser cache limit or has invalid endpoint pairs.');await redraw();return;}
        const active=new Set((c.plot.data||[]).filter(t=>shown.get(key(t))).map(key));
        for(const [k,v] of cache){
          if(bytes+size<=96*1024*1024)break;
          if(!active.has(k)){bytes-=v.bytes;cache.delete(k);}
        }
        if(bytes+size>96*1024*1024){fail(m.key,'Visible edge layers exceed the browser cache limit; hide another edge layer first.');await redraw();return;}
        cache.set(m.key,{a:a,b:b,bytes:size});bytes+=size;
        await redraw();
      });
    }
    return {mount:mount,prepare:prepare,patch:patch,after:after,receive:receive};
  }
  window.gflowuiLazyEdges={create:create,points:points,geometry:geometry};
})();
