(function () {
  "use strict";
  function resizePlots(split) {
    if (!window.Plotly) return;
    split.querySelectorAll(".js-plotly-plot").forEach(function(p) {
      var box=p.getBoundingClientRect();
      if (box.width > 0 && box.height > 0) window.Plotly.Plots.resize(p);
    });
  }
  function bind() {
    document.querySelectorAll(".ec-workspace").forEach(function(split) {
      if (split.dataset.ecBound) return;
      split.dataset.ecBound = "1";
      var handle = split.querySelector(".ec-divider");
      var inspector = split.querySelector(".ec-inspector");
      var storage = "gflowui-embedding-inspector-width";
      function setWidth(w) {
        var max = Math.max(380, split.getBoundingClientRect().width - 320);
        w = Math.max(380, Math.min(max, w));
        split.style.setProperty("--ec-inspector-width", w + "px");
        handle.setAttribute("aria-valuemin","380");
        handle.setAttribute("aria-valuemax",String(max));
        handle.setAttribute("aria-valuenow",String(Math.round(w)));
        return w;
      }
      var initialWidth = inspector.getBoundingClientRect().width;
      try { var saved = Number(localStorage.getItem(storage)); if (saved > 0) initialWidth = saved; } catch(e) {}
      setWidth(initialWidth);
      var pending = false;
      function redraw() {
        if (pending) return;
        pending = true;
        requestAnimationFrame(function(){pending=false;resizePlots(split);});
      }
      handle.addEventListener("pointerdown",function(event) {
        event.preventDefault(); handle.setPointerCapture(event.pointerId);
        function move(e) {
          var box = split.getBoundingClientRect();
          setWidth(box.right-e.clientX);redraw();
        }
        function stop(e) {
          handle.removeEventListener("pointermove",move);handle.removeEventListener("pointerup",stop);
          handle.removeEventListener("pointercancel",stop);
          try {handle.releasePointerCapture(e.pointerId);localStorage.setItem(storage,String(inspector.getBoundingClientRect().width));}catch(error){}
          redraw();
        }
        handle.addEventListener("pointermove",move);handle.addEventListener("pointerup",stop);handle.addEventListener("pointercancel",stop);
      });
      handle.addEventListener("keydown",function(e) {
        if(e.key!=="ArrowLeft" && e.key!=="ArrowRight")return;
        e.preventDefault();var w=setWidth(inspector.getBoundingClientRect().width+(e.key==="ArrowLeft"?20:-20));
        try{localStorage.setItem(storage,String(w));}catch(error){} redraw();
      });
      split.querySelectorAll("details").forEach(function(d){d.addEventListener("toggle",redraw);});
      // Width, accordion and scroll state stay in this stable DOM shell.
      // No Shiny input is emitted by resizing or opening a section.
    });
  }
  document.addEventListener("click",function(event) {
    var button=event.target.closest(".ec-load-run");
    if(button && !button.disabled && window.Shiny) {
      window.Shiny.setInputValue(button.dataset.ecInput,{id:button.dataset.ecRun,nonce:Date.now()+Math.random()},{priority:"event"});
    }
    var heading=event.target.closest(".ec-sortable th");
    if(!heading || heading.cellIndex===0)return;
    var table=heading.closest("table"),body=table.tBodies[0],column=heading.cellIndex;
    var direction=heading.dataset.direction==="ascending"?-1:1;
    table.querySelectorAll("th[aria-sort]").forEach(function(h){h.removeAttribute("aria-sort");});
    heading.dataset.direction=direction===1?"ascending":"descending";
    heading.setAttribute("aria-sort",heading.dataset.direction);
    Array.from(body.rows).sort(function(a,b) {
      var x=a.cells[column].dataset.sort || "",y=b.cells[column].dataset.sort || "";
      if(!x && !y)return 0;if(!x)return 1;if(!y)return -1;
      var nx=Number(x),ny=Number(y);
      return direction*(Number.isFinite(nx)&&Number.isFinite(ny)?nx-ny:x.localeCompare(y));
    }).forEach(function(row){body.appendChild(row);});
  });
  document.addEventListener("keydown",function(e) {
    if((e.key==="Enter" || e.key===" ") && e.target.matches(".ec-sortable th")){e.preventDefault();e.target.click();}
  });
  var watchedOutput = null;
  function watchWorkspace() {
    var output = document.getElementById("workspace_view");
    if (output && output !== watchedOutput) {
      watchedOutput = output;
      // Shiny inserts renderUI content asynchronously, after shiny:value.
      // Observe only this stable output's direct children, not Plotly internals.
      new MutationObserver(bind).observe(output,{childList:true});
    }
    bind();
  }
  document.addEventListener("DOMContentLoaded",watchWorkspace);
  if (document.readyState !== "loading") watchWorkspace();
})();
