// Resizing the two linked outputs does not recreate either widget or its camera.
(function () {
  function resize() {
    if (!window.Plotly) return;
    ['reference_plot', 'source_datasets-plot'].forEach(function (id) {
      var el = document.getElementById(id);
      if (el && el.data && el.clientWidth && el.clientHeight) window.Plotly.Plots.resize(el);
    });
  }
  // The sidebar can change available width without a window resize.
  $(document).on('shiny:connected', function () {
    var stage = document.querySelector('.gf-viewer-stage');
    if (!stage || !window.ResizeObserver || stage.gfLinkedResizeObserver) return;
    var lastWidth;
    stage.gfLinkedResizeObserver = new window.ResizeObserver(function (entries) {
      var width = entries[0].contentRect.width;
      if (width === lastWidth) return;
      lastWidth = width;
      window.requestAnimationFrame(resize);
    });
    stage.gfLinkedResizeObserver.observe(stage);
  });
  $(document).on('shiny:inputchanged', function (event) {
    if (event.name === 'source_datasets-show') setTimeout(resize, 180);
  });
})();

window.gflowuiWithinMount = function (el, namespace) {
  // A reactive redraw reuses the element: replace our handlers rather than
  // accumulating duplicate click toggles.
  Object.keys(el.gfWithinHandlers || {}).forEach(function (ev) {
    el.removeListener(ev, el.gfWithinHandlers[ev]);
  });
  el.gfWithinHandlers = {};
  ['plotly_click', 'plotly_selected'].forEach(function (ev) {
    var handler = function (data) {
      if (!data || !Array.isArray(data.points)) return;
      Shiny.setInputValue(namespace + ev + '-within_dcst',
        JSON.stringify(data.points.map(function (p) { return {key:p.customdata}; })),
        {priority:'event'});
    };
    el.gfWithinHandlers[ev] = handler;
    el.on(ev, handler);
  });
};
