(function () {
  "use strict";

  var timer = null;

  function publishLevel(input) {
    var maximum;
    var raw;
    var levelIndex;

    if (
      !input ||
      input.id !== "basin_tree_level_range" ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== "function"
    ) {
      return;
    }
    maximum = Number(input.max);
    raw = Number(input.value);
    if (!Number.isFinite(maximum) || !Number.isFinite(raw)) {
      return;
    }
    levelIndex = Math.max(0, Math.min(maximum, maximum - raw));
    window.Shiny.setInputValue(
      "basin_tree_level_index",
      levelIndex,
      { priority: "event" }
    );
  }

  document.addEventListener("input", function (event) {
    var target = event.target;
    if (!target || target.id !== "basin_tree_level_range") {
      return;
    }
    window.clearTimeout(timer);
    timer = window.setTimeout(function () {
      publishLevel(target);
    }, 60);
  });

  document.addEventListener("change", function (event) {
    var target = event.target;
    if (!target || target.id !== "basin_tree_level_range") {
      return;
    }
    window.clearTimeout(timer);
    publishLevel(target);
  });
})();
