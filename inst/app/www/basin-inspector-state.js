(function () {
  "use strict";

  function clamp(value) {
    return Math.max(230, Math.min(900, Math.round(value)));
  }

  function notify(height) {
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue(
        "basin_inspector_height",
        clamp(height),
        { priority: "event" }
      );
    }
  }

  function notifyRowChange(target) {
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
      return;
    }
    var owner = target.closest("[data-gf-basin-role]");
    if (!owner) {
      return;
    }
    var key = owner.dataset.gfBasinKey || "";
    var role = owner.dataset.gfBasinRole || "";
    var control = role === "selection"
      ? owner.querySelector('input[type="checkbox"]')
      : owner.querySelector("select");
    if (!control) {
      return;
    }
    if (!key || !role) {
      return;
    }
    window.Shiny.setInputValue(
      "basin_inspector_row_event",
      {
        key: key,
        role: role,
        checked: control.type === "checkbox" ? control.checked : null,
        value: control.value || "",
        nonce: Date.now() + Math.random()
      },
      { priority: "event" }
    );
  }

  function bind() {
    var panel = document.getElementById("gf_basin_inspector");
    if (!panel || panel.dataset.gfInspectorBound === "1") {
      return;
    }
    panel.dataset.gfInspectorBound = "1";
    var key = panel.dataset.storageKey || "gflowui-basin-inspector-height";
    try {
      var stored = Number(window.localStorage.getItem(key));
      if (Number.isFinite(stored)) {
        panel.style.height = clamp(stored) + "px";
      }
    } catch (error) {
      // Local storage can be unavailable in hardened browser contexts.
    }
    var save = function () {
      if (panel.classList.contains("gf-basin-inspector-maximized")) {
        return;
      }
      var height = clamp(panel.getBoundingClientRect().height);
      try {
        window.localStorage.setItem(key, String(height));
      } catch (error) {
        // Persistence is best-effort; the Shiny session still retains height.
      }
      notify(height);
    };
    panel.addEventListener("mouseup", save);
    panel.addEventListener("touchend", save);
  }

  document.addEventListener("DOMContentLoaded", bind);
  document.addEventListener("change", function (event) {
    var target = event.target;
    if (
      target &&
      target.closest &&
      target.closest("[data-gf-basin-role]")
    ) {
      notifyRowChange(target);
    }
  });
  new MutationObserver(bind).observe(document.documentElement, {
    childList: true,
    subtree: true
  });
})();
