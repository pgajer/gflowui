(function () {
  "use strict";

  var basinRecipeStorageKey = "gflowui-basin-analysis-recipe-v1";
  var basinRecipeHandlersBound = false;

  function publishRecipeEvent(status, recipe, requestId, message) {
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
      return;
    }
    window.Shiny.setInputValue(
      "basin_recipe_restore_event",
      {
        status: status,
        recipe: recipe || null,
        request_id: requestId || "",
        message: message || "",
        nonce: Date.now() + Math.random()
      },
      { priority: "event" }
    );
  }

  function bindBasinRecipeHandlers() {
    if (
      basinRecipeHandlersBound ||
      !window.Shiny ||
      typeof window.Shiny.addCustomMessageHandler !== "function"
    ) {
      return;
    }
    basinRecipeHandlersBound = true;
    window.Shiny.addCustomMessageHandler(
      "gflowui-basin-recipe-save",
      function (payload) {
        var requestId = payload && payload.request_id
          ? String(payload.request_id)
          : "";
        try {
          window.localStorage.setItem(
            basinRecipeStorageKey,
            JSON.stringify(payload.recipe)
          );
          publishRecipeEvent("saved", payload.recipe, requestId, "");
        } catch (error) {
          publishRecipeEvent(
            "storage_error",
            null,
            requestId,
            String(error && error.message ? error.message : error)
          );
        }
      }
    );
    window.Shiny.addCustomMessageHandler(
      "gflowui-basin-recipe-request",
      function (payload) {
        var requestId = payload && payload.request_id
          ? String(payload.request_id)
          : "";
        var stored;
        try {
          stored = window.localStorage.getItem(basinRecipeStorageKey);
          if (stored === null) {
            publishRecipeEvent("missing", null, requestId, "");
            return;
          }
          publishRecipeEvent(
            "available",
            JSON.parse(stored),
            requestId,
            ""
          );
        } catch (error) {
          publishRecipeEvent(
            "storage_error",
            null,
            requestId,
            String(error && error.message ? error.message : error)
          );
        }
      }
    );
  }

  function clamp(value, split) {
    var available = split
      ? Math.max(380, split.getBoundingClientRect().width - 420)
      : 1200;
    return Math.max(
      380,
      Math.min(1200, available, Math.round(value))
    );
  }

  function notify(width) {
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue(
        "basin_inspector_width",
        Math.round(width),
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
    var control = role === "selection" || role === "pin"
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

  function bindSplitter() {
    var split = document.getElementById("gf_reference_split");
    var handle = document.getElementById("gf_general_inspector_resize");
    var panel = document.getElementById("gf_basin_inspector");
    var inspector = document.getElementById("gf_general_inspector");
    if (!split || !handle || !inspector) {
      return;
    }

    if (panel) {
      split.classList.add("gf-general-inspector-open");
      var key = panel.dataset.storageKey ||
        "gflowui-general-inspector-width";
      if (split.dataset.gfInspectorStorageKey !== key) {
        split.dataset.gfInspectorStorageKey = key;
        try {
          var storedValue = window.localStorage.getItem(key);
          var stored = storedValue === null ? NaN : Number(storedValue);
          if (Number.isFinite(stored)) {
            split.style.setProperty(
              "--gf-general-inspector-width",
              clamp(stored, split) + "px"
            );
          }
        } catch (error) {
          // Local storage can be unavailable in hardened browser contexts.
        }
      }
    } else {
      split.classList.remove("gf-general-inspector-open");
      return;
    }

    var updateAccessibility = function (width) {
      var maximum = clamp(1200, split);
      handle.setAttribute("aria-valuemin", "380");
      handle.setAttribute("aria-valuemax", String(maximum));
      handle.setAttribute("aria-valuenow", String(Math.round(width)));
    };

    var setWidth = function (width) {
      var adjusted = clamp(width, split);
      split.style.setProperty(
        "--gf-general-inspector-width",
        adjusted + "px"
      );
      updateAccessibility(adjusted);
      return adjusted;
    };

    var save = function () {
      var width = setWidth(inspector.getBoundingClientRect().width);
      var storageKey = split.dataset.gfInspectorStorageKey ||
        "gflowui-general-inspector-width";
      try {
        window.localStorage.setItem(storageKey, String(width));
      } catch (error) {
        // Persistence is best-effort; the Shiny session still retains width.
      }
      notify(width);
    };

    setWidth(inspector.getBoundingClientRect().width || 620);

    if (split.dataset.gfSplitterBound === "1") {
      return;
    }
    split.dataset.gfSplitterBound = "1";

    var dragging = false;
    var resizeFromPointer = function (event) {
      var bounds = split.getBoundingClientRect();
      setWidth(bounds.right - event.clientX);
    };

    handle.addEventListener("pointerdown", function (event) {
      if (!split.classList.contains("gf-general-inspector-open")) {
        return;
      }
      dragging = true;
      handle.setPointerCapture(event.pointerId);
      document.body.classList.add("gf-general-inspector-resizing");
      resizeFromPointer(event);
      event.preventDefault();
    });
    handle.addEventListener("pointermove", function (event) {
      if (dragging) {
        resizeFromPointer(event);
      }
    });
    handle.addEventListener("pointerup", function (event) {
      if (!dragging) {
        return;
      }
      dragging = false;
      if (handle.hasPointerCapture(event.pointerId)) {
        handle.releasePointerCapture(event.pointerId);
      }
      document.body.classList.remove("gf-general-inspector-resizing");
      save();
    });
    handle.addEventListener("pointercancel", function () {
      dragging = false;
      document.body.classList.remove("gf-general-inspector-resizing");
    });
    handle.addEventListener("keydown", function (event) {
      var width = inspector.getBoundingClientRect().width;
      var step = event.shiftKey ? 64 : 24;
      if (event.key === "ArrowLeft") {
        setWidth(width + step);
      } else if (event.key === "ArrowRight") {
        setWidth(width - step);
      } else if (event.key === "Home") {
        setWidth(380);
      } else if (event.key === "End") {
        setWidth(1200);
      } else {
        return;
      }
      event.preventDefault();
      save();
    });
    handle.addEventListener("dblclick", function () {
      setWidth(620);
      save();
    });
    window.addEventListener("resize", function () {
      if (split.classList.contains("gf-general-inspector-open")) {
        setWidth(inspector.getBoundingClientRect().width);
      }
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    bindSplitter();
    bindBasinRecipeHandlers();
  });
  document.addEventListener("shiny:connected", bindBasinRecipeHandlers);
  if (window.jQuery) {
    window.jQuery(document).on(
      "shiny:connected",
      bindBasinRecipeHandlers
    );
  }
  window.setTimeout(bindBasinRecipeHandlers, 0);
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
  new MutationObserver(bindSplitter).observe(document.documentElement, {
    childList: true,
    subtree: true
  });
})();
