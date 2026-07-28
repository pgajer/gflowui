(function () {
  "use strict";

  var densityControlIds = {
    occupation_density_low_color: "low",
    occupation_density_mid_color: "midpoint",
    occupation_density_high_color: "high",
    occupation_density_low_alpha: "low_alpha",
    occupation_density_mid_alpha: "midpoint_alpha",
    occupation_density_high_alpha: "high_alpha"
  };
  var densityState = {};
  var nonce = 0;
  var suppressRememberUntil = 0;

  function rememberDensityControls() {
    Object.keys(densityControlIds).forEach(function (inputId) {
      var stateKey = densityControlIds[inputId];
      var input = document.getElementById(inputId);
      var checked = document.querySelector(
        'input[name="' + inputId + '"]:checked'
      );

      if (checked) {
        densityState[stateKey] = checked.value;
      } else if (input) {
        densityState[stateKey] = input.value;
      }
    });
  }

  function publishDensityControls() {
    var payload;

    rememberDensityControls();
    if (
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== "function" ||
      Object.keys(densityState).length === 0
    ) {
      return;
    }
    payload = Object.assign({}, densityState, { nonce: ++nonce });
    window.Shiny.setInputValue(
      "density_display_client_snapshot",
      payload,
      { priority: "event" }
    );
  }

  function isDensityControl(target) {
    return Boolean(
      target &&
      (
        Object.prototype.hasOwnProperty.call(
          densityControlIds,
          target.id
        ) ||
        Object.prototype.hasOwnProperty.call(
          densityControlIds,
          target.name
        )
      )
    );
  }

  function isWorkflowInteraction(target) {
    return Boolean(target && target.closest && target.closest("#workflow_controls"));
  }

  function restoreDensityControls() {
    Object.keys(densityControlIds).forEach(function (inputId) {
      var stateKey = densityControlIds[inputId];
      var desired = densityState[stateKey];
      var input = document.getElementById(inputId);
      var checked;
      var slider;

      if (typeof desired === "undefined") {
        return;
      }
      checked = document.querySelector(
        'input[name="' + inputId + '"][value="' + desired + '"]'
      );
      if (checked && !checked.checked) {
        checked.checked = true;
        checked.dispatchEvent(new Event("change", { bubbles: true }));
        return;
      }
      if (!input || String(input.value) === String(desired)) {
        return;
      }
      slider = window.jQuery ?
        window.jQuery(input).data("ionRangeSlider") :
        null;
      if (slider && typeof slider.update === "function") {
        slider.update({ from: Number(desired) });
      } else {
        input.value = desired;
      }
      input.dispatchEvent(new Event("change", { bubbles: true }));
    });
  }

  function scheduleDensityRestore() {
    suppressRememberUntil = Date.now() + 600;
    restoreDensityControls();
    window.requestAnimationFrame(restoreDensityControls);
    window.setTimeout(restoreDensityControls, 50);
    window.setTimeout(restoreDensityControls, 180);
    window.setTimeout(restoreDensityControls, 400);
  }

  function containsDensityControls(node) {
    if (!node || node.nodeType !== 1) {
      return false;
    }
    return Boolean(
      node.id === "workflow_controls" ||
      node.id === "occupation_density_low_alpha" ||
      (
        node.querySelector &&
        node.querySelector("#occupation_density_low_alpha")
      )
    );
  }

  document.addEventListener("input", function (event) {
    if (
      Date.now() >= suppressRememberUntil &&
      isDensityControl(event.target)
    ) {
      rememberDensityControls();
    }
  }, true);

  document.addEventListener("change", function (event) {
    if (
      Date.now() >= suppressRememberUntil &&
      isDensityControl(event.target)
    ) {
      rememberDensityControls();
    }
  }, true);

  document.addEventListener("pointerdown", function (event) {
    if (isWorkflowInteraction(event.target)) {
      publishDensityControls();
    }
  }, true);

  document.addEventListener("keydown", function (event) {
    if (
      isWorkflowInteraction(event.target) &&
      (event.key === "Enter" || event.key === " ")
    ) {
      publishDensityControls();
    }
  }, true);

  new MutationObserver(function (mutations) {
    var shouldRestore = mutations.some(function (mutation) {
      return Array.prototype.some.call(
        mutation.addedNodes,
        containsDensityControls
      );
    });

    if (shouldRestore && Object.keys(densityState).length > 0) {
      scheduleDensityRestore();
    }
  }).observe(document.documentElement, {
    childList: true,
    subtree: true
  });
}());
