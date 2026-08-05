(function () {
  "use strict";

  function cloneCamera(camera) {
    if (!camera) {
      return null;
    }
    try {
      return JSON.parse(JSON.stringify(camera));
    } catch (error) {
      return camera;
    }
  }

  function currentReferenceCamera() {
    var remembered = cloneCamera(window.__gflowuiReferenceCamera);
    if (remembered) {
      return remembered;
    }
    var graph = document.getElementById("reference_plot");
    try {
      if (
        graph &&
        graph._fullLayout &&
        graph._fullLayout.scene &&
        graph._fullLayout.scene.camera
      ) {
        return cloneCamera(graph._fullLayout.scene.camera);
      }
    } catch (error) {
      // Ignore a transient live-layout lookup failure during replacement.
    }
    return null;
  }

  function plotlyPayload(value) {
    if (value && value.x && value.x.layout) {
      return value.x;
    }
    if (value && value.layout) {
      return value;
    }
    return null;
  }

  function preserveReferenceCamera(event) {
    if (!event || event.name !== "reference_plot") {
      return;
    }
    var camera = currentReferenceCamera();
    var payload = plotlyPayload(event.value);
    if (!camera || !payload || !payload.layout) {
      return;
    }
    payload.layout.scene = payload.layout.scene || {};
    payload.layout.scene.camera = camera;
    window.__gflowuiReferenceCamera = cloneCamera(camera);
  }

  function bindReferenceCameraPreserver() {
    if (!window.jQuery || window.__gflowuiReferenceCameraPreserverBound) {
      return;
    }
    window.__gflowuiReferenceCameraPreserverBound = true;
    window.jQuery(document).on(
      "shiny:value.gflowuiReferenceCamera",
      preserveReferenceCamera
    );
  }

  bindReferenceCameraPreserver();
  document.addEventListener("DOMContentLoaded", bindReferenceCameraPreserver);
  document.addEventListener("shiny:connected", bindReferenceCameraPreserver);
  window.setTimeout(bindReferenceCameraPreserver, 0);
})();
