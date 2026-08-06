(function () {
  "use strict";

  var state = {
    payload: null,
    inFlightNonce: null,
    pendingIndex: null,
    nonceCounter: 0,
    previewCount: 0,
    commitPublishCount: 0
  };

  function slider() {
    return document.getElementById("basin_tree_event_range");
  }

  function eventAt(index) {
    var events = state.payload && state.payload.events;
    if (!Array.isArray(events) || index < 0 || index >= events.length) {
      return null;
    }
    return events[index];
  }

  function clampIndex(value) {
    var count = state.payload && Array.isArray(state.payload.events) ?
      state.payload.events.length : 0;
    var index = Math.round(Number(value));
    if (!Number.isFinite(index) || count < 1) {
      return 0;
    }
    return Math.max(0, Math.min(count - 1, index));
  }

  function relayoutThreshold(event) {
    var plot = document.getElementById(
      (state.payload && state.payload.plot_id) ||
        "basin_merge_tree_interactive_plot"
    );
    if (!plot || !event || !window.Plotly ||
        typeof window.Plotly.relayout !== "function") {
      return;
    }
    window.Plotly.relayout(plot, {
      "shapes[0].y0": event.height,
      "shapes[0].y1": event.height,
      "annotations[0].y": event.height,
      "annotations[0].text": "h = " + event.height_text
    });
  }

  function applyAscentFlowStyle(payload) {
    var plot;
    var traceIndex;
    var opacity;
    var width;
    if (!payload) {
      return;
    }
    plot = document.getElementById(payload.plot_id || "reference_plot");
    if (!plot || !Array.isArray(plot.data) || !window.Plotly ||
        typeof window.Plotly.restyle !== "function") {
      return;
    }
    traceIndex = plot.data.findIndex(function (trace) {
      return trace && trace.meta &&
        trace.meta.gflowui_layer === "canonical_ascent_flow";
    });
    if (traceIndex < 0) {
      return;
    }
    opacity = Math.max(0, Math.min(1, Number(payload.opacity)));
    width = Math.max(0.5, Math.min(8, Number(payload.width)));
    window.Plotly.restyle(plot, {
      "line.color": payload.color,
      "line.width": Number.isFinite(width) ? width : 2,
      opacity: Number.isFinite(opacity) ? opacity : 1
    }, [traceIndex]);
  }

  function setButtonState(index, count) {
    var previous = document.getElementById("basin_tree_previous_event");
    var next = document.getElementById("basin_tree_next_event");
    if (previous) {
      previous.disabled = index <= 0;
    }
    if (next) {
      next.disabled = index >= count - 1;
    }
  }

  function showPreview(index) {
    var input = slider();
    var event = eventAt(index);
    var status = document.getElementById("basin_tree_event_preview_status");
    if (!input || !event) {
      return;
    }
    state.previewCount += 1;
    input.dataset.previewCount = String(state.previewCount);
    input.setAttribute("aria-valuenow", String(index));
    input.setAttribute("aria-valuetext", event.aria_label);
    if (status) {
      status.textContent = "Preview — Event " + (index + 1) + " of " +
        state.payload.events.length + " — " + event.summary + " — h = " +
        event.height_text + ". " +
        (state.payload.link_graph ?
          "Release to apply to the graph." :
          "Release to apply to the tree.");
    }
    setButtonState(index, state.payload.events.length);
    relayoutThreshold(event);
  }

  function publishCommit(index) {
    var input = slider();
    if (!state.payload || !window.Shiny ||
        typeof window.Shiny.setInputValue !== "function") {
      return;
    }
    index = clampIndex(index);
    if (state.inFlightNonce !== null) {
      state.pendingIndex = index;
      return;
    }
    if (index === Number(state.payload.committed_index)) {
      applyPayload(state.payload);
      return;
    }
    state.nonceCounter += 1;
    state.inFlightNonce = "event-" + Date.now() + "-" + state.nonceCounter;
    state.commitPublishCount += 1;
    if (input) {
      input.dataset.commitPublishCount = String(state.commitPublishCount);
    }
    window.Shiny.setInputValue("basin_tree_event_commit", {
      context_token: state.payload.context_token,
      event_index: index,
      nonce: state.inFlightNonce
    }, { priority: "event" });
  }

  function applyPayload(payload) {
    var input;
    var committed;
    var event;
    var status;
    var pending;
    var contextChanged;

    if (!payload || !Array.isArray(payload.events) || !payload.events.length) {
      return;
    }
    contextChanged = state.payload &&
      state.payload.context_token !== payload.context_token;
    if (contextChanged) {
      state.inFlightNonce = null;
      state.pendingIndex = null;
    } else if (payload.ack_nonce &&
        payload.ack_nonce === state.inFlightNonce) {
      state.inFlightNonce = null;
    }
    state.payload = payload;
    committed = clampIndex(payload.committed_index);
    event = eventAt(committed);
    input = slider();
    if (input) {
      input.min = "0";
      input.max = String(payload.events.length - 1);
      input.step = "1";
      input.value = String(committed);
      input.disabled = payload.events.length === 1;
      input.setAttribute("aria-valuemin", "0");
      input.setAttribute("aria-valuemax", String(payload.events.length - 1));
      input.setAttribute("aria-valuenow", String(committed));
      input.dataset.contextToken = payload.context_token;
      input.dataset.previewCount = String(state.previewCount);
      input.dataset.commitPublishCount = String(state.commitPublishCount);
      input.dataset.serverCutComputeCount = String(
        payload.cut_compute_count || 0
      );
      input.dataset.serverStaticBuildCount = String(
        payload.static_build_count || 0
      );
      input.dataset.serverAcceptedCommitCount = String(
        payload.accepted_commit_count || 0
      );
      input.dataset.serverTreeRenderCount = String(
        payload.tree_render_count || 0
      );
      input.dataset.serverGraphOverlayComputeCount = String(
        payload.graph_overlay_compute_count || 0
      );
      input.dataset.lastStaticBuildElapsedMs = String(
        payload.last_static_build_elapsed_ms || 0
      );
      input.dataset.lastCutElapsedMs = String(
        payload.last_cut_elapsed_ms || 0
      );
      input.dataset.lastCutCacheHit = payload.last_cut_cache_hit ?
        "true" : "false";
      input.setAttribute("aria-valuetext", event.aria_label);
    }
    status = document.getElementById("basin_tree_event_preview_status");
    if (status) {
      status.textContent = "";
    }
    setButtonState(committed, payload.events.length);
    relayoutThreshold(event);

    if (state.inFlightNonce === null && state.pendingIndex !== null) {
      pending = state.pendingIndex;
      state.pendingIndex = null;
      if (pending !== committed) {
        publishCommit(pending);
      }
    }
  }

  document.addEventListener("input", function (browserEvent) {
    var target = browserEvent.target;
    if (!target || target.id !== "basin_tree_event_range") {
      return;
    }
    showPreview(clampIndex(target.value));
  });

  document.addEventListener("change", function (browserEvent) {
    var target = browserEvent.target;
    if (!target || target.id !== "basin_tree_event_range") {
      return;
    }
    publishCommit(clampIndex(target.value));
  });

  document.addEventListener("click", function (browserEvent) {
    var target = browserEvent.target;
    var input;
    var delta;
    var index;
    if (!target || (target.id !== "basin_tree_previous_event" &&
        target.id !== "basin_tree_next_event")) {
      return;
    }
    input = slider();
    if (!input || target.disabled) {
      return;
    }
    delta = target.id === "basin_tree_previous_event" ? -1 : 1;
    index = clampIndex(Number(input.value) + delta);
    input.value = String(index);
    showPreview(index);
    publishCommit(index);
  });

  function registerMessageHandler() {
    if (state.messageHandlerRegistered || !window.Shiny ||
        typeof window.Shiny.addCustomMessageHandler !== "function") {
      return;
    }
    window.Shiny.addCustomMessageHandler(
      "gflowui-basin-tree-event-domain",
      applyPayload
    );
    window.Shiny.addCustomMessageHandler(
      "gflowui-basin-ascent-flow-style",
      applyAscentFlowStyle
    );
    state.messageHandlerRegistered = true;
  }

  document.addEventListener("shiny:connected", function () {
    registerMessageHandler();
    if (state.payload) {
      applyPayload(state.payload);
    }
  });

  registerMessageHandler();
})();
