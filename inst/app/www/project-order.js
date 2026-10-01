(function () {
  'use strict';
  let dragging = null;
  function rows(panel) { return Array.from(panel.querySelectorAll('.gf-project-order-row')); }
  function changed(panel, message) {
    const items = rows(panel);
    items.forEach((row, i) => {
      row.querySelector('[data-project-action="up"]').disabled = i === 0;
      row.querySelector('[data-project-action="down"]').disabled = i === items.length - 1;
    });
    panel.querySelector('.gf-project-order-status').textContent = message + ' Choose Save order to keep changes.';
  }
  document.addEventListener('click', function (event) {
    const button = event.target.closest('[data-project-action]');
    const panel = document.getElementById('gf_project_manager');
    if (!button || !panel || !button.closest('#shiny-modal')) return;
    const action = button.dataset.projectAction;
    const row = button.closest('.gf-project-order-row');
    if (action === 'up' && row.previousElementSibling) {
      row.parentNode.insertBefore(row, row.previousElementSibling);
      changed(panel, 'Project moved up.');
      row.querySelector('[data-project-action="' + (row.previousElementSibling ? 'up' : 'down') + '"]').focus();
    } else if (action === 'down' && row.nextElementSibling) {
      row.parentNode.insertBefore(row.nextElementSibling, row);
      changed(panel, 'Project moved down.');
      row.querySelector('[data-project-action="' + (row.nextElementSibling ? 'down' : 'up') + '"]').focus();
    } else if (action === 'alphabetical') {
      const list = panel.querySelector('.gf-project-order-list');
      rows(panel).sort((a, b) => a.querySelector('.gf-project-order-name').firstChild.textContent
        .localeCompare(b.querySelector('.gf-project-order-name').firstChild.textContent, undefined, {sensitivity: 'base'}))
        .forEach(item => list.appendChild(item));
      changed(panel, 'Projects sorted alphabetically.');
    } else if ((action === 'save' || action === 'open') && window.Shiny) {
      window.Shiny.setInputValue('project_manager_action', {
        action: action, token: panel.dataset.projectToken,
        ids: rows(panel).map(item => item.dataset.projectId), id: row ? row.dataset.projectId : null
      }, {priority: 'event'});
    }
  });
  function clearDrag() {
    if (!dragging) return;
    dragging.row.classList.remove('gf-project-dragging');
    if (dragging.target) dragging.target.classList.remove('gf-project-drop-target');
    if (dragging.handle.hasPointerCapture(dragging.pointerId)) dragging.handle.releasePointerCapture(dragging.pointerId);
    dragging = null;
  }
  document.addEventListener('pointerdown', function (event) {
    const handle = event.target.closest('.gf-project-drag');
    if (!handle || event.button !== 0) return;
    event.preventDefault();
    dragging = {row: handle.closest('.gf-project-order-row'), handle: handle,
      pointerId: event.pointerId, startY: event.clientY, active: false, target: null};
    handle.setPointerCapture(event.pointerId);
  });
  document.addEventListener('pointermove', function (event) {
    if (!dragging || event.pointerId !== dragging.pointerId) return;
    if (Math.abs(event.clientY - dragging.startY) > 4) dragging.active = true;
    if (!dragging.active) return;
    dragging.row.classList.add('gf-project-dragging');
    const list = dragging.row.parentNode;
    const bounds = list.getBoundingClientRect();
    if (event.clientY < bounds.top + 24) list.scrollTop -= 12;
    if (event.clientY > bounds.bottom - 24) list.scrollTop += 12;
    const under = document.elementFromPoint(event.clientX, event.clientY);
    const row = under && under.closest('.gf-project-order-row');
    if (dragging.target) dragging.target.classList.remove('gf-project-drop-target');
    dragging.target = row && row !== dragging.row && row.parentNode === list ? row : null;
    if (dragging.target) {
      const rect = row.getBoundingClientRect();
      dragging.after = event.clientY >= rect.top + rect.height / 2;
      row.classList.add('gf-project-drop-target');
    }
  });
  document.addEventListener('pointerup', function (event) {
    if (!dragging || event.pointerId !== dragging.pointerId) return;
    if (dragging.active && dragging.target) {
      const row = dragging.target;
      row.parentNode.insertBefore(dragging.row, dragging.after ? row.nextElementSibling : row);
      changed(document.getElementById('gf_project_manager'), 'Project moved.');
    }
    clearDrag();
  });
  document.addEventListener('pointercancel', clearDrag);
}());
