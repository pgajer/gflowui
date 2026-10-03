(function () {
  'use strict';
  const states = new Map();
  let applying = false;
  const array = x => x == null ? [] : Array.isArray(x) ? x : [x];
  function render(message) {
    const root = document.getElementById(message.container);
    if (!root) return;
    applying = true;
    try {
      for (const field of array(message.fields)) {
        const select = document.getElementById(field.id);
        if (!select || !select.selectize) continue;
        const row = document.getElementById(field.id + '_row');
        row.style.display = field.visible ? '' : 'none';
        row.setAttribute('aria-hidden', field.visible ? 'false' : 'true');
        const label = row.querySelector('label'); if (label) label.textContent = field.label;
        select.dataset.atlasField = field.key;
        const control = select.selectize;
        control.clear(true);control.clearOptions();
        control.addOption({value: '', label: 'Choose…', text: 'Choose…'});
        for (const option of array(field.options)) control.addOption({value: option.value, label: option.label, text: option.label});
        control.setValue(field.selected || '', true);control.refreshOptions(false);
      }
    } finally { applying = false; }
  }
  function init() {
    if (!window.Shiny || !window.jQuery) return;
    Shiny.addCustomMessageHandler('gflowuiAtlasNavigation', message => {
      states.set(message.container,message);render(message);
    });
    $(document).on('change.gflowuiAtlasNavigation','[data-gf-atlas-navigation] select',function () {
      if (applying) return;
      const root = this.closest('[data-gf-atlas-navigation]'), state = states.get(root.id);
      if (!state || !this.dataset.atlasField) return;
      const value = this.selectize ? this.selectize.getValue() : this.value;
      Shiny.setInputValue(state.input,{token:state.token,project:state.project,key:this.dataset.atlasField,value:value},{priority:'event'});
    });
    $(document).on('shiny:bound.gflowuiAtlasNavigation',event => {
      const root = event.target.closest && event.target.closest('[data-gf-atlas-navigation]');
      if (!root || event.target.tagName !== 'SELECT') return;
      const state = states.get(root.id);if (state) render(state);
      Shiny.setInputValue(root.dataset.gfAtlasNavigation.replace(/nav_choice$/, 'nav_mounted'),Date.now(),{priority:'event'});
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',init);else init();
})();
