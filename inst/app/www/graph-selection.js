(function () {
  'use strict';
  let seq=0, intentSeq=0, seen=new WeakSet();
  const root=()=>document.getElementById('gf_grouped_selectors');
  function select(id,value) {
    const r=root(); if(!r || !r.querySelector('#'+CSS.escape(id)))return;
    intentSeq=++seq;
    Shiny.setInputValue('graph_selector_intent',{scope:r.dataset.scope,seq:seq,id:id,value:String(value)},{priority:'event'});
    document.dispatchEvent(new CustomEvent('gflowui:selection',{detail:{seq:seq,scope:r.dataset.scope,id:id,value:value,start:performance.now()}}));
  }
  window.gflowuiGraphSelection={select:select};
  // Commit the option chosen by a person, never an updateSelectInput echo.
  function optionSelection(option) {
    const holder=option&&option.closest('.shiny-input-container');
    const s=holder&&holder.querySelector('select');
    if(s && s.closest('#gf_grouped_selectors'))select(s.id,option.dataset.value);
  }
  document.addEventListener('pointerdown',function(e){
    optionSelection(e.target.closest('.selectize-dropdown [data-selectable][data-value]'));
  },true);
  document.addEventListener('keydown',function(e){
    if(e.key!=='Enter')return;
    const holder=e.target.closest('.shiny-input-container');
    optionSelection(holder&&holder.querySelector('.selectize-dropdown .active[data-selectable][data-value]'));
  },true);
  document.addEventListener('change',function(e){
    const s=e.target;
    if(e.isTrusted && s.matches('select') && !s.selectize && s.closest('#gf_grouped_selectors'))select(s.id,s.value);
  },true);
  $(function(){
    Shiny.addCustomMessageHandler('gflowuiSelectorState',function(m){
      const r=root();if(!r||r.dataset.scope!==m.scope||m.seq<intentSeq)return;
      (m.fields||[]).forEach(f=>{const row=document.getElementById(f.input_id+'_row');if(row){
        const s=row.querySelector('select'),control=s&&s.selectize;
        if(control){
          const opts=Array.isArray(f.options)?f.options:[f.options];
          const signature=JSON.stringify(opts);
          if(s.dataset.choiceSignature!==signature){control.clearOptions(true);control.addOption(opts);s.dataset.choiceSignature=signature;}
          if(control.getValue()!==String(f.selected))control.setValue(String(f.selected),true);
        }
        row.style.display=f.visible===false?'none':'';row.setAttribute('aria-hidden',f.visible===false?'true':'false');}});
    });
    new MutationObserver(function(){const r=root();if(r&&!seen.has(r)){seen.add(r);intentSeq=0;Shiny.setInputValue('graph_selectors_mounted',{scope:r.dataset.scope,generation:++seq},{priority:'event'});}}).observe(document.getElementById('workflow_controls'),{childList:true,subtree:true});
  });
})();
