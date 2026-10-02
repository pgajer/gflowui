(function () {
  'use strict';
  let state = {project_id: '', region: '', entries: [], config: {}}, applied = new Set();
  let menu = null, lastCatalog = '', timer = null, applying = false;
  const array = x => x == null ? [] : Array.isArray(x) ? x : [x];
  const value = select => array(select.selectize ? select.selectize.getValue() :
    Array.from(select.selectedOptions).map(o => o.value)).map(String);
  const equal = (a,b) => JSON.stringify(array(a)) === JSON.stringify(array(b));
  function labelFor(select) {
    const own = document.querySelector('label[for="' + CSS.escape(select.id) + '"]');
    if (own && own.textContent.trim()) return own;
    const row = select.closest('.gf-graph-row');
    return row && row.querySelector('.gf-graph-row-label');
  }
  function controls() {
    const excluded = array(state.config.exclude);
    return Array.from(document.querySelectorAll('#workflow_controls select[id]')).filter(s =>
      !excluded.includes(s.id) && !s.closest('.modal') && labelFor(s));
  }
  function available(select) {
    return select.selectize ? Object.keys(select.selectize.options) : Array.from(select.options).map(o => o.value);
  }
  function conditionalVisible(select) {
    // Collapsed accordion panels still receive defaults; hidden conditional fields do not.
    let parent = select.parentElement;
    while (parent && parent.id !== 'workflow_controls') {
      if ((parent.hasAttribute('data-display-if') || parent.getAttribute('aria-hidden') === 'true') &&
          getComputedStyle(parent).display === 'none') return false;
      parent = parent.parentElement;
    }
    return !select.disabled;
  }
  function contextFor(select) {
    const context = {};
    array((state.config.dependencies || {})[select.id]).forEach(id => {
      const dependency = document.getElementById(id);
      if (dependency && dependency.tagName === 'SELECT' && conditionalVisible(dependency)) context[id] = value(dependency);
    });
    return context;
  }
  const scope = select => select.id === 'local_atlas-region' ? '' : state.region || '';
  const keyFor = select => JSON.stringify([select.id, scope(select), contextFor(select)]);
  function closeMenu(focus) {
    if (!menu) return;
    const label = menu.label; menu.node.remove(); menu = null;
    label.setAttribute('aria-expanded','false'); if (focus && label.isConnected) label.focus();
  }
  function openMenu(select, label) {
    closeMenu(false);
    if (!state.project_id || !select || !select.isConnected) return;
    const selected = value(select), context = contextFor(select), region = scope(select), project = state.project_id;
    const labels = selected.length ? selected.map(v => {const o=Array.from(select.options).find(o=>o.value===v);return o ? o.textContent : v;}) : ['No selection'];
    const node = document.createElement('div'); node.className='gf-default-menu';node.id='gf-default-menu';node.setAttribute('role','menu');
    const caption=document.createElement('div');caption.className='gf-default-menu-caption';caption.textContent=labels.join(', ');node.appendChild(caption);
    const save=document.createElement('button');save.type='button';save.textContent='Set as Default';save.setAttribute('role','menuitem');
    const cancel=document.createElement('button');cancel.type='button';cancel.textContent='Cancel';cancel.setAttribute('role','menuitem');
    save.addEventListener('click',()=>{
      if (state.project_id===project && scope(select)===region) {
        applied.add(keyFor(select));
        Shiny.setInputValue('dropdown_default_save',{project_id:project,entry:{id:select.id,value:selected,value_label:labels,context:context,region:region}},{priority:'event'});
      }
      closeMenu(true);
    });
    cancel.addEventListener('click',()=>closeMenu(true));node.append(save,cancel);document.body.appendChild(node);
    const rect=label.getBoundingClientRect();node.style.left=Math.max(8,Math.min(rect.left,innerWidth-node.offsetWidth-8))+'px';
    node.style.top=Math.max(8,Math.min(rect.bottom+4,innerHeight-node.offsetHeight-8))+'px';
    menu={node:node,label:label};label.setAttribute('aria-expanded','true');save.focus();
    node.addEventListener('keydown',e=>{
      if(e.key==='Escape'){e.preventDefault();closeMenu(true);}
      if(e.key==='ArrowDown'||e.key==='ArrowUp'){e.preventDefault();(document.activeElement===save?cancel:save).focus();}
      if(e.key==='Tab')closeMenu(false);
    });
  }
  function scan() {
    if(!state.project_id || !window.Shiny)return;
    const selects=controls(), catalog=[];
    for(const select of selects) {
      const label=labelFor(select);
      if(!label.dataset.gfDefaultFor) {
        label.dataset.gfDefaultFor=select.id;label.classList.add('gf-default-label');label.tabIndex=0;
        label.setAttribute('role','button');label.setAttribute('aria-haspopup','menu');label.setAttribute('aria-expanded','false');
        label.title='Click to set the default for this dropdown';
        if(!label.id)label.id=select.id+'-default-label';
        select.setAttribute('aria-labelledby',label.id);
        if(select.selectize)select.selectize.$control_input.attr('aria-labelledby',label.id);
        label.addEventListener('click',e=>{e.preventDefault();e.stopPropagation();openMenu(document.getElementById(label.dataset.gfDefaultFor),label);});
        label.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();openMenu(document.getElementById(label.dataset.gfDefaultFor),label);}});
      }
      catalog.push({id:select.id,label:label.textContent.trim().replace(/:$/,''),choices:available(select),multiple:select.multiple});
    }
    const encoded=JSON.stringify(catalog);
    if(encoded!==lastCatalog){lastCatalog=encoded;Shiny.setInputValue('dropdown_defaults_catalog',{project_id:state.project_id,controls:catalog},{priority:'event'});}
    // Walk dependencies in UI order, allowing Shiny to rebuild downstream choices between restores.
    for(const select of selects) {
      if(!conditionalVisible(select))continue;
      const key=keyFor(select);if(applied.has(key))continue;
      const entry=array(state.entries).find(e=>e.id===select.id && (e.region||'')===scope(select) &&
        Object.entries(e.context||{}).every(([id,v])=>{const d=document.getElementById(id);return d && equal(value(d),v);}));
      if(!entry)continue;
      const wanted=array(entry.value).map(String);
      if((!wanted.length && !select.multiple) || !wanted.every(v=>available(select).includes(v)))continue;
      applied.add(key);
      if(equal(value(select),wanted))continue;
      applying=true;
      if(select.selectize)select.selectize.setValue(select.multiple?wanted:wanted[0]);
      else {$(select).val(select.multiple?wanted:wanted[0]).trigger('change');}
      applying=false;schedule();break;
    }
  }
  function schedule(){clearTimeout(timer);timer=setTimeout(scan,180);}
  $(function(){
    Shiny.addCustomMessageHandler('gflowuiDropdownDefaults',message=>{
      if(state.project_id!==message.project_id){applied=new Set();lastCatalog='';closeMenu(false);}
      state=message;state.entries=array(message.entries);state.config=message.config||{};schedule();
    });
    new MutationObserver(schedule).observe(document.getElementById('workflow_controls'),{childList:true,subtree:true});
    $(document).on('change','#workflow_controls select',schedule);
    function touched(event) {
      const holder=event.target.closest('.shiny-input-container');
      const select=event.target.tagName==='SELECT'?event.target:holder&&holder.querySelector('select');
      if(select && select.closest('#workflow_controls') && !event.target.closest('.gf-default-label'))applied.add(keyFor(select));
    }
    document.addEventListener('pointerdown',touched,true);
    document.addEventListener('keydown',touched,true);
    $(document).on('shiny:idle shiny:bound shiny:value',schedule);
    document.addEventListener('pointerdown',e=>{if(menu&&!menu.node.contains(e.target)&&e.target!==menu.label)closeMenu(false);});
  });
})();
