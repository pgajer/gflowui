import {chromium} from '/Users/pgajer/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright/index.mjs';
import fs from 'node:fs';import assert from 'node:assert/strict';
const exp=JSON.parse(fs.readFileSync('/tmp/phase2-expected.json','utf8'));
const browser=await chromium.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',args:['--enable-webgl','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
const page=await browser.newPage({viewport:{width:1600,height:1100}});const errors=[];page.on('pageerror',e=>errors.push(e.message));
const evidence=[];
async function settle(){await page.waitForTimeout(600);await page.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy'),{},{timeout:120000});await page.waitForTimeout(700);}
async function open(){await page.goto('http://127.0.0.1:3875');await page.waitForTimeout(1800);await page.evaluate(()=>Shiny.setInputValue('project_select','comb_fermat_embeddings_01_oct_2026',{priority:'event'}));await page.locator('#graph_selector_metric').waitFor({state:'attached',timeout:120000});await settle();}
async function choose(id,value){await page.evaluate(({id,value})=>{const el=document.getElementById(id);if(!el?.selectize?.options[value])throw Error('Unavailable '+id+' '+value);if(id.startsWith('graph_selector_'))window.gflowuiGraphSelection.select(id,value);else el.selectize.setValue(value);},{id,value});await settle();}
async function vertices(){return await page.evaluate(()=>{const el=document.querySelector('#reference_plot .js-plotly-plot')||document.getElementById('reference_plot');return [...new Set((el?.data||[]).filter(t=>t.meta?.gflowui_vertices).flatMap(t=>Array.from(t.customdata||[])))].sort((a,b)=>a-b);});}
async function match(ids){await page.waitForFunction(n=>{const el=document.querySelector('#reference_plot .js-plotly-plot')||document.getElementById('reference_plot');return (el?.data||[]).filter(t=>t.meta?.gflowui_vertices).reduce((s,t)=>s+(t.customdata?.length||0),0)===n;},ids.length,{timeout:120000});assert.deepEqual(await vertices(),ids);}
try{
 await open();console.log('Opened copied project');
 console.log('plots',await page.locator('.js-plotly-plot').evaluateAll(xs=>xs.map(x=>({id:x.id,parent:x.parentElement.id,traces:x.data?.length}))));
 for(const metric of ['Euclidean (L¹-normalized)','Euclidean (square-root)']){
  await choose('graph_selector_metric',metric);await choose('graph_selector_construction','Ambient (complete p=1)');
  for(const route of ['Direct 3D MDS','10D MDS → PCA 3D']){
   await choose('graph_selector_route',route);
   for(const subset of Object.keys(exp.subsets)){
    await choose('graph_sample_subset',subset);await match(exp.subsets[subset]);
    evidence.push({metric,route,subset,count:(await vertices()).length});console.log('PASS',metric,route,subset,(await vertices()).length);
   }
  }
 }
 await choose('graph_cst_type','dcst');await choose('graph_dcst_level','dcst_level3');await choose('graph_cst_type','udcst');
 assert.equal(await page.locator('#graph_dcst_level').inputValue(),'udcst_level2');
 await choose('graph_sample_subset','All');
 const group='Lactobacillus_crispatus + Lactobacillus_iners';
 const box=page.locator('.gf-dcst-table input[type=checkbox]').filter({visible:true});
 await page.locator('.gf-dcst-table input[type=checkbox]').evaluateAll((xs,g)=>xs.find(x=>x.value===g).click(),group);await settle();
 await match(exp.labels.map((x,i)=>x===group?i+1:0).filter(Boolean));
 await page.evaluate(()=>Shiny.setInputValue('source_datasets-show',true,{priority:'event'}));await settle();
 await page.waitForFunction(()=>document.getElementById('source_datasets-plot')?.data?.length>0,{},{timeout:120000});
 const ids2d=await page.evaluate(()=>[...new Set(document.getElementById('source_datasets-plot').data.flatMap(t=>Array.from(t.customdata||[])))]);
 assert.deepEqual(ids2d.sort(),(await vertices()).map(i=>exp.ids[i-1]).sort());
 console.log('PASS linked 2D IDs',ids2d.length);
 await page.locator('.gf-dcst-table input[type=color]').evaluateAll((xs,g)=>{const x=xs.find(x=>x.dataset.group===g);x.value='#123456';x.dispatchEvent(new Event('change',{bubbles:true}));},group);await settle();
 await page.screenshot({path:'/tmp/phase2-linked.png'});
 await choose('graph_sample_subset','2_60');await page.waitForTimeout(1400);
 await open();assert.equal(await page.locator('#graph_sample_subset').inputValue(),'2_60');assert.equal(await page.locator('#graph_cst_type').inputValue(),'udcst');
 const restored=await page.locator('.gf-dcst-table input[type=color]').evaluateAll((xs,g)=>xs.find(x=>x.dataset.group===g).value,group);assert.equal(restored,'#123456');
 console.log('PASS reopen subset, group selection, type/level and palette');
 assert.equal((await vertices()).length,3508);
 const errtexts=await page.locator('.shiny-output-error').allTextContents();assert.deepEqual(errtexts,[]);assert.deepEqual(errors,[]);
 fs.writeFileSync('/tmp/phase2-browser-evidence.json',JSON.stringify({checks:evidence,linked_ids:ids2d.length,persistence:true,errors},null,2));
 console.log('ALL CHECKS PASSED');
} finally{await browser.close();}
