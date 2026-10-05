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
const loc=JSON.parse(fs.readFileSync('/tmp/phase2-local.json'));const src=JSON.parse(fs.readFileSync('/tmp/phase2-source.json'));
async function input(id,value){await page.evaluate(({id,value})=>Shiny.setInputValue(id,value,{priority:'event'}),{id,value});await settle();}
async function selectGroups(groups){await input('graph_dcst_table_selection',{project:'comb_fermat_embeddings_01_oct_2026',level:'udcst_level2',groups});}
try{
 await open();await choose('graph_sample_subset','All');await selectGroups([]);
 await input('source_datasets-datasets',[src.dataset]);
 const selected=exp.ids.map((x,i)=>src.ids.includes(x)?i+1:0).filter(Boolean);await match(selected);console.log('PASS source filter',src.dataset,selected.length);
 await input('source_datasets-cross',1);await page.locator('#source_datasets-cross_table table').waitFor({state:'attached',timeout:30000});
 console.log('Cross-table rows',await page.locator('#source_datasets-cross_table tbody tr').count());
 await page.locator('.modal-footer button').filter({hasText:'Dismiss'}).click();await settle();
 await input('source_datasets-datasets',[]);
 await selectGroups(['absent-state-for-empty-view-test']);await match([]);
 await input('source_datasets-show',true);await settle();
 assert.equal(await page.locator('.shiny-output-error').count(),0);
 await input('source_datasets-cross',2);await page.locator('#source_datasets-cross_table table').waitFor({state:'attached',timeout:30000});
 assert.equal(await page.locator('#source_datasets-cross_table tbody tr').count(),0);await page.locator('.modal-footer button').filter({hasText:'Dismiss'}).click();await settle();
 console.log('PASS empty 3D, 2D and cross-table');
 await selectGroups([]);await input('source_datasets-show',false);
 await input('local_atlas-region',loc.region);await input('local_atlas-view',loc.view);
 await page.waitForFunction(()=>document.querySelector('#local_atlas-context')?.textContent.includes('Saved local fit'),{},{timeout:120000});await settle();
 assert.equal((await vertices()).length,4000);
 assert.equal(await page.locator('#graph_layout_color_by').inputValue(),'udcst');
 await choose('graph_sample_subset','2_60');
 const wanted=new Set(exp.subsets['2_60'].map(i=>exp.ids[i-1]));const localExpected=loc.ids.map((id,i)=>wanted.has(id)?i+1:0).filter(Boolean);await match(localExpected);
 console.log('PASS Li 4000 local preset',localExpected.length);
 await input('source_datasets-show',true);await settle();
 await page.waitForFunction(()=>document.getElementById('source_datasets-plot')?.data?.length>0,{},{timeout:120000});
 const ids2d=await page.evaluate(()=>[...new Set(document.getElementById('source_datasets-plot').data.flatMap(t=>Array.from(t.customdata||[])))]);
 assert.deepEqual(ids2d.sort(),localExpected.map(i=>loc.ids[i-1]).sort());
 const visible=new Set(await vertices());const endpointIndices=await page.evaluate(()=>document.getElementById('reference_plot').data.filter(t=>t.name==='Endpoints').flatMap(t=>Array.from(t.customdata||[])));
 assert(endpointIndices.every(x=>visible.has(x)));console.log('PASS local linked IDs and endpoint visibility',ids2d.length,endpointIndices.length);
 const plot=page.locator('#reference_plot');const box=await plot.boundingBox();await page.mouse.move(box.x+box.width*.5,box.y+box.height*.5);await page.mouse.down();await page.mouse.move(box.x+box.width*.64,box.y+box.height*.61,{steps:12});await page.mouse.up();await settle();
 await page.screenshot({path:'/tmp/phase2-local.png'});
 assert.equal(await page.locator('.shiny-output-error').count(),0);assert.deepEqual(errors,[]);
 fs.writeFileSync('/tmp/phase2-local-evidence.json',JSON.stringify({source_filter:selected.length,empty_views:true,local_region:loc.region,local_visible:ids2d.length,endpoint_indices:endpointIndices,pointer_rotation:true,errors},null,2));console.log('ALL LOCAL CHECKS PASSED');
}finally{await browser.close();}
