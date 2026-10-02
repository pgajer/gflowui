test_that('selection patches validate scope, ordering and available values', {
  fields<-list(list(input_id='metric',selected='E',choices=c('E','H')),
    list(input_id='inner',selected='ambient',choices=c('ambient','fermat')))
  r<-list(scope='project|region',seq=3,id='inner',value='fermat')
  expect_equal(gflowui_selection_patch(r,'project|region',fields,2)$values,list(metric='E',inner='fermat'))
  expect_null(gflowui_selection_patch(r,'other|region',fields,2))
  expect_null(gflowui_selection_patch(r,'project|region',fields,3))
  r$value<-'missing';expect_null(gflowui_selection_patch(r,'project|region',fields,2))
})

test_that('bounded caches evict least recently used entries and oversized values', {
  cache<-gflowui_lru_cache(2,1000); n<-0
  make<-function(){n<<-n+1;n}
  expect_equal(cache('a',make),1);expect_equal(cache('b',make),2)
  expect_equal(cache('a',make),1);expect_equal(cache('c',make),3)
  expect_equal(cache('b',make),4)
  big<-function(){n<<-n+1;rep(n,1000)}
  cache('big',big);cache('big',big);expect_equal(n,6)
})

test_that('hover cache follows ordering, membership, count and changed assets', {
  a<-list(sample_ids=c('a','b'),taxon_names=c('one','two'),
    indices=list(1:2,2:1),abundances=list(c(.7,.3),c(.6,.4)))
  cache<-gflowui_hover_cache()
  compare<-function(ids,a,n)expect_identical(cache(ids,a,n),gflowui_vertex_hover_text(ids,a,n))
  compare(c('a','b'),a,2);compare(c('a','b'),a,2)
  compare(c('b','a'),a,2);compare('b',a,1)
  a$abundances[[2]]<-c(.9,.1);compare('b',a,1)
  expect_null(cache(NULL,NULL,4))
})

test_that('panel structure remains stable when only contents change', {
  panels<-function(text)bslib::accordion_panel('Graphs',value='graphs',shiny::tagList(shiny::p(text),NULL,shiny::selectInput('choice','Choice',c('a','b'))))
  first<-gflowui_workflow_parts(list(panels('first')), 'graphs')
  second<-gflowui_workflow_parts(list(panels('second')), 'graphs')
  expect_identical(first$signature,second$signature)
  expect_length(first$chunks,3)
  expect_match(as.character(first$chunks[[1]]),'first')
  expect_match(as.character(second$chunks[[1]]),'second')
  expect_null(first$chunks[[2]])
  expect_match(as.character(first$panels[[1]]),'stable_graphs_3')
})

test_that('scene deltas move every coordinate-bearing layer without resending annotations', {
  a<-list(data=list(list(type='scatter3d',x=1:2,y=3:4,z=5:6,customdata=1:2,hovertext=c('a','b')),
    list(type='scatter3d',mode='lines',x=c(1,2,NA),y=c(3,4,NA),z=c(5,6,NA))),
    layout=list(scene=list(uirevision='stable')),config=list())
  b<-a;b$data[[1]]$x<-c(2,3);b$data[[2]]$x<-c(2,3,NA)
  d<-gflowui_scene_delta(a,b)
  expect_equal(d$kind,'coordinates');expect_equal(d$indices,0:1)
  expect_identical(d$coordinates[[2]]$x,b$data[[2]]$x)
  expect_false('hovertext' %in% names(d$coordinates[[1]]))
  expect_equal(gflowui_scene_delta(a,a)$kind,'unchanged')
  b$data[[1]]$customdata<-2:1;expect_equal(gflowui_scene_delta(a,b)$kind,'full')
  b<-a;b$data[[1]]$hovertext<-c('new','b');expect_equal(gflowui_scene_delta(a,b)$kind,'full')
  b<-a;b$data[[1]]$marker<-list(color='red');expect_equal(gflowui_scene_delta(a,b)$kind,'full')
  b<-a;b$data<-b$data[1];expect_equal(gflowui_scene_delta(a,b)$kind,'full')
  b<-a;b$layout$scene$aspectmode<-'data';expect_equal(gflowui_scene_delta(a,b)$kind,'full')
})

test_that('unchanged structural state does not invalidate its consumers', {
  shiny::testServer(function(input,output,session){
    source<-shiny::reactiveVal(list(key='a',ignored=0))
    stable<-gflowui_distinct_reactive(function()source()$key)
    counter<-new.env();counter$n<-0L
    shiny::observe({stable();counter$n<-counter$n+1L})
  },{
    session$flushReact(); first<-counter$n
    source(list(key='a',ignored=1));session$flushReact()
    expect_identical(counter$n,first)
    source(list(key='b',ignored=1));session$flushReact()
    expect_identical(counter$n,first+1L)
  })
})

test_that('scene server sends coordinates on a warmed matching canvas and resyncs remounts', {
  skip_if_not_installed('plotly')
  shiny::testServer(function(input,output,session){
    state<-shiny::reactiveValues(offset=0,scope='a',seq=0)
    sent<-new.env();sent$messages<-list()
    session$sendCustomMessage<-function(type,message){sent$messages[[length(sent$messages)+1L]]<-list(type=type,message=message)}
    widget<-shiny::reactive(plotly::plot_ly(x=c(0,1)+state$offset,y=c(0,1),z=c(0,1),type='scatter3d',mode='markers'))
    gflowui_scene_server(input,output,session,widget,function()list(scope=state$scope,selection_seq=state$seq,set_id='g'))
  },{
    session$flushReact()
    session$setInputs(gflowui_scene_mounted=list(generation=1L,revision=1L))
    state$offset<-2;state$seq<-1;session$flushReact()
    expect_equal(tail(sent$messages,1)[[1]]$message$kind,'coordinates')
    expect_equal(tail(sent$messages,1)[[1]]$message$selection_seq,1)
    session$setInputs(gflowui_scene_mounted=list(generation=1L,revision=1L,remount=TRUE))
    expect_equal(tail(sent$messages,1)[[1]]$message$kind,'full')
    before<-length(sent$messages)
    session$setInputs(gflowui_scene_resync=list(generation=99L))
    expect_equal(length(sent$messages),before)
    state$seq<-2;session$flushReact()
    expect_equal(tail(sent$messages,1)[[1]]$message$kind,'unchanged')
  })
})

test_that("browser selection and scene protocols reject stale events", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node is unavailable")
  withr::local_dir(test_path("..", ".."))
  for (script in c("graph-selection-regression.js", "scene-updates-regression.js", "lazy-edges-regression.js")) {
    result <- system2(node, file.path("tests", "testthat", script), stdout=TRUE, stderr=TRUE)
    expect_null(attr(result, "status"), info=paste(result, collapse="\n"))
  }
})


test_that('edge requests are limited to the current scene and generation', {
  shiny::testServer(function(input,output,session){
    state<-shiny::reactiveValues(key='current',scope='a')
    sent<-new.env();sent$messages<-list()
    session$sendCustomMessage<-function(type,message){sent$messages[[length(sent$messages)+1L]]<-list(type=type,message=message)}
    widget<-shiny::reactive(plotly::plot_ly(x=1,y=1,z=1,type='scatter3d',mode='lines',
      meta=list(gflowui_edges=list(key=state$key))))
    registry<-list(get=function(key)matrix(c(1L,2L),ncol=2))
    gflowui_scene_server(input,output,session,widget,
      function()list(scope=state$scope,selection_seq=0,set_id='g'),edges=registry)
  },{
    session$flushReact()
    session$setInputs(gflowui_edge_request=list(generation=1L,key='unknown',request_id=1))
    expect_length(sent$messages,0)
    session$setInputs(gflowui_edge_request=list(generation=99L,key='current',request_id=2))
    expect_length(sent$messages,0)
    session$setInputs(gflowui_edge_request=list(generation=1L,key='current',request_id=3))
    expect_equal(sent$messages[[1]]$type,'gflowuiEdgeData')
    expect_equal(sent$messages[[1]]$message$a,1L)
    expect_equal(sent$messages[[1]]$message$b,2L)
    session$setInputs(gflowui_scene_mounted=list(generation=1L,revision=1L))
    state$key<-'replacement';session$flushReact();before<-length(sent$messages)
    session$setInputs(gflowui_edge_request=list(generation=1L,key='current',request_id=4))
    expect_length(sent$messages,before)
  })
})
