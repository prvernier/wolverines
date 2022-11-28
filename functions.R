# load data into reactiveValues object
# This is essentially a list that can be passed into other functions
load.data <- function() {
  data <- reactiveValues(
    grid = st_read('data/wolverines.gpkg', 'grid5k', quiet = T),
    linear = st_read('data/wolverines.gpkg', 'linear_features', quiet = T),
    areal = st_read("data/wolverines.gpkg", "areal_features", quiet=T),
    factors = st_read("data/wolverines.gpkg", "survey_factors", quiet=T)
  )
  
  return(data)
}

# server functions 
# update inputs 
update.inputs <- function(input, session, data) {
  # print('in update inputs')
  updateSelectInput(inputId = 'inv',
                    choices = names(data$factors)[3:22],
                    selected = 'merge100_pct')
  
  updateSelectInput(inputId = 'factors',
                    choices = names(data$factors)[3:22],
                    selected = c('merge100_pct','elev_median','elev_sd','forest_pct',
                                 'water_pct')
  )
}


# create reactive clusters() object (data$clusters)
create.clusters <- function(input, session, data) {
  # print('in create.clusters')
  shinybusy::add_busy_spinner()
  # duplicate data$factors, replace NAs with 0s 
  x <- data$factors %>%
    mutate(
      placer_pct=ifelse(is.na(placer_pct),0,placer_pct),
      quartz_pct=ifelse(is.na(quartz_pct),0,quartz_pct),
      recent_fires_pct=ifelse(is.na(recent_fires_pct),0,recent_fires_pct),
      area500_pct=ifelse(is.na(area500_pct),0,area500_pct),
      line500_pct=ifelse(is.na(line500_pct),0,line500_pct),
      merge100_pct=ifelse(is.na(merge100_pct),0,merge100_pct),
      water_pct=ifelse(is.na(water_pct),0,water_pct),
      forest_pct=ifelse(is.na(forest_pct),0,forest_pct),
      wetland_pct=ifelse(is.na(wetland_pct),0,wetland_pct)
     )
  
  # select only factors that user wants to cluster by
  # st_drop_geometry() drops geom field from table (geom describes what type of
  # feature it is and has some numbers describing it)
  y <- select(x, unlist(input$factors)) %>% 
    st_drop_geometry()
  
  # Now actually cluster cells
  # kmeans() returns a list; $cluster object is a vector where the name is the
  # cell number and the value is what cluster it's in
  clust <- kmeans(scale(y), input$clusters)$cluster
  # print('kmeans')
  
  # add field to x with what cluster the cell is in
  x <- x %>%
    mutate(clusters = clust)
  # print('mutate(clusters = clust)')
  
  shinybusy::remove_modal_spinner()
  # print(x)
  # print(class(x))
  data$clusters <- x
  # print('finished create.clusters')
  return(data)
}

render.map1 <- function(input, output, session, data) {
  # print('in render.map1')
  output$map1 <- renderTmap({
    # Set color palette based on selected characteristics
    if (input$inv == 'forest_pct') {
      pal <- 'YlGn'
    } else if (input$inv %in% c('water_pct','wetland_pct')) {
      pal <- 'PuBu'
    } else if (input$inv %in% c('quartz_pct','placer_pct')) {
      pal <- 'Greys'
    } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
      pal <- '-BrBG'
    } else {
      pal <- 'Reds'
    }
    
    
    # If the user clicks the button to generate clusters
    if (input$clustButton) {
      # print('if(input$clustButton)')
      m <- tm_shape(data$clusters) + tm_fill('clusters', palette = 'Set1',
                                          alpha = 1,
                                          group = 'Clusters',
                                          # zindex determines the order that elements
                                          # appear on the page; an element with a
                                          # higher z index appears in front of a lower
                                          # zindex
                                          zindex = 500)
    } else {
      # print('in else')
      m <- tm_shape(data$factors) + tm_fill(input$inv, palette = pal, 
                                            style = input$style,
                                            alpha = 1,
                                            colorNA = NULL, 
                                            group = input$inv,
                                            title = input$inv,
                                            zindex = 500)
    }
    
    # Base map
    m <- m + tm_shape(data$grid) + tm_borders() +
      tm_shape(data$linear) + tm_lines(col = 'black',
                                       alpha = 1,
                                       group = 'linfeat', zindex = 560) +
      tm_shape(data$areal)  + tm_fill(col = 'red',
                                      alpha = 1,
                                      group = 'arealfeat', zindex = 570)

    # print('added linear and areal features')
    if (input$goButton) {
      
    }
    m <- m + tm_legend(position = c('right', 'top'), frame = T)
    # print('reached end of tmap rendering')
    m
  }) # renderTmap
  print('finished render.map1')
} # render.map1


update.transparency <- function(input, session, data) {
  # shinybusy::add_busy_spinner()
  observeEvent(input$alpha, ignoreInit = T, handlerExpr = {
  print('in update.transparency')
    # Couldn't get pal to be within the scope of this function so I just copied
    # and pasted it 
    if (input$inv == 'forest_pct') {
      pal <- 'YlGn'
    } else if (input$inv %in% c('water_pct','wetland_pct')) {
      pal <- 'PuBu'
    } else if (input$inv %in% c('quartz_pct','placer_pct')) {
      pal <- 'Greys'
    } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
      pal <- '-BrBG'
    } else {
      pal <- 'Reds'
    }
    
    if (input$clustButton > 0) {
      print('clust button clicked')
      # create new layers for clusters/factors with alpha specified
      m1 <- tm_shape(data$clusters) + tm_fill('clusters', palette = 'Set1',
                                           alpha = input$alpha,
                                           group = 'Clusters',
                                           zindex = 500)
    } else {
      print('in else')
      m1 <- tm_shape(data$factors) + tm_fill(input$inv, palette = pal, 
                                             style = input$style,
                                        alpha = input$alpha,
                                        colorNA = NULL, 
                                        group = input$inv,
                                        title = input$inv,
                                        zindex = 500)
    }
    m.linareal <- tm_shape(data$linear) + tm_lines(col = 'black',
                                              alpha = input$alpha,
                                              group = 'linfeat', zindex = 560) +
      tm_shape(data$areal) + tm_fill(col = 'red',
                                alpha = input$alpha,
                                group = 'arealfeat', zindex = 570)
    tmapProxy('map1',
              x = {tm_remove_layer(500) +
                  tm_remove_layer(560) +
                  tm_remove_layer(570) +
                  m.linareal +
                  m1}
    ) # tmapProxy 
  }) # observeEvent
  # shinybusy::remove_modal_spinner()
} # update.transparency()

map.selected.cells <- function(input, output, session, data) {
  # add randomly selected cells
  print('in map.selected.cells')
  # m <- m + tm_shape(data$n2) + tm_borders(col = 'green', lwd = 2,
  #                                         group = 'simp.random', zindex = 600) +
  #          tm_shape(data$n2) + tm_borders(col = 'yellow', lwd = 2,
  #                                  group = 'strat.random', zindex = 600)

  tmapProxy('map1', session, x = {
    tm_shape(data$n2) + tm_borders(col = 'green', lwd = 2,
                                   group = 'simp.random', zindex = 800) +
    tm_shape(data$n2) + tm_borders(col = 'yellow', lwd = 2,
                                   group = 'strat.random', zindex = 800)
  })
}

create.dta1 <- function(data) {
  print('in create.dta1')
  # print(data$clusters)
  # print(is.null(data$clusters))
  
  if (is.null(data$clusters)) {
    print('returning early')
    return()
  }
  
  data$dta1 <- data$clusters %>%
    st_drop_geometry() %>%
    group_by(clusters) %>%
    summarize(n = n(),
              merge100_pct = round(mean(merge100_pct),1),
              recent_fires_pct = round(mean(recent_fires_pct),1),
              #quartz_pct = round(mean(quartz_pct),1),
              #placer_pct = round(mean(placer_pct),1),
              elev_median = round(mean(elev_median),1),
              elev_sd = round(mean(elev_sd),1),
              forest_pct = round(mean(forest_pct),1),
              water_pct = round(mean(water_pct),1),
              wetland_pct = round(mean(wetland_pct),1)
             )
  print('finished create.dta1')
  return(data)
} # create.dta1()

render.tab1 <- function(output, data) {
  print('in render.tab1')
  output$tab1 <- DT::renderDataTable({
    x <- data$dta1
    datatable(x, rownames = F, 
              options = list(dom = 'tip', 
                             scrollX = T, 
                             scrollY = T,
                             pageLength = 25),
              class = 'compact')
  })
  print('finished render.tab1')
}

render.tab2 <- function(output, session, data) {
  print('in render.tab2')
  output$tab2 <- DT::renderDataTable({
    print('in renderDataTable for tab2')
    simple <- tibble(type = 'simple', id = pull(data$n1, id)) %>%
      left_join(data$factors)
    
    stratified <- tibble(type = 'stratified', id = pull(data$n2, id)) %>%
      left_join(data$factors)
    
    ks_simple <- 
      tibble(type = 'simple',
             merge100_pct = ks.test(data$factors$merge100_pct, simple$merge100_pct)[[1]],
             #merge500_pct = ks.test(data$factors$merge500_pct, simple$merge500_pct)[[1]],
             placer_pct = ks.test(data$factors$placer_pct, simple$placer_pct)[[1]],
             quartz_pct = ks.test(data$factors$quartz_pct, simple$quartz_pct)[[1]],
             recent_fires_pct = ks.test(data$factors$recent_fires_pct, simple$recent_fires_pct)[[1]],
             benchmark_pct = ks.test(data$factors$benchmark_pct, simple$benchmark_pct)[[1]],
             elev_median = ks.test(data$factors$elev_median, simple$elev_median)[[1]],
             elev_sd = ks.test(data$factors$elev_sd, simple$elev_sd)[[1]],
             forest_pct = ks.test(data$factors$forest_pct, simple$forest_pct)[[1]],
             wetland_pct = ks.test(data$factors$wetland_pct, simple$wetland_pct)[[1]],
             water_pct = ks.test(data$factors$water_pct, simple$water_pct)[[1]]
      )
    
    ks_stratified <- 
      tibble(type="stratified",
             merge100_pct = ks.test(data$factors$merge100_pct, stratified$merge100_pct)[[1]],
             #merge500_pct = ks.test(data$factors$merge500_pct, stratified$merge500_pct)[[1]],
             placer_pct = ks.test(data$factors$placer_pct, stratified$placer_pct)[[1]],
             quartz_pct = ks.test(data$factors$quartz_pct, stratified$quartz_pct)[[1]],
             recent_fires_pct = ks.test(data$factors$recent_fires_pct, stratified$recent_fires_pct)[[1]],
             benchmark_pct = ks.test(data$factors$benchmark_pct, stratified$benchmark_pct)[[1]],
             elev_median = ks.test(data$factors$elev_median, stratified$elev_median)[[1]],
             elev_sd = ks.test(data$factors$elev_sd, stratified$elev_sd)[[1]],
             forest_pct = ks.test(data$factors$forest_pct, stratified$forest_pct)[[1]],
             wetland_pct = ks.test(data$factors$wetland_pct, stratified$wetland_pct)[[1]],
             water_pct = ks.test(data$factors$water_pct, stratified$water_pct)[[1]])
    # View(ks_simple)
    # View(ks_stratified)
    
    ks <- rbind(ks_simple, ks_stratified)
    
    print('finished renderDataTable for tab2')
    
    datatable(ks, rownames = F, options = list(dom = 'tip', scrollX = T,
                                               scrollY = T, pageLength = 25),
              class = 'compact') %>%
      formatRound(columns = c('merge100_pct', 'placer_pct', 'quartz_pct', 
                              'recent_fires_pct', 'benchmark_pct', 'elev_median',
                              'elev_sd', 'forest_pct', 'wetland_pct', 'water_pct'),
                  digits = 3)
  }) # renderDataTable
  print('finished render.tab2')
} # render.tab2()

strat.sample <- function(input, data) {
    print('in strat sample')
    # n1 is random sampling; ie same number is not sampled from each cluster
    # sample from data$clusters (defined above), number to be samples is number of
    #   samples per cluster * number of clusters
    data$n1 <- sample_n(data$clusters, input$size * as.numeric(input$clusters))
    
    # n2 is stratified random sampling; ie same number sampled from each cluster
    data$n2 <- data$clusters %>%
          group_by(clusters) %>%
          sample_n(size = input$size)
    
    print('finished strat.sample')
  
  return(data)
}



