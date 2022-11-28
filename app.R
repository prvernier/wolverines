## Load Required Libraries
library(shiny)
library(sf)
library(DT)
library(tmap)
library(dplyr)
library(magrittr)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinybusy)

functionspath <- list.files(pattern = 'functions.R', full.names = T)
source(functionspath)

## Set up map options
tmap_mode('view')
tmap_options(check.and.fix = T)
# Basemap 
tm_basemap(leaflet::providers$Esri.WorldImagery)

shinybusy::use_busy_spinner()

## Load Data
# grid <- st_read('data/wolverines.gpkg', 'grid5k', quiet = T)
# linear <- st_read("data/wolverines.gpkg", "linear_features", quiet=T)
# areal <- st_read("data/wolverines.gpkg", "areal_features", quiet=T)
# factors <- st_read("data/wolverines.gpkg", "survey_factors", quiet=T)


ui = dashboardPage(
  dashboardHeader(title = 'Wolverines Survey'),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem('Survey Factors', tabName = 'fri', icon = icon('th'))
    ), # sidebarMenu
    
    # Select feature type (merge_100, percent_forest, etc) to view
    selectInput("inv", label = "Feature type:", 
                # choices = names(factors)[3:22],
                # selected = 'merge_100'),
                # choices defined server side 
                choices = NULL,
                selected = NULL),
    # Style for binning cells for display
    selectInput("style", label="Select style:", 
                choices=c("jenks","kmeans","quantile","pretty","equal"), 
                selected="quantile"), 
    # Set transparency
    sliderInput("alpha", label="Transparency:", min=0, max=1, value=1, step=0.1, 
                ticks=FALSE),
    hr(),
    # features to cluster by
    selectInput("factors", label = "Select features:", multiple=TRUE,
                # choices = names(factors)[3:22],
                # selected=c('merge100_pct','elev_median','elev_sd','forest_pct',
                #            'water_pct')
                choices = NULL,
                selected = NULL
               ),
    # how many clusters
    sliderInput("clusters", label="Number of clusters:", min=0, max=20, value=4, 
                ticks=FALSE),
    # button to generate clusters
    actionButton("clustButton", "Generate clusters"),
    hr(),
    
    # select random sites
    # slider for how many cells to select from each bin
    sliderInput("size", label="Sample size per strata:", min=0, max=100, 
                value=25, step=5, ticks=FALSE),
    # button to select random sites
    actionButton("goButton", "Select random grids")

  ), # dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'fri',
              fluidRow(
                tabBox(
                  id = 'one', width = '12',
                  tabPanel('Mapview', tmapOutput('map1', height = 900)),
                  tabPanel('Clusters', DT::dataTableOutput('tab1')),
                  tabPanel('Similarity', DT::dataTableOutput('tab2'))
                ) # tabBox
              ) # fluidRow
      ) # tabItem
    ) # tabItems
  ) # dashboardBody
) # ui

server <- function(input, output, session) {
  
  # load factors, linear, areal, and grid into a single reactiveValues object; it
  #   is essentially a list that you can pass into function as a single argument
  data <- load.data()
  
  observe({
    # This populates the dropdown fields that are dependent on factors (now 
    # data$factors)
    update.inputs(input, session, data)
    # print('inputs updated')
  })

  # creates the clusters once the user hits the make clusters button
  observeEvent(input$clustButton, {
    data <- create.clusters(input, session, data)
  })

  # needs to be wrapped in observe() since data is a reactive object
  observe({
    data <- create.dta1(data)
  })

  # ditto about observe()
  observe({
    render.map1(input, output, session, data)
  })
  
  observe({
    update.transparency(input, session, data)
  })
  
  observe({
    render.tab1(output, data)  
  })
  
  observeEvent(input$goButton, {
    data <- strat.sample(input, data)
    map.selected.cells(input, output, session, data)
    render.tab2(output, session, data)
  })
}



#' server <- function(input, output) {
#'   #' Create reactiveValue: table of cells, separated into different bins based on
#'   #' kmeans clustering
#'   clusters <- eventReactive(input$clustButton, {
#'     print('clustbutton clicked, in eventreactive')
#'     shinybusy::add_busy_spinner()
#'     # replace NAs with 0s
#'     x <- factors %>%
#'       mutate(
#'              placer_pct=ifelse(is.na(placer_pct),0,placer_pct),
#'              quartz_pct=ifelse(is.na(quartz_pct),0,quartz_pct),
#'              recent_fires_pct=ifelse(is.na(recent_fires_pct),0,recent_fires_pct),
#'              area500_pct=ifelse(is.na(area500_pct),0,area500_pct),
#'              line500_pct=ifelse(is.na(line500_pct),0,line500_pct),
#'              merge100_pct=ifelse(is.na(merge100_pct),0,merge100_pct),
#'              water_pct=ifelse(is.na(water_pct),0,water_pct),
#'              forest_pct=ifelse(is.na(forest_pct),0,forest_pct),
#'              wetland_pct=ifelse(is.na(wetland_pct),0,wetland_pct)
#'             ) # mutate
#' 
#'     print('replaced NAs with 0')
#'   })
#'   # select only factors that user wants to cluster by  #   # st_drop_geometry() drops geom field from table (geom describes what type of  #   # feature it is and has some numbers describing it)  #   y <- select(x, unlist(input$factors)) %>% st_drop_geometry()  #   print('selected only selected factors')  #   # Now actually cluster cells  #   # kmeans() returns a list; $cluster object is a vector where the name is the  #   # cell number and the value is what cluster it's in  #   clust <- kmeans(scale(y), input$clusters)$cluster  #   print('kmeans')  #   # add field to x with what cluster the cell is in  #   x <- x %>%  #     mutate(clusters = clust)  #   print('mutate(clusters = clust)')  #  #   shinybusy::remove_modal_spinner()  # })
#'   # n1 is random sampling; ie same number is not sampled from each cluster
#'   n1 <- eventReactive(input$goButton, {
#'     print('goButton clicked')
#'     # sample from clusters() (defined above), number to be samples is number of
#'     #   samples per cluster * number of clusters
#'     n <- sample_n(clusters(), input$size * as.numeric(input$clusters))
#'   })
#' 
#'   # n2 is stratified random sampling; ie same number sampled from each cluster
#'   n2 <- eventReactive(input$goButton, {
#'     print('goButton clicked 2')
#'     n <- clusters() %>%
#'       group_by(clusters) %>%
#'       sample_n(size = input$size)
#'   })
#' 
#'   #### Map Output ----
#' 
#'   # output$map1 <- renderTmap({  #   print('in renderTmap')  #   # print(pal)  #   #   # Set color palette based on selected characteristics  #   if (input$inv == 'forest_pct') {  #     pal <- 'YlGn'  #   } else if (input$inv %in% c('water_pct','wetland_pct')) {  #     pal <- 'PuBu'  #   } else if (input$inv %in% c('quartz_pct','placer_pct')) {  #     pal <- 'Greys'  #   } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {  #     pal <- '-BrBG'  #   } else {  #     pal <- 'Reds'  #   }  #   #   # If the user clicks the button to generate clusters  #   if (input$clustButton) {  #     print('if(input$clustButton)')  #     m <- tm_shape(clusters()) + tm_fill('clusters', palette = 'Set1',  #                                       alpha = 1,  #                                       group = 'Clusters',  #                                       # zindex determines the order that elements  #                                       # appear on the page; an element with a  #                                       # higher z index appears in front of a lower  #                                       # zindex  #                                       zindex = 500)  #   } else {  #     m <- tm_shape(factors) + tm_fill(input$inv, palette = pal,  #                                      style = input$style,  #                                      alpha = 1,  #                                      colorNA = NULL,  #                                      group = input$inv,  #                                      title = input$inv,  #                                      zindex = 500)  #   }  #   #   # Base map  #   m <- m + tm_shape(grid) + tm_borders() +  #     tm_shape(linear) + tm_lines(col = 'black',  #                                 alpha = 1,  #                                 group = 'linfeat', zindex = 560) +  #     tm_shape(areal) + tm_fill(col = 'red',  #                               alpha = 1,  #                               group = 'arealfeat', zindex = 570)  #   #   print('added linear and areal features')  #   #   # add randomly selected cells  #   if (input$goButton) {  #     m <- m + tm_shape(n2()) + tm_borders(col = 'green', lwd = 2,  #                                          group = 'simp.random', zindex = 600) +  #              tm_shape(n2()) + tm_borders(col = 'yellow', lwd = 2,  #                                          group = 'strat.random', zindex = 600)  #   }  #   #   m <- m + tm_legend(position = c('right', 'top'), frame = T)  #   print('reached end of tmap rendering')  #   m  # }) # renderTmap
#'   # ignoreInit means that it isn't triggered when the app opens, only when the
#'   # user moves the slider
#'   observeEvent(input$alpha, ignoreInit = T, handlerExpr = {
#'     # Couldn't get pal to be within the scope of this function so I just copied
#'     # and pasted it
#'     if (input$inv == 'forest_pct') {
#'       pal <- 'YlGn'
#'     } else if (input$inv %in% c('water_pct','wetland_pct')) {
#'       pal <- 'PuBu'
#'     } else if (input$inv %in% c('quartz_pct','placer_pct')) {
#'       pal <- 'Greys'
#'     } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
#'       pal <- '-BrBG'
#'     } else {
#'       pal <- 'Reds'
#'     }
#' 
#'     if (input$clustButton > 0) {
#'       # create new layers for clusters/factors with alpha specified
#'       m1 <- tm_shape(clusters()) + tm_fill('clusters', palette = 'Set1',
#'                                               alpha = input$alpha,
#'                                               group = 'Clusters',
#'                                               zindex = 500)
#'     } else {
#'       m1 <- tm_shape(factors) + tm_fill(input$inv, palette = pal, style = input$style,
#'                                         alpha = input$alpha,
#'                                         colorNA = NULL,
#'                                         group = input$inv,
#'                                         title = input$inv,
#'                                         zindex = 500)
#' 
#'       m.linareal <- tm_shape(linear) + tm_lines(col = 'black',
#'                                                 alpha = input$alpha,
#'                                                 group = 'linfeat', zindex = 560) +
#'                     tm_shape(areal) + tm_fill(col = 'red',
#'                                               alpha = input$alpha,
#'                                               group = 'arealfeat', zindex = 570)
#'     }
#'     tmapProxy('map1',
#'               x = {tm_remove_layer(500) +
#'                    tm_remove_layer(560) +
#'                    tm_remove_layer(570) +
#'                    m.linareal +
#'                    m1
#'                   }
#'              ) # tmapProxy
#'   }) # observeEvent
#' 
#'   dta1 <- reactive({
#'     x <- clusters() %>% st_drop_geometry() %>%
#'       group_by(clusters) %>%
#'       summarize(n = n(),
#'                 merge100_pct = round(mean(merge100_pct),1),
#'                 recent_fires_pct = round(mean(recent_fires_pct),1),
#'                 #quartz_pct = round(mean(quartz_pct),1),
#'                 #placer_pct = round(mean(placer_pct),1),
#'                 elev_median=round(mean(elev_median),1),
#'                 elev_sd=round(mean(elev_sd),1),
#'                 forest_pct=round(mean(forest_pct),1),
#'                 water_pct=round(mean(water_pct),1),
#'                 wetland_pct=round(mean(wetland_pct),1),
#'       )
#'   }) # reactive
#' 
#'   output$tab1 <- DT::renderDataTable({
#'     x = dta1()
#'       datatable(x, rownames = F, options = list(dom = 'tip',
#'                                                 scrollX = T,
#'                                                 scrollY = T,
#'                                                 pageLength = 25),
#'                 class = 'compact')
#'   })
#' 
#'   output$tab2 <- DT::renderDataTable({
#'     print('in renderDataTable for tab2')
#'     simple <- tibble(type = 'simple', id = pull(n1(), id)) %>%
#'       left_join(factors)
#' 
#'     stratified <- tibble(type = 'stratified', id = pull(n2(), id)) %>%
#'       left_join(factors)
#' 
#'     ks_simple <-
#'       tibble(type = 'simple',
#'              merge100_pct = ks.test(factors$merge100_pct, simple$merge100_pct)[[1]],
#'              #merge500_pct = ks.test(factors$merge500_pct, simple$merge500_pct)[[1]],
#'              placer_pct = ks.test(factors$placer_pct, simple$placer_pct)[[1]],
#'              quartz_pct = ks.test(factors$quartz_pct, simple$quartz_pct)[[1]],
#'              recent_fires_pct = ks.test(factors$recent_fires_pct, simple$recent_fires_pct)[[1]],
#'              benchmark_pct = ks.test(factors$benchmark_pct, simple$benchmark_pct)[[1]],
#'              elev_median = ks.test(factors$elev_median, simple$elev_median)[[1]],
#'              elev_sd = ks.test(factors$elev_sd, simple$elev_sd)[[1]],
#'              forest_pct = ks.test(factors$forest_pct, simple$forest_pct)[[1]],
#'              wetland_pct = ks.test(factors$wetland_pct, simple$wetland_pct)[[1]],
#'              water_pct = ks.test(factors$water_pct, simple$water_pct)[[1]]
#'             )
#' 
#'     ks_stratified <-
#'       tibble(type="stratified",
#'              merge100_pct = ks.test(factors$merge100_pct, stratified$merge100_pct)[[1]],
#'              #merge500_pct = ks.test(factors$merge500_pct, stratified$merge500_pct)[[1]],
#'              placer_pct = ks.test(factors$placer_pct, stratified$placer_pct)[[1]],
#'              quartz_pct = ks.test(factors$quartz_pct, stratified$quartz_pct)[[1]],
#'              recent_fires_pct = ks.test(factors$recent_fires_pct, stratified$recent_fires_pct)[[1]],
#'              benchmark_pct = ks.test(factors$benchmark_pct, stratified$benchmark_pct)[[1]],
#'              elev_median = ks.test(factors$elev_median, stratified$elev_median)[[1]],
#'              elev_sd = ks.test(factors$elev_sd, stratified$elev_sd)[[1]],
#'              forest_pct = ks.test(factors$forest_pct, stratified$forest_pct)[[1]],
#'              wetland_pct = ks.test(factors$wetland_pct, stratified$wetland_pct)[[1]],
#'              water_pct = ks.test(factors$water_pct, stratified$water_pct)[[1]])
#'     View(ks_simple)
#'     View(ks_stratified)
#' 
#'     ks <- rbind(ks_simple, ks_stratified)
#' 
#'     print('finished renderDataTable for tab2')
#' 
#'     datatable(ks, rownames = F, options = list(dom = 'tip', scrollX = T,
#'                                                scrollY = T, pageLength = 25),
#'               class = 'compact') %>%
#'       formatRound(columns = c('merge100_pct', 'placer_pct', 'quartz_pct',
#'                               'recent_fires_pct', 'benchmark_pct', 'elev_median',
#'                               'elev_sd', 'forest_pct', 'wetland_pct', 'water_pct'),
#'                   digits = 3)
#' 
#' 
#'   })
#' }

shinyApp(ui, server)







