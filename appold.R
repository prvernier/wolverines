## Load Required Libraries
library(shiny)
library(sf)
library(DT)
library(tmap)
library(dplyr)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinybusy)

tmap_mode("view")
tmap_options(check.and.fix = TRUE)
tmap_options(basemaps = c(Esri.WorldImagery="Esri.WorldImagery", Esri.NatGeoWorldMap="Esri.NatGeoWorldMap"))

## Load in data
grid <- st_read("data/wolverines.gpkg", "grid5k", quiet=T)
linear <- st_read("data/wolverines.gpkg", "linear_features", quiet=T)
areal <- st_read("data/wolverines.gpkg", "areal_features", quiet=T)
factors <- st_read("data/wolverines.gpkg", "survey_factors", quiet=T)

ui = dashboardPage(
  dashboardHeader(title = "Wolverines Survey"),
  dashboardSidebar(
    
    sidebarMenu(
        # fri = forest resources inventory
        menuItem("Survey factors", tabName = "fri", icon = icon("th"))
    ),
    # Select feature type (merge_100, percent_forest, etc) to view
    selectInput("inv", label = "Feature type:", choices = names(factors)[3:22], selected='merge100_pct'),
    # Style for binning cells for display
    selectInput("style", label="Select style:", choices=c("jenks","kmeans","quantile","pretty","equal"), selected="quantile"),
    # Set transparency
    sliderInput("alpha", label="Transparency:", min=0, max=1, value=1, step=0.1, ticks=FALSE),
    hr(),
    # features to cluster by
    selectInput("factors", label = "Select features:", multiple=TRUE, choices = names(factors)[3:22], selected=c('merge100_pct','elev_median','elev_sd','forest_pct','water_pct')),
    # how many clusters
    sliderInput("clusters", label="Number of clusters:", min=0, max=20, value=4, ticks=FALSE),
    # button to generate clusters
    actionButton("clustButton", "Generate clusters"),
    hr(),
    # select random sites
    # slider for how many cells to select from each bin
    sliderInput("size", label="Sample size per strata:", min=0, max=100, value=25, step=5, ticks=FALSE),
    # button to select random sites
    actionButton("goButton", "Select random grids")
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName="fri",
            fluidRow(
                tabBox(
                    id = "one", width="12",
                    tabPanel("Mapview", leafletOutput("map1", height=900)),
                    tabPanel("Clusters", DT::dataTableOutput("tab1")),#pre(includeText("bc10.txt")))
                    tabPanel("Similarity", DT::dataTableOutput("tab2"))#pre(includeText("bc10.txt")))
                )
            )
        )
    )
  )
)

server = function(input, output) {

    #' Create reactiveValue: table of cells, separated into different bins based 
    #'   kmeans clustering
    clusters <- eventReactive(input$clustButton, {
        print('in clusters eventReactive')
        # replace NAs with 0s
        x <- mutate(factors,
            placer_pct=ifelse(is.na(placer_pct),0,placer_pct),
            quartz_pct=ifelse(is.na(quartz_pct),0,quartz_pct),
            recent_fires_pct=ifelse(is.na(recent_fires_pct),0,recent_fires_pct),
            area500_pct=ifelse(is.na(area500_pct),0,area500_pct),
            line500_pct=ifelse(is.na(line500_pct),0,line500_pct),
            merge100_pct=ifelse(is.na(merge100_pct),0,merge100_pct),
            water_pct=ifelse(is.na(water_pct),0,water_pct),
            forest_pct=ifelse(is.na(forest_pct),0,forest_pct),
            wetland_pct=ifelse(is.na(wetland_pct),0,wetland_pct))
        #clust <- kmeans(scale(cbind(x$merge100_pct,x$elev_median,x$elev_sd,x$forest_pct)),input$clusters)$cluster
        #' select only factors that user wants to cluster by; not sure what 
        #' st_drop_geometry() does
        y <-  select(x, unlist(input$factors)) %>% st_drop_geometry()
        #' Now actually cluster cells
        #' kmeans() returns a list; $cluster object is a vector where the name 
        #'   is the cell number and the value is what cluster it's in
        clust <- kmeans(scale(y),input$clusters)$cluster
        #' add field to x with what cluster the cell is in
        x <- x %>% 
          mutate(clusters=clust)
        print('finished clusters eventReactive')
    })
    
    # sample cells
    n1 <- eventReactive(input$goButton, {
      print('in n1 eventreactive')
      #' sample from clusters() (defined above), number to be sampled is number 
      #' of samples per cluster * number of clusters
      n <- sample_n(clusters(), input$size*as.numeric(input$clusters))
      
      #nn <- tibble(type="simple", id=pull(n, id))
      #readr::write_csv(nn, paste0('samples/simple_random_c',input$clusters,'_n',input$size,'.csv'))
      #n
    })
    
    # also sample cells
    n2 <- eventReactive(input$goButton, {
      print('in n2 eventreactive')
      n <- clusters() %>%
        group_by(clusters) %>% 
        sample_n(size=input$size)
      #nn <- tibble(type="stratified", id=pull(n, id))
      #readr::write_csv(nn, paste0('samples/stratified_random_c',input$clusters,'_n',input$size,'.csv'))
      #n
    })
    
    output$map1 <- renderLeaflet({
      print('rendering leaflet')
        #' Select color palettes based on what environmental/disturbance layer
        #' the user wants to look at
         
        shinybusy::show_modal_spinner()
        if (input$inv=='forest_pct') {
            pal='YlGn'
        } else if (input$inv %in% c('water_pct','wetland_pct')) {
            pal='PuBu'
        } else if (input$inv %in% c('quartz_pct','placer_pct')) {
            pal='Greys'
        } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
            pal='-BrBG'
        } else {
            pal='Reds'
        }
      
        #' If the user clicks the button to generate clusters 
        if (input$clustButton) {
          # print('in if(input$clustButton)')
            m <- tm_shape(clusters()) + tm_fill('clusters', palette='Set1', 
                                                alpha=1,
                                                group="Clusters")
        } else {
          # print('in else statement')
            m <- tm_shape(factors) + tm_fill(input$inv, palette=pal, style=input$style, 
                                             alpha=1, 
                                             colorNA=NULL, group=input$inv, title=input$inv)
        }
      
        # base map
        # print('creating map')
        m <- m + tm_shape(grid) + tm_borders() +
            tm_shape(linear) + tm_lines(col='black', 
                                        alpha=1,
                                        group="Linear features") +
            tm_shape(areal) + tm_fill(col='red', 
                                      alpha=1,
                                      group="Areal features")
        # print('added tm stuff to map')
        if (input$goButton) {
            m <- m + tm_shape(n1()) + tm_borders(col='green', lwd=2, group="Simple random") +
                tm_shape(n2()) + tm_borders(col='yellow', lwd=2, group="Stratified random")
        }
        m <- m + tm_legend(position = c("right", "top"), frame=TRUE)
        # print('added legend')
        lf <- tmap_leaflet(m)
        # print('tmap_leaflet called line 156ish')
        shinybusy::remove_modal_spinner()
        # lf %>% leaflet::hideGroup(c("Areal disturbances","Linear disturbances",
        #                             "Areal features","Linear features","Elevation",
        #                             "Simple random"))
        lf
    })
    
    observeEvent(input$alpha, {
      leafletProxy('map1') +
        tm_shape(grid) + 
        tm_borders() + 
        tm_shape(factors, alpha = input$alpha) + 
        tm_fill(alpha=input$alpha) +
        tm_lines(col='black', alpha=input$alpha, group="Linear features") +
        tm_shape(areal) + 
        tm_fill(col='red', alpha=input$alpha, group="Areal features")
    })

    dta1 <- reactive({
      # print('in reactive dta1')
        x <- clusters() %>% st_drop_geometry() %>%
            #select(merge100_pct,elev_median,elev_sd,forest_pct,clusters) %>%
            group_by(clusters) %>%
            summarize(n=n(),
                merge100_pct=round(mean(merge100_pct),1),
                recent_fires_pct=round(mean(recent_fires_pct),1),
                #quartz_pct=round(mean(quartz_pct),1),
                #placer_pct=round(mean(placer_pct),1),
                elev_median=round(mean(elev_median),1),
                elev_sd=round(mean(elev_sd),1),
                forest_pct=round(mean(forest_pct),1),
                water_pct=round(mean(water_pct),1),
                wetland_pct=round(mean(wetland_pct),1),
                )
        #x2 = as_tibble(x1) %>% slice(1) %>% unlist(., use.names=FALSE)
        #x = bind_cols(Attribute=names(x1), Value=x2)
    })

	output$tab1 <- DT::renderDataTable({
        x = dta1() #%>% filter(!Attribute %in% casVars)
		datatable(x, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact")
    })

    #dta2 <- reactive({
    #})

    output$tab2 <- DT::renderDataTable({
        simple <- tibble(type="simple", id=pull(n1(), id)) %>%
            left_join(factors)

        stratified <- tibble(type="stratified", id=pull(n2(), id)) %>%
            left_join(factors)

        ks_simple <- tibble(type="simple",
            merge100_pct = ks.test(factors$merge100_pct, simple$merge100_pct)[[1]],
            #merge500_pct = ks.test(factors$merge500_pct, simple$merge500_pct)[[1]],
            placer_pct = ks.test(factors$placer_pct, simple$placer_pct)[[1]],
            quartz_pct = ks.test(factors$quartz_pct, simple$quartz_pct)[[1]],
            recent_fires_pct = ks.test(factors$recent_fires_pct, simple$recent_fires_pct)[[1]],
            benchmark_pct = ks.test(factors$benchmark_pct, simple$benchmark_pct)[[1]],
            elev_median = ks.test(factors$elev_median, simple$elev_median)[[1]],
            elev_sd = ks.test(factors$elev_sd, simple$elev_sd)[[1]],
            forest_pct = ks.test(factors$forest_pct, simple$forest_pct)[[1]],
            wetland_pct = ks.test(factors$wetland_pct, simple$wetland_pct)[[1]],
            water_pct = ks.test(factors$water_pct, simple$water_pct)[[1]])

        ks_stratified <- tibble(type="stratified",
            merge100_pct = ks.test(factors$merge100_pct, stratified$merge100_pct)[[1]],
            #merge500_pct = ks.test(factors$merge500_pct, stratified$merge500_pct)[[1]],
            placer_pct = ks.test(factors$placer_pct, stratified$placer_pct)[[1]],
            quartz_pct = ks.test(factors$quartz_pct, stratified$quartz_pct)[[1]],
            recent_fires_pct = ks.test(factors$recent_fires_pct, stratified$recent_fires_pct)[[1]],
            benchmark_pct = ks.test(factors$benchmark_pct, stratified$benchmark_pct)[[1]],
            elev_median = ks.test(factors$elev_median, stratified$elev_median)[[1]],
            elev_sd = ks.test(factors$elev_sd, stratified$elev_sd)[[1]],
            forest_pct = ks.test(factors$forest_pct, stratified$forest_pct)[[1]],
            wetland_pct = ks.test(factors$wetland_pct, stratified$wetland_pct)[[1]],
            water_pct = ks.test(factors$water_pct, stratified$water_pct)[[1]])
        
        ks <- rbind(ks_simple, ks_stratified)

        datatable(ks, rownames=F, options=list(dom = 'tip', scrollX = TRUE, scrollY = TRUE, pageLength = 25), class="compact") %>%
            formatRound(columns=c('merge100_pct','placer_pct','quartz_pct','recent_fires_pct','benchmark_pct','elev_median','elev_sd',
                'forest_pct','wetland_pct','water_pct'), digits=3)
    })

}
shinyApp(ui, server)