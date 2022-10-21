library(sf)
library(DT)
library(tmap)
library(dplyr)
library(leaflet)
library(tidyverse)
library(shinydashboard)

tmap_mode("view")
tmap_options(check.and.fix = TRUE)
tmap_options(basemaps = c(Esri.WorldImagery="Esri.WorldImagery", Esri.NatGeoWorldMap="Esri.NatGeoWorldMap"))
grid <- st_read("data/wolverines.gpkg", "grid5k", quiet=T)
linear <- st_read("data/wolverines.gpkg", "linear_features", quiet=T)
areal <- st_read("data/wolverines.gpkg", "areal_features", quiet=T)
factors <- st_read("data/wolverines.gpkg", "survey_factors", quiet=T)

ui = dashboardPage(
  dashboardHeader(title = "Wolverines Survey"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("Survey factors", tabName = "fri", icon = icon("th"))
    ),
    selectInput("inv", label = "Feature type:", choices = names(factors)[3:22], selected='merge100_pct'),
    selectInput("style", label="Select style:", choices=c("jenks","kmeans","quantile","pretty","equal"), selected="quantile"),
    sliderInput("alpha", label="Transparency:", min=0, max=1, value=1, step=0.1, ticks=FALSE),
    hr(),
    selectInput("factors", label = "Select features:", multiple=TRUE, choices = names(factors)[3:22], selected=c('merge100_pct','elev_median','elev_sd','forest_pct','water_pct')),
    sliderInput("clusters", label="Number of clusters:", min=0, max=20, value=3, ticks=FALSE),
    actionButton("clustButton", "Generate clusters"),
    hr(),
    sliderInput("size", label="Sample size per strata:", min=0, max=100, value=10, step=5, ticks=FALSE),
    actionButton("goButton", "Select random grids")
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName="fri",
            fluidRow(
                tabBox(
                    id = "one", width="12",
                    tabPanel("Mapview", leafletOutput("map1", height=900)),
                    tabPanel("Clusters", DT::dataTableOutput("tab1"))#pre(includeText("bc10.txt")))
                )
            )
        )
    )
  )
)

server = function(input, output) {

    n1 <- eventReactive(input$goButton, {
        n = sample_n(clusters(), input$size*as.numeric(input$clusters))
        nn <- tibble(type="simple", id=pull(n, id))
        readr::write_csv(nn, paste0('samples/simple_random_c',input$clusters,'_n',input$size,'.csv'))
        n
    })

    n2 <- eventReactive(input$goButton, {
        n <- group_by(clusters(), clusters) %>% sample_n(size=input$size)
        nn <- tibble(type="stratified", id=pull(n, id))
        readr::write_csv(nn, paste0('samples/stratified_random_c',input$clusters,'_n',input$size,'.csv'))
        n
    })

    clusters <- eventReactive(input$clustButton, {
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
        y = select(x, unlist(input$factors)) %>% st_drop_geometry()
        clust <- kmeans(scale(y),input$clusters)$cluster
        x <- mutate(x, clusters=clust)
    })

    output$map1 <- renderLeaflet({
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
        if (input$clustButton) {
            m <- tm_shape(clusters()) + tm_fill('clusters', palette='Set1', alpha=input$alpha, group="Clusters")
        } else {
            m <- tm_shape(factors) + tm_fill(input$inv, palette=pal, style=input$style, alpha=input$alpha, colorNA=NULL, group=input$inv, title=input$inv)
        }
        m <- m + tm_shape(grid) + tm_borders() +
            tm_shape(linear) + tm_lines(col='black', alpha=input$alpha, group="Linear features") +
            tm_shape(areal) + tm_fill(col='red', alpha=input$alpha, group="Areal features")
        if (input$goButton) {
            m <- m + tm_shape(n1()) + tm_borders(col='green', lwd=2, group="Simple random") +
                tm_shape(n2()) + tm_borders(col='yellow', lwd=2, group="Stratified random")
        }
        m <- m + tm_legend(position = c("right", "top"), frame=TRUE)
        lf <- tmap_leaflet(m)
        lf %>% leaflet::hideGroup(c("Areal disturbances","Linear disturbances","Areal features","Linear features","Elevation","Simple random"))
    })

    dta1 <- reactive({
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

}
shinyApp(ui, server)