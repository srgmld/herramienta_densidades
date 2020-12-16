
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)

macros <- sf::st_read("input/mapas/macrodistrito_sur/macros.geojson") %>% 
  filter(!MACRO_VIGE %in% c("HAMPATURI", "ZONGO"))

fb_lp <- sf::st_read("input/mapas/fb_lp")
fb_ea <- sf::st_read("input/mapas/fb_ea")

manzanos_lp <- sf::st_read("input/mapas/manzanas_lp", stringsAsFactors = FALSE)
manzanos_lp <- sf::st_transform(manzanos_lp, 4326)
manzanos_lp$nombreciud <- "Nuestra Señora de La Paz"


ui <- fluidPage(
  fluidRow(
    column(4,
           selectInput('macro', 'Macrodistrito:', c(unique(macros$MACRO_VIGE)), selected = "SAN ANTONIO")),
    column(4,
           selectInput('eps', 'epsilon', choices = c(0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3), selected = 0.1)),
    column(4, 
           sliderInput("pts", label = "Min. Puntos:",
                       min = 10, max = 100, value = 20, step = 10))
  ),
  fluidRow(
    column(4,
           actionButton("simulate", "Simular!"))
  ),
  fluidRow(
    column(12,
           leafletOutput("map",width = "100%", height = 1400))
  )
)




server <- function(input, output, session) {

  
  manzanos_macro <- eventReactive(
    input$simulate,{
    st_intersection(st_buffer(manzanos_lp, dist = 0), 
                    st_buffer(macros %>% filter(MACRO_VIGE == input$macro), dist = 0))
  }) 
  
  
  fbcoord_macro <- eventReactive(
    input$simulate,{
    st_intersection(fb_lp, st_buffer(macros %>% filter(MACRO_VIGE == input$macro), dist = 0))
  })
  
  
  epsilon <- eventReactive(
    input$simulate,{
    input$eps
  })
 
  puntos <- eventReactive(
    input$simulate,{
    input$pts
  })
    

  
  output$map <- renderLeaflet({
    
    
    # fbcoord_macro <- st_intersection(fb_lp, st_buffer(macros %>% filter(MACRO_VIGE == input$macro), dist = 0))
    # 
    # locs = dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>%
    #                         as_tibble() %>% setNames(c("lon","lat"))),lon, lat)
    # 
    # locs.scaled = scale(locs,center = T,scale = T)
    # 
    # db = dbscan::dbscan(locs.scaled, eps= input$eps, minPts = input$pts)
    # 
    # 
    # fb_cluster <- cbind(locs, cluster = db$cluster)
    # 
    # categoria_pal <- colorFactor(c("#6a6e7a",colourvalues::colour_values(1:length(unique(db$cluster)),
    #                                                                      palette = colourvalues::get_palette("viridis")[70:256,])),
    #                              fb_cluster$cluster)
    # 
    # fb_cluster <- st_as_sf(fb_cluster, coords = c("lon","lat"), crs = 4326)
    
    
    # fbcoord_macro <- st_intersection(fb_lp, st_buffer(macros %>% filter(MACRO_VIGE == input$macro), dist = 0))
    
    

    categoria_pal <- colorFactor(c("#6a6e7a",colourvalues::colour_values(1:length(unique(dbscan::dbscan(scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro())) %>%
                                                                                                                               as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                                                                                              center = T,scale = T),
                                                                                                        eps= epsilon(), minPts = puntos()) %>% .$cluster)),
                                                                         palette = colourvalues::get_palette("viridis")[70:256,])),
                                 cbind(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro())) %>%
                                                        as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                       cluster = dbscan::dbscan(scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro())) %>%
                                                                                       as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                                                      center = T,scale = T),
                                                                eps= epsilon(), minPts = puntos()) %>% .$cluster) %>% .$cluster)
    
    fb_cluster <- st_as_sf(cbind(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro())) %>%
                                                  as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                 cluster = dbscan::dbscan(scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro())) %>%
                                                                                 as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                                                center = T,scale = T),
                                                          eps= epsilon(), minPts = puntos()) %>% .$cluster),
                           coords = c("lon","lat"), crs = 4326)
    
    leaflet(options = leafletOptions(minZoom =  5, maxZoom = 16)) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "mapa blanco") %>% 
      addProviderTiles('Esri.WorldImagery', group = "mapa satelital") %>%
      addCircles(data = fb_cluster, group = "Densidades",
                 color = ~categoria_pal(fb_cluster$cluster)) %>% 
      addPolygons(data = manzanos_macro(), group = "Manzanos",
                  stroke = T,
                  weight = 1, opacity = 0.8, fillOpacity = 0, color = "#141414"
      ) %>%
      addLayersControl(
        baseGroups = c("mapa blanco", "mapa satelital"),
        overlayGroups = c("Densidades", "Manzanos")
        ,
        options = layersControlOptions(collapsed = F))
    
    
  })
  
    
}

shinyApp(ui, server)






