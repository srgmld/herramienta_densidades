library(rgdal)
library(leaflet)
library(tidyverse)
library(magrittr)
library(leaflet.extras)
library(sf)
library(mapdeck)


# Facebook
# fb <- read.delim("input/mapas/fb_population_bol_2018-10-01.txt", sep = ",")
# fb_coord <- st_as_sf(fb, coords = c("longitude","latitude"), crs = 4326)

# # Conversion de shp a geojson
manzanos_lp <- sf::st_read("input/mapas/manzanas_lp", stringsAsFactors = FALSE)
manzanos_lp <- sf::st_transform(manzanos_lp, 4326)
manzanos_lp$nombreciud <- "Nuestra Señora de La Paz"
# a <- geojsonio::geojson_json(manzanos_lp)
# manzanos_lp %>% geojson::geo_write("input/mapas/manzanos_lp.geojson")

distritos_lp <- sf::st_read("input/mapas/macrodistrito_sur/distritos.geojson")
macros <- sf::st_read("input/mapas/macrodistrito_sur/macros.geojson")
distritos_ea <- sf::st_read("input/mapas/distritos_el_alto")


# # Coord FB LP & EA
# #------
# lp_ea <- bind_rows(macros %>% filter(!MACRO_VIGE %in% c("HAMPATURI", "ZONGO")), distritos_ea )
# fb_lpea <- st_intersection(fb_coord, st_buffer(lp_ea, dist = 0))
# fb_lpea <- fb_lpea[1:5]
# sf::st_write(fb_lpea,"input/mapas/fb_lp_ea/fb_lp_ea.shp")
# 
# fb_lpea <- sf::st_read("input/mapas/fb_lp_ea")
# 
# fb_lp <- st_intersection(fb_lpea, st_buffer(macros %>% filter(!MACRO_VIGE %in% c("HAMPATURI", "ZONGO")), dist = 0))
# sf::st_write(fb_lp,"input/mapas/fb_lp/fb_lp.shp")
# 
# fb_ea <- st_intersection(fb_lpea, st_buffer(distritos_ea, dist = 0))
# sf::st_write(fb_ea,"input/mapas/fb_ea/fb_ea.shp")
# 
# #-------

# #-------
# ## PRUEBAS
# labs_1 <- distritos_lp %>% filter(!MACRO_VIGE %in% c("HAMPATURI", "ZONGO")) 
# labs_1$geometry <- NULL
# 
# labs_1 <- lapply(seq(nrow(labs_1)), function(i) {
#   paste0("<b>Macrodistrito</b>", ': ', labs_1[i, "MACRO_VIGE"], "<br>"
#   ) 
# })
# 
# 
# leaflet() %>% 
#   addFullscreenControl() %>% 
#   addProviderTiles(providers$CartoDB.Positron, group = "Mapa claro") %>%
#   addProviderTiles('Esri.WorldImagery', group = "Mapa satelital") %>% 
#   addPolygons(data = macros %>% filter(!MACRO_VIGE %in% c("HAMPATURI", "ZONGO")), stroke = T,
#               label  = lapply(labs_1, htmltools::HTML)) %>%
#   addLayersControl(
#     baseGroups = c("Mapa claro", "Mapa satelital"),
#     options = layersControlOptions(collapsed = F)
#   ) 
# 
# 
#
# leaflet() %>%
#   addFullscreenControl() %>%
#   addProviderTiles(providers$CartoDB.Positron, group = "Mapa claro") %>%
#   addProviderTiles('Esri.WorldImagery', group = "Mapa satelital") %>%
#   addCircles(a) %>%
#   addLayersControl(
#     baseGroups = c("Mapa claro", "Mapa satelital"),
#     options = layersControlOptions(collapsed = F)
#   )
# 
# 
# #---------


fb_lp <- sf::st_read("input/mapas/fb_lp")
fb_ea <- sf::st_read("input/mapas/fb_ea")

unique(macros$MACRO_VIGE)


manzanos_macro <- st_intersection(st_buffer(manzanos_lp, dist = 0), 
                                  st_buffer(macros %>% filter(MACRO_VIGE == "SAN ANTONIO"), dist = 0))
fbcoord_macro <- st_intersection(fb_lp, st_buffer(macros %>% filter(MACRO_VIGE == "SAN ANTONIO"), dist = 0))



#Ploting DBSCAN results.
factoextra::fviz_cluster(dbscan::dbscan(scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                                                               as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                              center = T,scale = T),
                                        eps= 0.1, minPts = 20),
                         scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                                                   as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                  center = T,scale = T),
                         stand = F,ellipse = T,geom = "point")



categoria_pal <- colorFactor(c("#6a6e7a",colourvalues::colour_values(1:length(unique(db$cluster)), 
                                                                     palette = colourvalues::get_palette("viridis")[70:256,])),
                             cbind(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                                                    as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                   cluster = dbscan::dbscan(scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                                                                                   as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                                                  center = T,scale = T),
                                                            eps= 0.1, minPts = 20) %>% .$cluster) %>% .$cluster)


fb_cluster <- st_as_sf(cbind(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                                              as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                             cluster = dbscan::dbscan(scale(dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                                                                             as_tibble() %>% setNames(c("lon","lat"))),lon, lat),
                                                            center = T,scale = T),
                                                      eps= 0.1, minPts = 20) %>% .$cluster),
                       coords = c("lon","lat"), crs = 4326)

head(fb_cluster,5)

leaflet(options = leafletOptions(minZoom =  5, maxZoom = 16)) %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "mapa blanco") %>% 
  addProviderTiles('Esri.WorldImagery', group = "mapa satelital") %>%
  addCircles(data = fb_cluster, group = "Densidades",
             color = ~categoria_pal(fb_cluster$cluster)) %>% 
  addPolygons(data = manzanos_macro, group = "Manzanos",
              stroke = T,
              weight = 1, opacity = 0.8, fillOpacity = 0, color = "#141414"
              ) %>%
  addLayersControl(
    baseGroups = c("mapa blanco", "mapa satelital"),
    overlayGroups = c("Densidades", "Manzanos")
    ,
    options = layersControlOptions(collapsed = F))


# prueba1 %>% htmlwidgets::saveWidget(here::here("output/mapas/prueba_SanAntonio.html"))


# CALACOTO

manzanos_macro <- st_intersection(st_buffer(manzanos_lp, dist = 0), 
                                  st_buffer(macros %>% filter(MACRO_VIGE == "CALACOTO"), dist = 0))
fbcoord_macro <- st_intersection(fb_lp, st_buffer(macros %>% filter(MACRO_VIGE == "CALACOTO"), dist = 0))


locs = dplyr::select((do.call(rbind, st_geometry(fbcoord_macro)) %>% 
                        as_tibble() %>% setNames(c("lon","lat"))),lon, lat)

locs.scaled = scale(locs,center = T,scale = T)

db = dbscan::dbscan(locs.scaled, eps= 0.1, minPts = 50)


#Ploting DBSCAN results.
factoextra::fviz_cluster(db,locs.scaled, stand = F,ellipse = T,geom = "point")


fb_cluster <- cbind(locs, cluster = db$cluster)

categoria_pal <- colorFactor(c("#6a6e7a",colourvalues::colour_values(1:length(unique(db$cluster)), 
                                                                     palette = colourvalues::get_palette("viridis")[70:256,])),
                             fb_cluster$cluster)


fb_cluster <- st_as_sf(fb_cluster, coords = c("lon","lat"), crs = 4326)

head(fb_cluster,5)

prueba2 <- leaflet(options = leafletOptions(minZoom =  5, maxZoom = 16)) %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "mapa blanco") %>% 
  addProviderTiles('Esri.WorldImagery', group = "mapa satelital") %>%
  addCircles(data = fb_cluster, group = "Densidades",
             color = ~categoria_pal(fb_cluster$cluster)) %>% 
  addPolygons(data = manzanos_macro, group = "Manzanos",
              stroke = T,
              weight = 1, opacity = 0.8, fillOpacity = 0, color = "#141414"
  ) %>%
  addLayersControl(
    baseGroups = c("mapa blanco", "mapa satelital"),
    overlayGroups = c("Densidades", "Manzanos")
    ,
    options = layersControlOptions(collapsed = F))

prueba2 %>% htmlwidgets::saveWidget(here::here("output/mapas/prueba_Calacoto.html"))
