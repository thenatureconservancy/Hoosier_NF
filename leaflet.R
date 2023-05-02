

## make and save leaflet for dash to try...
# have had issues with deployment and finding reports


# load packages
library(leaflet)
library(sf)
library(tidyverse)

# read and prep data
shp <- st_read("data/hoosier_ltas_dissolved.shp") %>% 
  st_transform(crs = 4326) %>%
  arrange(MAP_UNIT_S)

paths <- list.files('reports/', pattern = '.html', all.files = TRUE, full.names = TRUE) %>%
  sort()

shp <- cbind(shp, paths)

# make leaflet

leaflet(shp) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  setView(-86.468321, 38.556166, zoom = 8.2) %>%
  addPolygons(color = "#404241", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#0b631a",
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = TRUE),
              popup = paste0( "LTA:"
                              , shp$MAP_UNIT_S
                              , "<br>"
                              , "<a href='"
                              , shp$paths
                              , "' target='_blank'>"
                              , "For LTA report click Here</a>"
              )
  )
