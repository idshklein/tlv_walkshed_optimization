library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)
library(gtfstools)
shp <- st_read("tlvdrive.gpkg",layer = "edges")
gtfs <- read_gtfs("C:/Users/yehuda/Downloads/gtfs.zip",encoding = "UTF-8")
pol <- shp %>% 
  st_transform(2039) %>% 
  st_union() %>% 
  st_buffer(200)
tlv_stops <- gtfs$stops %>% 
  st_as_sf(coords = c("stop_lon","stop_lat"),crs = 4326) %>% 
  st_transform(2039) %>% 
  st_filter(pol)

pts <- gtfs %>% 
  filter_by_stop_id(tlv_stops$stop_id) %>% 
  filter_by_stop_id(gtfs$stops %>% filter(!stop_id %in%tlv_stops$stop_id) %>% pull(stop_id),keep = FALSE) %>% 
  `$`(stops) %>% 
  st_as_sf(coords = c("stop_lon","stop_lat"),crs = 4326) 
tlv_net <- shp %>% 
  filter(oneway == FALSE) %>% 
  st_reverse() %>% 
  bind_rows(shp) %>% 
  as_sfnetwork(directed = T)

od <- tlv_net %>% 
  st_network_blend(pts) %>% 
  mutate(rn = row_number()) %>% 
  {
    (.) %>% 
      filter(!is.na(stop_id)) %>% 
      as_tibble() %>% 
      pull(rn)
  } 
  
tlv_net %>% 
  st_network_blend(pts) %>% 
  st_network_paths(pts,pts) %>% View()
  convert(to_spatial_shortest_paths)
