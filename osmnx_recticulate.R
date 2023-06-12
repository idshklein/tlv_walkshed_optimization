library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)
library(mapview)
library(reticulate)
library(randomcoloR)
use_condaenv("ox")
ox <- import("osmnx")
tlv <- ox$graph_from_place("tel aviv", network_type = "walk")
ox$save_graph_geopackage(tlv,filepath = "tlv.gpkg")
st_read("tlv.gpkg",layer = "edges") %>% 
  as_sfnetwork() %>% 
  convert(to_undirected) %>% 
  mutate(group = group_louvain()) %>% 
  st_as_sf() %>% 
  mapview(zcol = "group",col.regions = randomColor(1000))


tlvdrive <- ox$graph_from_place("tel aviv", network_type = "drive")
ox$save_graph_geopackage(tlvdrive,filepath = "tlvdrive.gpkg")
st_read("tlvdrive.gpkg",layer = "edges") %>% 
  as_sfnetwork() %>% 
  convert(to_undirected) %>% 
  mutate(group = group_louvain()) %>% 
  st_as_sf() %>% 
  mapview(zcol = "group",col.regions = randomColor(1000))

tlvall <- ox$graph_from_place("tel aviv", network_type = "all")
ox$save_graph_geopackage(tlvall,filepath = "tlvall.gpkg")

mapviewOptions(vector.palette = randomColor(1000))
mapviewGetOption("vector.palette")
netwalk <- st_read("tlv.gpkg",layer = "edges") %>% as_sfnetwork() 
netdrive <- st_read("tlvdrive.gpkg",layer = "edges") %>% as_sfnetwork() 
bind_rows(netwalk %E>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  select(-from,-to,name ),
netdrive %E>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  select(-from,-to,name )
) %>% 
  distinct()
  arrange(-n)
  

net %>% 
  convert(to_undirected) %>% 
  mutate(group = as.factor(group_louvain(edge_length()^(-1)))) %>%
  # convert(to_spatial_contracted,group,simplify = TRUE) %>% 
  # mutate(group = group_louvain()) %>%
  {
    groups = (.) %>% as_tibble() %>% pull(group) %>% unique()
    net1 <- (.)
    map_dbl(groups,~ net1 %>%
              filter(group == .x) %>%
              st_network_cost() %>%
              apply(1,max) %>%
              min())
  } %>% hist()
  # st_as_sf() %>%
  # mapview(zcol = "group")
  autoplot()
net %E>% 
  st_as_sf() %>% 
  pull(highway) %>% 
  unique()

