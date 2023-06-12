# load packages
pacman::p_load(tidyverse,sf,gtfstools,tidygraph,lubridate,hms,igraph,mapview, osmdata,reticulate,gganimate,sfnetworks,lwgeom,furrr)
# load gtfs
gtfs <- read_gtfs("C:/Users/idshk/Downloads/gtfs.zip",encoding = "UTF-8")
# parse stop times at 15 seconds
gtfs$stop_times$departure_time <- gtfs$stop_times$departure_time %>%parse_hms() %>%  round_hms(15) %>% as.numeric()
gtfs$stop_times$departure_time <- gtfs$stop_times$departure_time/15
# keep pnly 11.7.22 trips
trips <- gtfs$trips$trip_id %>% str_split("_") %>% 
  keep(~.x[2]=="110722") %>% 
  map(~paste0(.x[1],"_",.x[2])) %>% 
  unlist()
# filter gtfs
gtfs2 <- gtfs %>% 
  filter_by_weekday(weekday = "monday") %>% 
  filter_by_trip_id(trips)
tlv <-  opq_osm_id(1400916, "rel") %>% 
  opq_string () %>%
  osmdata_sf ()
not_intersets  = function(x, y) !st_intersects(x, y)
stops <- gtfs2$stops %>% st_as_sf(coords = c("stop_lon","stop_lat"),crs = 4326) %>% 
  st_filter(tlv$osm_multipolygons ,.predicate =  not_intersets) %>% 
  pull(stop_id)
gtfs3 <- gtfs2 %>% 
  filter_by_stop_id(stops,keep = F)

# stops to stop
phase_1 <- tibble(from = gtfs3$stops$stop_code,
                  to = gtfs3$stops$stop_code,
                  period = 0)
# route
phase_21 <- gtfs3$stop_times %>% 
  group_by(trip_id) %>%
  filter(!any(is.na(departure_time ))) %>% 
  summarise(tick = seq(min(departure_time),max(departure_time),by = 1)) %>% 
  left_join(gtfs3$trips %>% select(trip_id,route_id)) %>% 
  left_join(gtfs3$stop_times %>% select(trip_id,departure_time,stop_id),by = c("trip_id","tick" = "departure_time" )) %>% 
  left_join(gtfs3$stops %>% select(stop_id,stop_code), by = "stop_id") %>% 
  mutate(canon_tick = tick - min(tick) + 1,
         tran_id = ifelse(!is.na(stop_code),stop_code,paste0(route_id,"_",canon_tick))) %>% 
  ungroup() 

phase_22 <- phase_21 %>% 
  distinct(route_id,canon_tick,tran_id) %>% 
  group_by(route_id) %>% 
  mutate(to = lead(tran_id)) %>% 
  rename(from = tran_id)
phase_23 <- phase_22 %>% 
  left_join(phase_21 %>% select(tran_id,route_id,tick),by = c("from" = "tran_id","route_id")) %>% 
  ungroup() %>% 
  select(from, to ,period = tick) %>% 
  filter(!is.na(to))
phase_3 <- bind_rows(phase_1,phase_23)
net <- as_tbl_graph(phase_3)
net1 <- net %>% 
  filter(net %>% components() %>% `$`(membership) %>% `==`(1))

# use_condaenv("ox")
# ox <- import("osmnx")
# tlv <- ox$graph_from_place("Tel Aviv District", network_type = "walk")
# ox$save_graph_geopackage(tlv,filepath = "mahoz_tlv.gpkg")
tlv_net <- st_read("C:/Users/idshk/OneDrive/Documents/mahoz_tlv.gpkg",layer="edges")
step3 <- tlv_net %>% mutate(period = 0,rn = row_number()) %>% select(period,rn) %>% st_transform(2039)
step3seg <- step3%>%st_segmentize(15)
q_segments <- function(step3,step3seg,row) {
  q1 <- step3  %>% slice(row)
  q2 <- step3seg  %>% slice(row)
  df1 <- q1 %>% st_coordinates() %>% as.data.frame() %>% select(-3)
  df2 <- q2 %>% st_coordinates()%>% as.data.frame()%>% select(-3)
  step <- df2 %>% 
    mutate(new = do.call(paste0, df2) %in% do.call(paste0, df1),
           cs = cumsum(!new)+1,
           cs1 = lag(cs)) 
  st_sf(st_sfc(map(1:max(step$cs),~step %>% filter(cs == .x | cs1 ==.x) %>% select(1,2) %>% as.matrix() %>% st_linestring())))
}
a <- Sys.time()
plan(multisession, workers = 11)
seg <- future_map_dfr(1:nrow(step3),~q_segments(step3,step3seg,.x)) 
Sys.time() - a
seg %>% as_sfnetwork(directed = F)
# step3 %>% mutate(st_length(geom))
# pts <- step3 %>% 
#   st_line_sample(density = 1/15) %>% 
#   st_sf()  %>% 
#   mutate(rn = row_number())
# map2(step3[2,],pts[2,],~ifelse(st_is_empty(.y$geometry),NA,st_split(.x,.y)))
# map2(step3[3,"geom"],pts[3,"geometry"] ,st_split) %>%View() `[[`(1)%>% st_collection_extract("LINESTRING")
# step4 <- st_split(step3,pts)
# 
# tlv_net %>% 
#   st_geod_segmentize(units::set_units(15,m))
#   # st_transform(2039) %>%
#   # slice(3) %>% 
#   # st_segmentize(15) %>% 
#   # mutate(q = st_length(geom)) %>% 
#   # st_transform(4326)
# tlv_net1 <- tlv_net %>% 
#   as_sfnetwork(directed = F) 
  

nams <- net1 %>% 
  as_tibble() %>% 
  pull(name) 
get_id <- function(names){
  which(nams%in% names)
}
get_id(c("22946"))

get_next <- function(node_name,tick){
  net1 %E>% 
    filter(period == 0|period == tick) %N>% 
    filter(node_is_adjacent(get_id(node_name),mode = "out")) %>% 
    as_tibble() %>%
    pull(name) 
}
get_next(c("22946"),1805)
# get_next2 <- function(node_name,tick){
#   net1 %E>% 
#     filter(edge_is_from(get_id(node_name))) %>% 
#     filter(period == 0|period == tick) %N>% 
#     # filter(node_is_adjacent(get_id(node_name),mode = "out")) %>% 
#     as_tibble() %>%
#     pull(name) 
# }
# get_next2(c("1523"),1805)


start <- 1800
id <- "22946"
ticks_num <- 180
ticks <- vector("list", ticks_num)
ticks[[1]] <- id
used_ids <- character()
break_ids <- character()
for(i in 2:(ticks_num)){
  used_ids <-  break_ids[!duplicated(break_ids)]
  break_ids <- unlist(ticks)[unlist(ticks) %>% str_detect("_") ]
  head_of_pt <- ticks[[i-1]][!ticks[[i-1]] %in% used_ids]
  ticks[[i]] <- get_next(head_of_pt,start +i)
  print(i)
  # print(break_ids)
  # print(used_ids)
  print(head_of_pt)
  # print(ticks[[i]])
}

# profvis({for(i in 2:(ticks_num)){
#   used_ids <-  break_ids[!duplicated(break_ids)]
#   break_ids <- unlist(ticks)[unlist(ticks) %>% str_detect("_") ]
#   head_of_pt <- ticks[[i-1]][!ticks[[i-1]] %in% used_ids]
#   ticks[[i]] <- get_next(head_of_pt,start +i)
#   print(i)
#   # print(break_ids)
#   # print(used_ids)
#   print(head_of_pt)
#   # print(ticks[[i]])
# }
# })
#  
data.frame(nstops = map_dbl(ticks,~.x[.x %>% str_detect("_",negate = T)] %>% length)) %>% 
  mutate(rn = row_number()) %>% 
  ggplot(aes(x=rn/4,y=nstops)) + 
  geom_line()

qq <- (ticks %>% unlist())[ticks %>% unlist() %>% str_detect("_",negate = T)] %>% unique()
ticks %>% 
  imap_dfr(~data.frame(stop_code = .x) %>% mutate(tick = .y)) %>% 
  filter(!str_detect(stop_code,"_")) %>% 
  left_join(gtfs3$stops) %>% 
  st_as_sf(coords = c("stop_lon","stop_lat"),crs = 4326) %>% 
  ggplot() + 
  geom_sf() + 
  transition_manual(tick)
  
gtfs3$stops %>% 
  filter(stop_code %in%qq) %>%
  st_as_sf(coords = c("stop_lon","stop_lat"),crs = 4326) %>% 
  mapview()

