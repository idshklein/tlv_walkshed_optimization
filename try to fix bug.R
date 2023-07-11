pacman::p_load(tidyverse,sf,sfnetworks,tidygraph,mapview,reticulate,randomcoloR,jsonlite,igraph,lwgeom,ggspatial,osmdata,patchwork,nngeo)
# create classification of links, whether they are drivable or walkable
types <- structure(list(type = c("residential", "pedestrian", "secondary", 
                                 "tertiary", "primary", "trunk_link", "trunk", "footway", "tertiary_link", 
                                 "service", "steps", "unclassified", "path", "living_street", 
                                 "primary_link", "track", "secondary_link", "road", "bridleway", 
                                 "bus_guideway", "bus_stop", "busway", "construction", "corridor", 
                                 "crossing", "cycleway", "elevator", "emergency_access_point", 
                                 "emergency_bay", "escape", "give_way", "milestone", "mini_roundabout", 
                                 "motorway", "motorway_junction", "motorway_link", "passing_place", 
                                 "platform", "proposed", "raceway", "rest_area", "services", "speed_camera", 
                                 "stop", "street_lamp", "toll_gantry", "traffic_mirror", "traffic_signals", 
                                 "trailhead", "turning_circle", "turning_loop", "User Defined"
), drive = c(1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 
             0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
             0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
             0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), walk = c(1L, 1L, 1L, 1L, 1L, 
                                                       0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 
                                                       0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 
                                                       1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
kind = c("street", "path", "street", "street", "street", 
         "highway", "highway", "path", "street", "street", "path", 
         "street", "path", "street", "street", "path", "street", "street", 
         "path", "highway", "nothing", "highway", "nothing", "path", 
         "path", "path", "path", "nothing", "nothing", "nothing", 
         "nothing", "nothing", "street", "highway", "highway", "highway", 
         "street", "path", "nothing", "nothing", "nothing", "nothing", 
         "nothing", "nothing", "nothing", "nothing", "nothing", "nothing", 
         "nothing", "nothing", "nothing", "nothing")), class = "data.frame", row.names = c(NA, 
                                                                                           -52L))
# preapre network, origins and destinations
# create query of walking 
# paris
q_walk <- '
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area:3600007444);
(._;>;);
out meta;'
# ashdod
q_walk <- '
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area:3601380013);
(._;>;);
out meta;'
# netanya
q_walk <- '
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area:3601383391);
(._;>;);
out meta;'
# mahoz tel aviv
q_walk <- '
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area:3601400916);
(._;>;);
out meta;'
# beer sheva
q_walk <- '
way(675627797);
map_to_area -> .ar;
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area.ar);
(._;>;);
out geom meta;'
# beitar ilit
q_walk <- 
  'rel(10044900);
map_to_area -> .ar;
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area.ar);
(._;>;);
out geom meta;'

# la plata
q_walk <- 
  'rel(3266014);
map_to_area -> .ar;
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area.ar);
(._;>;);
out geom meta;'

# afula

q_walk <- 
  'rel(1380226);
map_to_area -> .ar;
way["highway"]["area"!~"yes"]["highway"!~"abandoned|construction|no|planned|platform|proposed|raceway|razed"]["foot"!~"no"]["service"!~"private"](area.ar);
(._;>;);
out geom meta;'
raw <- osmdata_sf(q_walk)
jlm_walk <- raw$osm_lines %>% 
  bind_rows(raw$osm_polygons %>%
              st_cast("LINESTRING")) %>% 
  as_sfnetwork(directed =F) %>% 
  convert(to_spatial_subdivision) %>% 
  mutate(cmp = group_components()) %>% 
  filter(cmp ==1 ) 






# get all nodes from which you cannot depart or to which you cannot arrive as a pedestrian
jlm_walk %>% 
  mutate(rn = row_number(),
         degree = centrality_degree())%E>% 
  left_join(types, by =c("highway"="type")) %>% 
  filter(kind == "highway") %N>% 
  filter(centrality_degree() == degree,degree>0) %>% 
  as_tibble() %>% 
  as_tibble() %>% 
  pull(rn)
# get all nodes to which you can arrive as a pedestrian and as a motorist
tos2 <- jlm_walk %>% 
  mutate(rn = row_number())%E>% 
  left_join(types, by =c("highway"="type")) %N>% {
    eds <- (.) %E>% as_tibble()
    (.) %>% 
      # as_tibble() %>% 
      mutate(q = map_int(rn,~sum(eds$kind[eds$from == .x |eds$from == .x] == "street") ))
  } %>% 
  filter(q>2) %>% 
  as_tibble() %>% 
  as_tibble() %>% 
  pull(rn)
#  get all nodes from which you can depart as a pedestrian 
froms <- jlm_walk %>% 
  mutate(rn = row_number())%E>% 
  left_join(types, by =c("highway"="type")) %>% 
  filter(kind != "highway" |kind != "nothing") %N>% 
  filter(centrality_degree() > 0) %>% 
  # autoplot()
  as_tibble() %>% 
  as_tibble() %>% 
  pull(rn)
# create intial network with link classification, and leave out those nodes which belong to the highwway
net <- jlm_walk %>% 
  mutate(rn = row_number())%E>% 
  left_join(types, by =c("highway"="type")) %>% 
  filter(kind != "highway") %N>% 
  mutate(is_to = rn %in% tos2,
         is_from = rn %in% froms)
# filter the giant component only
giant <- net %>% components() %>% `$`(csize) %>% which.max()
in_giant <- net %>% components() %>% `$`(membership) %>% `==`(giant)
# filter the from and tos in the giant component 
froms1 <- froms[froms %in% (1:length(in_giant))[in_giant]]
tos1 <-  tos2[tos2 %in% (1:length(in_giant))[in_giant]]
# create a second network where you have only the giant component and no multiple ot looped edges
net1 <- net %>% 
  mutate(name = as.character(rn)) %>% 
  select(name,everything()) %>% 
  filter(in_giant) %>% 
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop()) %>%
  mutate(weight = edge_length() %>% as.integer())
# navigation of buses
net2 <- jlm_walk %>% 
  st_join(net1 %N>% as_tibble(),join = st_nn) %E>% 
  left_join(types, by =c("highway"="type")) %>% 
  filter(kind != "nothing",kind != "path")
# create a distance matrix from all froms to all tos
mat <- net1 %N>% 
  st_network_cost(from = as.character(froms1),to=as.character( tos1))
object.size(mat)
# turn the nubers to integers to save space
mode(mat) <- "integer"
object.size(mat)
gc()
# function to find all  the closest center to nodes, assigning which node is closest to which center
find_closest_center <- function(mat1,centers){
  closest_centers <- centers[mat1[,centers] %>% apply(1,which.min)]
  names(closest_centers) <- row.names(mat1)
  return(closest_centers %>% stack())
}
# function to break down the different centers' matrices
get_center_matrix <- function(closest_center,mat) {
  closest_center %>% 
    mutate(ind = as.character(ind)) %>% 
    filter(ind %in% colnames(mat)) %>% 
    group_by(values) %>%
    group_split() %>% 
    map(~mat[.x$ind,.x$ind, drop = FALSE] %>%as.matrix() )
}
# function to find a new center within  the old centers' matrices. find the node closest to all other nodes
find_new_centers <- function(center_matrix) {
  center_matrix %>% 
    map_chr(~.x %>%  apply(1,max) %>% which.min() %>% names())
}
# function to find another center - the most remote point within the given communities
find_extra_center <- function(center_matrix) {
  max_row <- center_matrix%>% 
    map(~.x %>%  apply(1,max))
  which_mat <-  max_row %>% 
    map(~.x %>%  max()) %>% unlist() %>% which.max()
  max_row %>% 
    map(~.x  %>% which.max()) %>% 
    `[[`(which_mat) %>% 
    names()
}
# function to check what is the largest distance within the clusters to check if the stoppong condition is met
find_max_distance_in_center_matrix <- function(center_matrix,centers) {
  map2_int(center_matrix,centers,~.x[,.y] %>% max())
}
# function to find the intial 2 centers for the algorithm - the two most remote nodes from each other
find_initial_centers <- function(mat) {
  maximum <- mat[colnames(mat),] %>%  apply(1,max) %>% max()
  which(apply(mat[colnames(mat),],1,max) == maximum) %>% names()
}

# the entire procedure:
proc <- function(mat,thershold,quan = 1){
  # find the current largest distance between nodes
  cur_max <- mat %>% max()
  # get initial centers for algorithm initialiazation
  centers <- find_initial_centers(mat)
  storage <- list()
  # wnter a loop until quan percent of the clusters have less than thereshold distance to their center
  while(cur_max > thershold){
    # for all nodes, find closest center
    closest_center <- find_closest_center(mat,centers)    
    # for all clusters, create a distance matrix for all nodes in the cluster
    center_matrix <- get_center_matrix(closest_center,mat)
    # find the new centers within the clusters
    new_centers <- find_new_centers(center_matrix)
    # if no new centers are found, add a new center in the most remote point from a center
    if(all(new_centers %in% centers)){
      centers <- c(centers,find_extra_center(center_matrix))
      # else, assign new centers and prepare for reassignment of nodes to clusters
    }else{
      centers <-new_centers
    }
    # store all centers in a list
    storage[[length(storage)+1]] <- centers
    # update the current maximum distance
    cur_max <- find_max_distance_in_center_matrix(center_matrix,new_centers) %>% 
      quantile(quan)
    print(paste0("n centers: ",length(centers)))
    print(cur_max)
  }
  # return the centers for all nodes
  storage
}
# run for 40% of the clusters to get a 500 meters distance from center
# res_centers <- proc(mat,500,0.4)
# la plata - 90%
res_centers <- proc(mat,500,0.9)
stor <- res_centers
# all_iter <- imap_dfr(stor,~find_closest_center(mat,.x) %>% mutate(turn= .y))
# all_iter %>% 
#   left_join(net1 %N>% 
#               as_tibble(), by = c("ind"="name")) %>% 
#   st_sf() %>% 
#   select(values, turn) %>% 
#   st_write("qgis_attempt.shp",append = F)
res_centers <- res_centers[[length(res_centers)]]
# get classification of nodes to centers
closest_center <- find_closest_center(mat,res_centers) %>% 
  mutate(dist = map2_int(ind,values, ~mat[.x,.y]))
# get a map of distance from the nodes to the center,having >1000 values as 1000 meters
net1 %N>% 
  as_tibble() %>% 
  left_join(closest_center,by = c("name" = "ind")) %>% 
  mutate(dist1 = ifelse(dist<1000,dist,1000)) %>% 
  # filter(dist==0) %>%
  mapview(zcol = "dist1")
pipe_message = function(.data, status) {message(status); .data}
# function to contract the net, and connect the different connected clusters to each other. 
# this is done to plan longer transti routes. all those connected clusters already have routes between them
contract_net <- function(net1, mat, res_centers) {
  contracted_net <- net1 %N>% 
    left_join(find_closest_center(mat,res_centers ),by = c("name" = "ind")) %>% 
    select(values) %>% 
    convert(to_spatial_contracted,values,simplify = TRUE,
            summarise_attributes = list(values = "first")) %>% 
    select(name = values) %E>% 
    mutate(weight = 1) 
  contracted_net %>% 
    as_tbl_graph() %>% 
    select(from,to)
}
# get all possible new edges that are not currently in the net
possible_new_edges <- function(contracted_net) {
  possible_edges <- contracted_net %N>% 
    as_tibble() %>% 
    expand_grid(to = .$name) %>% 
    rename(from = name)
  cur_edges <- contracted_net %>% 
    mutate(from1 = .N()$name[from],to1 = .N()$name[to]) %>% 
    as_tibble() %>% 
    select(from1,to1) 
  names_order <- contracted_net %N>% pull(name)
  res <- possible_edges%>% 
    anti_join(cur_edges, by = c("from" = "from1","to"="to1")) %>% 
    mutate(from = map_int(from,~which(.x == names_order)),
           to = map_int(to,~which(.x == names_order))) %>% 
    filter(from < to) %>% 
    select(-geometry) %>% 
    return(res)
}
# contract the net according to the centers of the clusters







contracted_net2 <- contract_net(net2,mat,res_centers)
named_contractedd_net <- contracted_net2%>% mutate(from1 = .N()$name[from],to1 = .N()$name[to])%>% as_tibble()
res23 <- map_df(1:nrow(named_contractedd_net),~net2 %>%
                  pipe_message(.x) %>% 
                  filter(kind == "street" | kind == "highway") %>%
                  st_network_paths(from = named_contractedd_net$from1[.x],to = named_contractedd_net$to1[.x]) %>%
                  mutate(from = named_contractedd_net$from1[.x],to = named_contractedd_net$to1[.x]))
geoms <- net2 %>%filter(kind == "street"| kind == "highway") %>%  as_tibble()
m23 <-res23 %>%
  select(edge_paths,from,to) %>%
  unnest(edge_paths) %>%
  mutate(geom = geoms[edge_paths,]$geometry) %>%
  st_sf() %>%
  mutate(name =paste0(from,"-",to),len = st_length(geom) %>% as.numeric() ) %>%
  group_by(from,to,name) %>%
  summarise(len = sum(len))
m23  %>% st_jitter(0.0001) %>% mapview(zcol = "len",label = "name") %>% `+`(net2 %N>% 
                                                                              as_tibble() %>% 
                                                                              left_join(closest_center,by = c("name" = "ind")) %>% 
                                                                              mutate(dist1 = ifelse(dist<1000,dist,1000)) %>% 
                                                                              filter(dist==0) %>%
                                                                              mapview(zcol = "dist1"))

# t(combn(1:229,2)) %>% 
#   as_tibble() %>% 
#   mutate(res = map2(V1,V2,~{
#     contracted_net2  %N>% 
#       filter(!row_number() %in% c(.x,.y) ) %>% 
#       components() %>% `$`(membership) %>% table() %>% stack()  
#   })) %>% unnest() %>% View()
# 
# contracted_net2 %>%as_sfnetwork() %>%  autoplot()
# contracted_net2  %N>% 
#   filter(!row_number() %in% c(93,94) ) %>% 
#   mutate(grp = group_components() %>% as.factor()) %>% 
#   as_sfnetwork() %>% 
#   as_tibble() %>% 
#   ggplot(aes(color = grp)) +annotation_map_tile(zoom=12) + geom_sf()





# get possible new edges
possible_new_edges2 <- possible_new_edges(contracted_net2)
# function to check what is the maximal number of transfers a person might neet to do
get_max_transfers <- function(contracted_net2) {
  contracted_net2 %>% 
    distances() %>% 
    max()
}
get_max_transfers(contracted_net2)
# function to find an edge where when added, will reduce the number of transfers in the optimum way
find_least_transfer_edge <- function(possible_new_edges2, contracted_net2) {
  opts <- map_dbl(1:nrow(possible_new_edges2),~contracted_net2 %>% 
                    bind_edges(possible_new_edges2[.x,] ) %>% 
                    # pipe_message(possible_new_edges2[.x,]) %>%
                    distances() %>% 
                    mean())
  rels <- which(opts == min(opts))
  print(length(rels))
  map_df(rels,~contracted_net2 %>% 
           bind_edges(possible_new_edges2[.x,] ) %>% 
           distances() %>% 
           as.vector() %>% 
           # pipe_message(.x) %>%
           set_names(.x) %>% 
           stack() %>% 
           mutate(ind = as.numeric(as.character(ind))) %>% 
           group_by(ind) %>% 
           count(values) %>% 
           filter(values == max(values))) %>% 
    ungroup() %>% 
    filter(n == min(n)) %>% 
    pull(ind) %>% 
    {
      possible_new_edges2[(.),]
    }
}
# add this edge to the contracted net
add_least_transer_edge <- function(contracted_net2, least_transfer_edge) {
  return(bind_edges(contracted_net2,least_transfer_edge))
}

# carry out the algorithm:
looper <- function(contracted,possible_new_edges2,hub_max_size = 1){
  # get edeges which already gor an added edges to prevent them from getting another edge
  already_added = data.frame(from=integer(),to=integer())
  # while the biggest number of transfers is bigger than 5 (4 transfers actualluy, five edges per route)
  while(get_max_transfers(contracted) > 5){
    # get the least transfer edge
    least_transfer_edge <- find_least_transfer_edge(possible_new_edges2,contracted)
    # add the least transfer edge to the netwrok
    contracted <-add_least_transer_edge(contracted, least_transfer_edge)  
    # add the least transfer edge to the already added edges
    
    already_added <- already_added %>% bind_rows(least_transfer_edge)  
    counter <- c(already_added$from,already_added$to) %>% table() %>% stack() %>% filter(values >hub_max_size-1)
    
    
    # get a list of all possible new edges, without those already added nodes of interst
    possible_new_edges2 <- possible_new_edges(contracted) %>% 
      # filter(!from %in% c(already_added$from,already_added$to) ,
      # !to %in% c(already_added$from,already_added$to))
      filter(!from %in% counter$ind ,
             !to %in% counter$ind)
    # print the number of max transfers
    print(get_max_transfers(contracted))
  }
  # return the net
  contracted
}
# least_transfer_edge <- find_least_transfer_edge(possible_new_edges2,contracted_net2)
# carry out the algorithm
res <- looper(contracted_net2,possible_new_edges2,3)
# add names of nodes to edges
with_names <- res %>% mutate(from1 = .N()$name[from],to1 = .N()$name[to])%>% as_tibble()
# get actual paths on the network currently without relate to roads or directions
res1 <- map_df(1:nrow(with_names),~net2 %>% 
                 pipe_message(.x) %>% 
                 filter(kind == "street" | kind == "highway") %>% 
                 st_network_paths(from = with_names$from1[.x],to = with_names$to1[.x]) %>% 
                 mutate(from = with_names$from1[.x],to = with_names$to1[.x]))
# get their geometries
geoms <- net2 %>%filter(kind == "street" | kind == "highway") %>%  as_tibble()
m23 <-res1 %>%
  select(edge_paths,from,to) %>%
  unnest(edge_paths) %>%
  mutate(geom = geoms[edge_paths,]$geometry) %>%
  st_sf() %>%
  mutate(name =paste0(from,"-",to),len = st_length(geom) %>% as.numeric() ) %>%
  group_by(from1=from,to1=to,name) %>%
  group_split() %>% 
  map_df(~.x %>% as_sfnetwork(directed=F) %E>%select(name,from1,to1) %>% convert(to_spatial_smooth,summarise_attributes ="first") %>% as_tibble()) %>% 
  select(-from,-to) %>% 
  rename(from = from1,to = to1) %>% 
  mutate(len =  st_length(geom) %>% as.numeric()) %>% 
  select(-.tidygraph_edge_index)
  
mapviewOptions(fgb = FALSE)
m23  %>% st_jitter(0.0001) %>% mapview(zcol = "len",label = "name",highlight = leaflet::highlightOptions(color = "red", weight = 20, sendToBack = F)) %>% 
  `+`(net2 %N>% 
        as_tibble() %>%
        left_join(closest_center,by = c("name" = "ind")) %>%
        mutate(dist1 = ifelse(dist<1000,dist,1000)) %>%
        filter(dist==0) %>%
        mapview(zcol = "values")) %>% 
  `+`(
    net1 %N>% 
      as_tibble() %>% 
      left_join(closest_center,by = c("name" = "ind")) %>% 
      mutate(dist2 = ifelse(dist<1000,dist,1000)) %>% 
      # filter(dist==0) %>%
      mapview(zcol = "dist2")  
  )
stops_alg <- net2 %N>% 
  as_tibble() %>%
  left_join(closest_center,by = c("name" = "ind")) %>%
  mutate(dist1 = ifelse(dist<1000,dist,1000)) %>%
  filter(dist==0)
lines_alg_1 <- m23
lines_alg_2 <- lines_alg_1 %>% 
  mutate(name = paste0(to,"-",from),
         tmp = from,
         from = to,
         to = tmp) %>% 
  st_reverse() %>% 
  mutate(len =  st_length(geom) %>% as.numeric()) %>% 
  select(-tmp)
lines_alg <- bind_rows(lines_alg_1,lines_alg_2)
m1 <- res %N>%  as_tibble() %>% st_sf() 
m2 <- res1 %>% 
  select(edge_paths,from,to) %>% 
  unnest(edge_paths) %>% 
  mutate(geom = geoms[edge_paths,]$geom) %>% 
  st_sf()
# map to show the scheme
ggplot() + 
  geom_sf(data = m2, aes(color = paste0(from,"-",to)),show.legend = F) +
  geom_sf(data = m1)+
  scale_color_manual(values = randomColor(nrow(m2)))
# distribution of the length of the bus routes
m2 %>% 
  mutate(name =paste0(from,"-",to),len = st_length(geom) %>% as.numeric() ) %>% 
  group_by(name) %>% 
  summarise(len = sum(len)) %>% 
  ggplot(aes(x=len)) + 
  geom_histogram(bins=100) + 
  scale_x_continuous(breaks = seq(0,13500,500)) + 
  guides(x=guide_axis(n.dodge = 2))
# show long lines
m2 %>% 
  mutate(name =paste0(from,"-",to),len = st_length(geom) %>% as.numeric() ) %>% 
  group_by(name) %>% 
  summarise(len = sum(len)) %>% 
  filter(len > 2100) %>% 
  st_transform(2039) %>% 
  st_jitter(200) %>% {
    q <- (.)
    ggplot(q) +
      annotation_map_tile(zoom=12,alpha=0.5)+
      geom_sf(mapping = aes(color = name),show.legend = F,size = 2) + 
      scale_color_manual(values = randomColor(nrow(q))) +
      geom_sf(data = m1)
  }

