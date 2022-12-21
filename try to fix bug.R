library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)
library(mapview)
library(reticulate)
library(randomcoloR)
library(jsonlite)
library(igraph)
library(lwgeom)
library(ggspatial)
# preapre network, origins and destinations
netwalk <- st_read("tlvall.gpkg",layer = "edges") %>% 
  as_sfnetwork(directed = F) 
# get all unique values of kinds of lonks
uniqs <- netwalk %E>% pull(highway) %>% unique()
# create a classifivcation ofr linls, whether they are drivable or walkable
ido_class <- data.frame(type = uniqs[str_which(uniqs,"\\[",negate = T)],
                        walk = c(T,T,T,T,T,T,F,F,T,F,T,F,T,T,T,T,T,T,T,T,T,T),
                        drive = c(T,F,T,F,T,T,T,T,T,T,T,T,T,T,F,F,T,T,F,F,F,T)) %>% 
  bind_rows(data.frame(type = uniqs[str_which(uniqs,"\\[")], walk = T,drive = F)) %>% 
  left_join(data.frame(walk = c(T,T,F,F),
                       drive =c(T,F,T,F),
                       kind = c("street","path","highway","irrelevent")),by = c("walk","drive"))
# get all nodes from which you cannot depart or to which you cannot arrive as a pedestrian
netwalk %>% 
  mutate(rn = row_number(),
         degree = centrality_degree())%E>% 
  left_join(ido_class, by =c("highway"="type")) %>% 
  filter(kind == "highway") %N>% 
  filter(centrality_degree() == degree,degree>0) %>% 
  # autoplot()
  as_tibble() %>% 
  as_tibble() %>% 
  pull(rn)
# get all nodes to which you can arrive as a pedestrian and as a motorist
tos2 <- netwalk %>% 
  mutate(rn = row_number())%E>% 
  left_join(ido_class, by =c("highway"="type")) %N>% {
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
froms <- netwalk %>% 
  mutate(rn = row_number())%E>% 
  left_join(ido_class, by =c("highway"="type")) %>% 
  filter(kind != "highway") %N>% 
  filter(centrality_degree() > 0) %>% 
  # autoplot()
  as_tibble() %>% 
  as_tibble() %>% 
  pull(rn)
# create intial network with link classification, and leave out those nodes which belong to the highwway
net <- netwalk %>% 
  mutate(rn = row_number())%E>% 
  left_join(ido_class, by =c("highway"="type")) %>% 
  filter(kind != "highway") %N>% 
  mutate(is_to = rn %in% tos2,
         is_from = rn %in% froms)
# filter the giant component only
in_giant <- net %>% components() %>% `$`(membership) %>% `==`(1)
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
# finction to break down the different centers' matrices
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
    # update the current maximum distance
    cur_max <- find_max_distance_in_center_matrix(center_matrix,new_centers) %>% 
      quantile(quan)
    print(paste0("n centers: ",length(centers)))
    print(cur_max)
  }
  # return the centers for all nodes
  centers
}
# run for 40% of the clusters to get a 500 meters distance from center
res_centers <- proc(mat,500,0.4)

# get classification of nodes to centers
closest_center <- find_closest_center(mat,res_centers) %>% 
  mutate(dist = map2_int(ind,values, ~mat[.x,.y]))
# get a map of distance from the nodes to the center,having >1000 values as 1000 meters
net1 %N>% 
  as_tibble() %>% 
  left_join(closest_center,by = c("name" = "ind")) %>% 
  mutate(dist1 = ifelse(dist<1000,dist,1000)) %>% 
  # filter(dist<=1000) %>%
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
    select(-geom)
  return(res)
}
# contract the net according to the centers of the clusters
contracted_net2 <- contract_net(net1,mat,res_centers)
# named_contractedd_net <- contracted_net2%>% mutate(from1 = .N()$name[from],to1 = .N()$name[to])%>% as_tibble()
# res23 <- map_df(1:nrow(named_contractedd_net),~net1 %>% 
#                  filter(kind == "street") %>% 
#                  st_network_paths(from = named_contractedd_net$from1[.x],to = named_contractedd_net$to1[.x]) %>% 
#                  mutate(from = named_contractedd_net$from1[.x],to = named_contractedd_net$to1[.x]))
# geoms <- net1 %>%filter(kind == "street") %>%  as_tibble()
# m23 <-res23 %>% 
#   select(edge_paths,from,to) %>% 
#   unnest(edge_paths) %>% 
#   mutate(geom = geoms[edge_paths,]$geom) %>% 
#   st_sf() %>% 
#   mutate(name =paste0(from,"-",to),len = st_length(geom) %>% as.numeric() ) %>% 
#   group_by(from,to,name) %>% 
#   summarise(len = sum(len))



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
           pipe_message(.x) %>%
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
# carry out the algorithm
res <- looper(contracted_net2,possible_new_edges2,3)
# add names of nodes to edges
with_names <- res %>% mutate(from1 = .N()$name[from],to1 = .N()$name[to])%>% as_tibble()
# get actual paths on the network currently without relate to roads or directions
res1 <- map_df(1:nrow(with_names),~net1 %>% 
                 filter(kind == "street") %>% 
                 st_network_paths(from = with_names$from1[.x],to = with_names$to1[.x]) %>% 
                 mutate(from = with_names$from1[.x],to = with_names$to1[.x]))
# get their geometries
geoms <- net1 %>%filter(kind == "street") %>%  as_tibble()
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

