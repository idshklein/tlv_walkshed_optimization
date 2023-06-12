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
# `-.gg` <- function(plot, layer) {
#   if (missing(layer)) {
#     stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
#   }
#   if (!is.ggplot(plot)) {
#     stop('Need a plot on the left side')
#   }
#   plot$layers = c(layer, plot$layers)
#   plot
# }
netwalk <- st_read("tlvall.gpkg",layer = "edges") %>% 
  as_sfnetwork(directed = F) 
uniqs <- netwalk %E>% pull(highway) %>% unique()
ido_class <- data.frame(type = uniqs[str_which(uniqs,"\\[",negate = T)],
                        walk = c(T,T,T,T,T,T,F,F,T,F,T,F,T,T,T,T,T,T,T,T,T,T),
                        drive = c(T,F,T,F,T,T,T,T,T,T,T,T,T,T,F,F,T,T,F,F,F,T)) %>% 
  bind_rows(data.frame(type = uniqs[str_which(uniqs,"\\[")], walk = T,drive = F)) %>% 
  left_join(data.frame(walk = c(T,T,F,F),
                       drive =c(T,F,T,F),
                       kind = c("street","path","highway","irrelevent")),by = c("walk","drive"))
# neither to nor from
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
# to
# tos <- netwalk %>% 
#   mutate(rn = row_number())%E>% 
#   left_join(ido_class, by =c("highway"="type")) %>% 
#   filter(kind == "street") %N>% 
#   filter(centrality_degree() >0) %>% 
#   # autoplot()
#   as_tibble() %>% 
#   as_tibble() %>% 
#   pull(rn)

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
#  from
froms <- netwalk %>% 
  mutate(rn = row_number())%E>% 
  left_join(ido_class, by =c("highway"="type")) %>% 
  filter(kind != "highway") %N>% 
  filter(centrality_degree() > 0) %>% 
  # autoplot()
  as_tibble() %>% 
  as_tibble() %>% 
  pull(rn)
net <- netwalk %>% 
  mutate(rn = row_number())%E>% 
  left_join(ido_class, by =c("highway"="type")) %>% 
  filter(kind != "highway") %N>% 
  mutate(is_to = rn %in% tos2,
         is_from = rn %in% froms)

in_giant <- net %>% components() %>% `$`(membership) %>% `==`(1)

froms1 <- froms[froms %in% (1:length(in_giant))[in_giant]]
tos1 <-  tos2[tos2 %in% (1:length(in_giant))[in_giant]]
net1 <- net %>% 
  mutate(name = as.character(rn)) %>% 
  select(name,everything()) %>% 
  filter(in_giant) %>% 
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop()) %>%
  mutate(weight = edge_length() %>% as.integer())
mat <- net1 %N>% 
  st_network_cost(from = as.character(froms1),to=as.character( tos1))
object.size(mat)
mode(mat) <- "integer"
object.size(mat)
gc()

find_closest_center <- function(mat1,centers){
  closest_centers <- centers[mat1[,centers] %>% apply(1,which.min)]
  names(closest_centers) <- row.names(mat1)
  return(closest_centers %>% stack())
}
get_center_matrix <- function(closest_center,mat) {
  closest_center %>% 
    mutate(ind = as.character(ind)) %>% 
    filter(ind %in% colnames(mat)) %>% 
    group_by(values) %>%
    group_split() %>% 
    map(~mat[.x$ind,.x$ind, drop = FALSE] %>%as.matrix() )
}
find_new_centers <- function(center_matrix) {
  center_matrix %>% 
    map_chr(~.x %>%  apply(1,max) %>% which.min() %>% names())
}
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

find_max_distance_in_center_matrix <- function(center_matrix,centers) {
  map2_int(center_matrix,centers,~.x[,.y] %>% max())
}

find_initial_centers <- function(mat) {
  maximum <- mat[colnames(mat),] %>%  apply(1,max) %>% max()
  which(apply(mat[colnames(mat),],1,max) == maximum) %>% names()
}
# init_centers <- find_initial_centers(mat)
# closest_center <- find_closest_center(mat,init_centers)
# center_matrix <- get_center_matrix(closest_center,mat)
# find_new_centers(center_matrix)
# find_max_distance_in_center_matrix(center_matrix,init_centers)
# find_extra_center(center_matrix) 


proc <- function(mat,thershold,quan = 1){
  cur_max <- mat %>% max()
  centers <- find_initial_centers(mat)
  while(cur_max > thershold){
    closest_center <- find_closest_center(mat,centers)    
    center_matrix <- get_center_matrix(closest_center,mat)
    new_centers <- find_new_centers(center_matrix)
    if(all(new_centers %in% centers)){
      centers <- c(centers,find_extra_center(center_matrix))
    }else{
      centers <-new_centers
    }
    cur_max <- find_max_distance_in_center_matrix(center_matrix,new_centers) %>% 
      quantile(quan)
    print(paste0("n centers: ",length(centers)))
    print(cur_max)
  }
  centers
}
res_centers <- proc(mat,500,0.4)


closest_center <- find_closest_center(mat,res_centers) %>% 
  mutate(dist = map2_int(ind,values, ~mat[.x,.y]))

net1 %N>% 
  as_tibble() %>% 
  left_join(closest_center,by = c("name" = "ind")) %>% 
  mutate(dist1 = ifelse(dist<1000,dist,1000)) %>% 
  # filter(dist<=1000) %>%
  mapview(zcol = "dist1")
pipe_message = function(.data, status) {message(status); .data}

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

contracted_net2 <- contract_net(net1,mat,res_centers)
possible_new_edges2 <- possible_new_edges(contracted_net2)
get_max_transfers <- function(contracted_net2) {
  contracted_net2 %>% 
    distances() %>% 
    max()
}
get_max_transfers(contracted_net2)
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

add_least_transer_edge <- function(contracted_net2, least_transfer_edge) {
  return(bind_edges(contracted_net2,least_transfer_edge))
}
looper <- function(contracted,possible_new_edges2){
  already_added = data.frame(from=integer(),to=integer())
  while(get_max_transfers(contracted) > 5){
    least_transfer_edge <- find_least_transfer_edge(possible_new_edges2,contracted)
    contracted <-add_least_transer_edge(contracted, least_transfer_edge)  
    already_added <- already_added %>% bind_rows(least_transfer_edge)
    possible_new_edges2 <- possible_new_edges(contracted) %>% 
      filter(!from %in% c(already_added$from,already_added$to) ,
             !to %in% c(already_added$from,already_added$to))
    print(get_max_transfers(contracted))
  }
  contracted
}
res <- looper(contracted_net2,possible_new_edges2)
res %>% distances() %>% hist()
res %>% as_sfnetwork() %>% plot()
with_names <- res %>% mutate(from1 = .N()$name[from],to1 = .N()$name[to])
res1 <- map(1:nrow(res %>% as_tibble()),~net1 %>% 
                 # filter(drive) %>% 
                 convert(to_spatial_shortest_paths,from = with_names %>% as_tibble()  %>% slice(.x)%>% pull(from1),
                         to = with_names %>% as_tibble()%>% slice(.x) %>% pull(to1)) %>% 
                 pipe_message(.x) %>% 
                 convert(to_spatial_smooth) %>% 
                 as_tibble())
map_df(res1,select,geom) %>% mapview() + st_sf(as_tibble(activate(res,nodes)))
map_df(res1,select,geom) %>% mapview() + st_startpoint(map_df(res1,select,geom)) + st_endpoint(map_df(res1,select,geom))



contracted3 <- add_least_transer_edge(contracted_net2, possible_new_edges2) 
possible_new_edges3 <- possible_new_edges(contracted3)
find_least_transfer_edge(contracted3, possible_new_edges3)
contracted4 <- add_least_transer_edge(contracted3, possible_new_edges3)
find_least_transfer_edge(possible_new_edges2,contracted_net2)
possible_new_edges4 <- possible_new_edges(contracted4)
contracted5 <- add_least_transer_edge(contracted4, possible_new_edges2) 
get_max_transfers(contracted4)
contracted_net2 %>% 
  bind_edges(possible_new_edges[456,] ) %>% 
  distances() %>% 
  as.vector()
hist()
contracted_net %N>%
  st_network_cost(from = res_centers,to = res_centers) %>% 
  as.data.frame() %>% 
  rownames_to_column("from") %>% 
  gather(to,transfers,-from)

stops <- p %N>%   as_tibble() %>% select(geom)
lines <- p %E>%   as_tibble()
mapview(lines) + stops

net1  %>% 
  mutate(weight = 1)%N>%
  st_network_cost(from = res_centers,to = res_centers)
convert(to_spatial_shortest_paths,from = res_centers[1],to = res_centers[5]) %>% 
  autoplot() 

center_matrix <- get_center_matrix(closest_center,mat)


init_centers %>% names()
mat[,init_centers %>% names()]
find_closest_center(mat,init_centers %>% names()) 
net1 %N>% 
  as_tibble() %>% 
  left_join(find_closest_center(mat,init_centers %>% names()) ,by = c("name" = "ind")) %>% 
  ggplot(aes(color = values))+geom_sf()
find_new_centers(mat,init_centers,tos1)  
init_centers  %>% stack() %>% 
  left_join(net1 %N>% 
              as_tibble(), by = c("ind"="name")) %>% 
  st_sf() 


net1 %N>% 
  as_tibble() %>% 
  left_join(find_closest_center(mat,find_new_centers(mat,init_centers,tos1) ) %>% stack(),by = c("name" = "ind")) %>% 
  ggplot()+
  geom_sf(aes(color = values)) + 
  geom_sf(data =   net1 %N>% 
            as_tibble() %>% 
            filter(name %in% find_new_centers(mat,init_centers,tos1)))
net1 %N>% 
  as_tibble() %>% 
  filter(!is_from) %>% 
  mapview()


