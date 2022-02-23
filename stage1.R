library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)
library(igraph)
library(lwgeom)
library(nngeo)
library(mapview)
library(osmdata)
library(randomcoloR)
library(viridis)
library(gifski)
library(fs)
library(gganimate)
library(patchwork)
library(scales)
Sys.setlocale(locale = "hebrew")
gush_dan <- st_read("C:/idos_shit2/israel-ISR_gpkg/tel_aviv-4309.gpkg",layer = "edges")
dat1 <- opq_osm_id (type = "relation", id = "1382494") %>%
  opq_string () %>%
  osmdata_sf ()
tlv <- dat1$osm_multipolygons 
step1 <- st_intersection(gush_dan,tlv)
step2 <- step1 %>% st_transform(2039)
step3 <- step2 %>% select()
step4 <- step3 %>% st_cast("LINESTRING")
pts <- step4 %>% 
  st_line_sample(density = 1/300) %>% 
  st_cast("POINT") %>% 
  st_sf()  %>% 
  filter(!st_is_empty(geometry)) %>% 
  `$`(geometry)
step5 <- step4 %>% 
  as_sfnetwork() %>% 
  st_network_blend(pts,0.1) %>% 
  activate(edges) %>% 
  st_as_sf()
net <- as_sfnetwork(step5,directed = F)
in_giant <- net %>% components() %>% `$`(membership) %>% `==`(1)
net1 <- net %>% 
  filter(in_giant) %>% 
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>% 
  filter(!edge_is_loop()) %>% 
  activate(nodes)
mat <- net1 %>% 
  st_network_cost()
colnames(mat) <- rownames(mat) <- as.numeric(1:nrow(mat))
vec <- function(mat1,centers){
  return(mat1[,centers] %>% apply(1,which.min))
}

create_new_centers <- function(old_centers, mat1){
  K <- length(old_centers)
  vec1 <- vec(mat1,old_centers)
  res <- map_dbl(1:K,function(x){
    if(length(mat1[vec1 == x,vec1 == x]) == 1){
      x
    }else{
      mat1[vec1 == x,vec1 == x] %>% 
        apply(1,max) %>%
        which.min() %>% 
        names() %>% 
        as.numeric()   
    }
  })
  return(res)
}

plot_progress <- function(net1,centers,mat1,n,cols){
  vec1 <- vec(mat1,centers)
  p <- ggplot() +
    geom_sf(data = net1 %>% st_as_sf() %>% mutate(vec1 = factor(vec1)),mapping = aes(color = vec1)) +
    geom_sf(data = net1 %>% st_as_sf() %>% slice(centers),color = "black",size =3) +
    ggtitle(n) +
    scale_color_manual(guide = "none",values = cols[1:length(centers)])  
  return(p)
}

incremental_add_centers <- function(centers, mat1, thershold,net1,cols){
  # ggsave(filename = "000.png",plot = plot_progress(net1,centers,mat1,0,cols))
  max_min_dist_from_center <- mat1[,centers] %>% apply(1,min) %>% max()
  i <- 1
  while(thershold < max_min_dist_from_center){
    tmp <- create_new_centers(centers, mat1)  
    if(identical(sort(tmp),sort(centers))){
      max_max_node_from_center <- mat1[,centers] %>% apply(1,min) %>% which.max()
      print(max_max_node_from_center)
      centers <- c(centers,max_max_node_from_center)
      print("A")
      print(centers)
    }else{
      centers <- create_new_centers(centers, mat1)  
      print("B")
      print(centers)
    }
    max_min_dist_from_center <- mat1[,centers] %>% apply(1,min) %>% max() 
    print(max_min_dist_from_center)
    prefix <- ifelse(i<10,paste0("00",i),ifelse(i<100,paste0("0",i),i))
    # ggsave(filename = paste0(prefix,".png"),plot = plot_progress(net1,centers,mat1,i,cols))
    i <- i+1
  }
  return(centers)
}

maximum <- mat %>% round() %>% apply(1,max) %>% max()
init_centers <- which(apply(round(mat),1,max) == maximum)
cols1 <- randomColor(1000)
division <- incremental_add_centers(init_centers,mat,750,net1,cols1)
plot_progress(net1,division,mat,"i",cols1)

cols2 <- division[vec(mat1 = mat,centers = division)]
net1 %>% 
  st_as_sf() %>% 
  mutate(rn = row_number()) %>% 
  {
    dists <- data.frame(rowi = 1:length(cols2),coli = cols2) %>% 
      mutate(dist = map2_dbl(rowi,coli,~mat[.x,.y]))
    (.) %>% 
      left_join(dists, by = c("rn"= "rowi"))
  } %>% 
  ggplot(aes(color = dist)) + 
  geom_sf()+
  scale_color_continuous(trans = 'reverse')
first <- net1 %>% 
  mutate(clus = division[vec(mat1 = mat,centers = division)],
         rn = row_number()) %>% 
  st_as_sf()
second <- net1 %E>% 
  left_join(first %>% st_drop_geometry(),by = c("from" = "rn")) %>% 
  left_join(first %>% st_drop_geometry(),by = c("to" = "rn"))
result <- second %>% 
  filter(clus.x != clus.y) %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  select(-from,-to) %>% 
  distinct() %>% 
  as_tbl_graph() %>% 
  mutate(name = as.numeric(name)) %>% 
  left_join(net1 %N>% st_as_sf() %>% mutate(rn = row_number()),by = c("name" = "rn")) %>% 
  as_sfnetwork()
result %E>% convert(to_spatial_explicit) %>%  st_as_sf() %>% mapview() + mapview(first, zcol = "clus")



nds <- result %N>% st_as_sf()
eds <- result %E>% convert(to_spatial_explicit) %>% st_as_sf() %>% st_drop_geometry()

new_edges <- setdiff(expand.grid(from = 1:(nds %>% nrow()),to = 1:(nds %>% nrow())),eds[,c("from","to")]) %>% filter(from < to)

pipe_message = function(.data, status) {message(status); .data}
add_connecting <- function(net,method = "sum"){
  nds <- net %N>% st_as_sf()
  eds <- net %E>% convert(to_spatial_explicit) %>% st_as_sf() %>% st_drop_geometry()
  new_edges <- setdiff(expand.grid(from = 1:(nds %>% nrow()),to = 1:(nds %>% nrow())),eds[,c("from","to")]) %>% filter(from < to)
  i <- 1
  while(nrow(new_edges) > 0){
    if(method == "max"){
      res <- new_edges %>% 
        mutate(metric = map2_dbl(from,to,~net %>% 
                                   add_edges(c(.x,.y)) %>% 
                                   # pipe_message(paste(.x,.y)) %>% 
                                   distances() %>%
                                   max()))
    }else if(method == "sum"){
      res <- new_edges %>% 
        mutate(metric = map2_dbl(from,to,~net %>% 
                                   add_edges(c(.x,.y)) %>% 
                                   # pipe_message(paste(.x,.y)) %>% 
                                   distances() %>%
                                   sum()))
    }else if(method == "gini"){
      res <- new_edges %>% 
        mutate(metric = map2_dbl(from,to,~net %>% 
                                   add_edges(c(.x,.y)) %>% 
                                   # pipe_message(paste(.x,.y)) %>% 
                                   distances() %>%
                                   as.vector() %>% 
                                   {Gini::gini(rep(1,length((.))),(.))}))
    }
    edge <- res %>% arrange(metric) %>% slice(1)
    net <- net %>% add_edges(c(edge$from,edge$to),attr = list(type = i,metric = edge$metric))
    new_edges <- new_edges %>% filter(from != edge$from,from != edge$to,to != edge$from,to != edge$to)
    i <-i + 1
    print(nrow(new_edges))
  }
  return(net %>% as_tbl_graph() %>% as_sfnetwork())
}
res1 <- add_connecting(result)
init_sum <- result %>% distances() %>% sum()
res1 %E>%
  convert(to_spatial_explicit) %>% 
  st_as_sf() %>% 
  mutate(type = ifelse(is.na(type),0,type),
         metric = ifelse(is.na(metric),init_sum,0)) %>% {
           p1 <- (.) %>% 
             ggplot() + 
             geom_sf(mapping = aes(color = type)) + 
             geom_sf(data = res1 %N>% convert(to_spatial_explicit)%>% st_as_sf()) +
             scale_color_continuous(trans = 'reverse')
           p2 <- res1 %>% 
             distances() %>% 
             as.vector() %>% 
             as.data.frame() %>% 
             ggplot(aes(x=.)) + 
             geom_histogram()
           p3 <- result %>% 
             distances() %>% 
             as.vector() %>% 
             as.data.frame() %>% 
             ggplot(aes(x=.)) + 
             geom_histogram()
           p1 + p3 + p2
         }
ggplot() + 
  geom_sf() + 
  geom_sf(data = res1 %N>% convert(to_spatial_explicit)%>% st_as_sf()) + 
  transition_time(type)

res1 %>% 
  convert(to_spatial_explicit)%>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = res1 %E>% convert(to_spatial_explicit)%>% st_as_sf(),mapping = aes(color = type))

res <- new_edges %>% 
  mutate(max_trans = map2_dbl(from,to,~result %>% 
                                add_edges(c(.x,.y)) %>% 
                                pipe_message(paste(.x,.y)) %>% 
                                distances() %>%
                                max()),
         total_trans = map2_dbl(from,to,~result %>% 
                                  add_edges(c(.x,.y)) %>% 
                                  pipe_message(paste(.x,.y)) %>% 
                                  distances() %>%
                                  sum()),
         gini_trans = map2_dbl(from,to,~result %>% 
                                 add_edges(c(.x,.y)) %>% 
                                 pipe_message(paste(.x,.y)) %>% 
                                 distances() %>%
                                 as.vector() %>% 
                                 {Gini::gini(rep(1,length((.))),(.))}))
res %>% arrange(total_trans)
q <- result %>% 
  add_edges(c(53,33),attr = list(new = T)) %>% 
  as_tbl_graph() %>% 
  as_sfnetwork() 
q %>% 
  convert(to_spatial_explicit)%>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = q %E>% convert(to_spatial_explicit)%>% st_as_sf(),mapping = aes(color = new))
