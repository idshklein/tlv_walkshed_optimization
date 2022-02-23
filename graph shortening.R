library(tidyverse)
library(sf)
library(sfnetworks)
library(nngeo)
library(igraph)
sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10),c(0,0)))))
net <- st_make_grid(sfc,square = T,n = c(10,10)) %>% 
  st_cast("LINESTRING") %>% 
  st_segments() %>% 
  st_sf() %>% 
  as_sfnetwork(directed = F)
nodes1 <- net %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  mutate(rn = factor(row_number()))
edges1 <- net %>% 
  activate(edges) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  select(from,to) %>% 
  mutate_all(factor) %>% 
  mutate(type = "orig")
transfers1 <- sfnetwork(nodes1,edges1) %>% distances()
dists1 <- sfnetwork(nodes1,edges1,crs = 2039) %>% 
  st_network_cost()
new_edges <- setdiff(expand.grid(from = nodes1$rn,to = nodes1$rn),edges1[,-3])

net %>% distances() %>% sum()
net %>% distances() %>% max()
net %>% distances() %>% hist()
i <- 1
while(nrow(new_edges) > 0){
  # for(i in 1:40){
  new_edge <- new_edges %>% 
    filter(from != to) %>% 
    mutate(diameter = map2_dbl(from,to,function(x,y){
      row1 <- data.frame(from = x, to = y,type = "new")
      bind_rows(edges1,row1) %>% 
        as_tbl_graph(directed = F) %>% 
        distances() %>% 
        sum()
    })) %>% 
    arrange(diameter) %>% 
    slice(1) %>% 
    select(-diameter) %>% 
    mutate(type = "new")
  edges1 <- edges1 %>% bind_rows(new_edge)
  new_edges <- new_edges %>% 
    filter(from != new_edge$from,
           from != new_edge$to,
           to != new_edge$from,
           to != new_edge$to)
  print(paste0("i:",i))
  print(new_edge)
  print(nrow(edges1))
  print(nrow(new_edges))
  as_tbl_graph(edges1,directed = F) %>% distances() %>% sum() %>% print()
  i <- i + 1
}
transfers2 <- sfnetwork(nodes1,edges1) %>%distances()
dists2 <- sfnetwork(nodes1,edges1,crs = 2039) %>% 
  st_network_cost()
data.frame(dist = c(as.vector(dists1), as.vector(dists2)),group = factor(c(rep(1,14641),rep(2,14641)))) %>% 
  ggplot(aes(x = dist,color = group,group = group)) + 
  geom_density(position = "dodge")
identical(dists2,transfers2)
data.frame(dist = c(as.vector(dists1), as.vector(dists2)),group = factor(c(rep(1,14641),rep(2,14641)))) %>% 
  group_by(group) %>% 
  mutate(rn  =row_number()) %>% 
  spread(group,dist) %>% 
  ggplot(aes(x = `1`,y = `2`)) + 
  geom_point() + 
  geom_abline(slope = 1,intercept = 0)
data.frame(dist = c(as.vector(transfers1), as.vector(transfers2)),group = factor(c(rep(1,14641),rep(2,14641)))) %>% 
  group_by(group) %>% 
  mutate(rn  =row_number()) %>% 
  spread(group,dist) %>% 
  ggplot(aes(x = `1`,y = `2`)) + 
  geom_jitter() + 
  geom_abline(slope = 1,intercept = 0)

# facet_wrap(~group,ncol = 1)
sfnetwork(nodes1,edges1) %>% distances() %>% sum()

sfnetwork(nodes1,edges1) %>%distances() %>% max()
sfnetwork(nodes1,edges1) %>% distances() %>% hist()
sfnetwork(nodes1,edges1) %>%distances() %>% table() %>% cumsum() %>% `/`(sfnetwork(nodes1,edges1) %>%distances() %>% length())
sfnetwork(nodes1,edges1) %>%distances() %>% `==`(4) %>% sum()
sfnetwork(nodes1,edges1) %>% 
  convert(to_spatial_explicit) %>%  
  {
    nds <- (.) %>% activate(nodes) %>% st_as_sf()
    eds <- (.) %>% activate(edges) %>% st_as_sf()
    ggplot() + 
      geom_sf(data = eds,mapping = aes(color = type)) + 
      geom_sf(data = nds)
  }




