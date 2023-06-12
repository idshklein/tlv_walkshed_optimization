library(tidyverse)
library(sf)
library(crsuggest)
library(lwgeom)
library(units)

q <- st_read("C:/Users/idshk/OneDrive/idos_shit/jlm_bike_path.shp") %>% st_zm() 
q[1,] %>% st_geometry() %>%  plot()
st_split(q[1,],q[1,] %>% 
  st_line_sample(sample = c(10,100,200,300,500)/as.numeric(st_length(.))) %>% st_snap(q[1,],tolerance = 1) ) %>% 
  st_collection_extract(type = "LINESTRING") %>%
  mutate(len = st_length(geometry)) %>% 
  plot()
q1 <- q[1,] %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  mutate(distance = pmap_dbl(list(X,Y,lead(X),lead(Y)), ~data.frame(c(..1,..3),c(..2,..4)) %>% dist()),
         distance = lag(distance) %>% coalesce(0),
         azimuth_rad = pmap_dbl(list(X,Y,lead(X),lead(Y)), ~atan((..4-..2)/(..3-..1)) %>% as_units("radians")),
         azimuth_deg = pmap_dbl(list(X,Y,lead(X),lead(Y)), ~atan((..4-..2)/(..3-..1)) %>% as_units("radians") %>% set_units("degrees")),
         cumdist = cumsum(distance)) %>% 
  filter(lag(cumdist) <30|is.na(lag(cumdist))) %>% 
  slice((nrow(.)-1):nrow(.))

add_to_x <- function(desired_distance, current_distance,radian){
  return((desired_distance - current_distance)*cos(radian))
}
add_to_y  <- function(desired_distance, current_distance,radian){
  return((desired_distance - current_distance)*sin(radian))
}
add_to_distance <- function(desired_distance, current_distance){
  return(desired_distance - current_distance)
}
add_row <- function(row,desired_distance){
  return(data.frame(X= row$X + add_to_x(desired_distance, row$cumdist,row$azimuth_rad),
                    Y= row$Y + add_to_y(desired_distance, row$cumdist,row$azimuth_rad),
                    L1 = 1,
                    distance = add_to_distance(desired_distance,row$cumdist),
                    azimuth_rad = row$azimuth_rad,
                    azimuth_deg = row$azimuth_deg,
                    cumdist = desired_distance))
}
q1 %>% bind_rows(add_row(q1[1,],30)) %>% arrange(cumdist)%>% st_as_sf(coords = c("X","Y"),crs = 2039) %>% ggplot()+geom_sf_label(mapping = aes(label = round(cumdist)))

((((10 - q1$cumdist) * cos(q1$azimuth_rad))^2 + ((10 - q1$cumdist) * sin(q1$azimuth_rad))^2)^0.5) == (10 - q1$cumdist)



