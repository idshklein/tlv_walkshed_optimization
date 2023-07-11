pacman::p_load(tidyverse,hms,gtfsrouter,gtfstools,data.table,mapview)
agency <- tibble(agency_id = 1,
                 agency_name = "1",
                 agency_url = "1",
                 agency_timezone = "Asia/Jerusalem") %>% 
  as.data.table()
calendar <- tibble(service_id = 1,
                   monday=1,
                   tuesday=1,
                   wednesday=1,
                   thursday=1,
                   friday=1,
                   saturday=1,
                   sunday=1,
                   start_date = "20230101",
                   end_date = "20300101") %>% 
  as.data.table()
routes <- tibble(route_id = lines_alg$name,
                 route_short_name = route_id,
                 route_long_name= route_id,
                 route_type = "0") %>% 
  as.data.table()
shapes <- lines_alg %>% st_coordinates() %>% 
  as_tibble() %>% 
  `colnames<-`(c("shape_pt_lon","shape_pt_lat","shape_id")) %>% 
  group_by(shape_id) %>% 
  mutate(shape_pt_sequence = row_number()) %>% 
  ungroup() %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  as.data.table()
trips <- tibble(route_id = lines_alg$name,
                service_id = 1,
                trip_id = route_id) %>% 
  rowid_to_column("shape_id") %>% 
  mutate(shape_id = as.character(shape_id)) %>% 
  as.data.table()

stop_times <- tibble(trip_id = lines_alg$name %>% rep(each = 2),
                     arrival_time = hms(minutes = c(rbind(0,3600*(lines_alg$len/1000)/25/60))) %>%round_hms(1) %>% as.character(),
                     departure_time= arrival_time
                     ) %>% 
  group_by(trip_id) %>% 
  mutate(stop_sequence = row_number(),
         stop_id = map2_chr(trip_id,stop_sequence,~str_split(.x,"-",simplify = T)[[.y]])) %>% 
  as.data.table()

frequencies <- tibble(trip_id = lines_alg$name,
                      start_time= "00:00:00",
                      end_time= "23:59:59",
                      headway_secs = as.integer(180)) %>% 
  as.data.table()
stops <- tibble(stop_name = stops_alg$values,
                stop_lat = stops_alg %>% st_coordinates() %>% `[`(,2),
                stop_lon = stops_alg %>% st_coordinates() %>% `[`(,1))%>% 
  rowid_to_column("stop_id") %>% 
  select(c("stop_id","stop_name","stop_lat","stop_lon")) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  as.data.table()

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
gtfs$agency <- agency
gtfs$calendar <- calendar
gtfs$frequencies <- frequencies
gtfs$routes <- routes
gtfs$shapes <- shapes
gtfs$stop_times <- stop_times
gtfs$stops <- stops
gtfs$trips <- trips
frequencies_to_stop_times(gtfs)
gtfs %>% gtfstools::get_trip_geometry(file = "shapes") %>% mapview()
