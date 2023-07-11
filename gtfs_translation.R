pacman::p_load(tidyverse,hms,gtfsrouter,gtfstools,data.table,mapview,lubridate,gtfs2gps,gganimate)
agency <- tibble(agency_id = "1",
                 agency_name = "1",
                 agency_url = "http://notintersting.com",
                 agency_timezone = "Asia/Jerusalem") %>% 
  as.data.table()
calendar <- tibble(service_id = "1",
                   monday=1,
                   tuesday=1,
                   wednesday=1,
                   thursday=1,
                   friday=1,
                   saturday=1,
                   sunday=1,
                   start_date = ymd("2023-01-01"),
                   end_date = ymd("2030-01-01")) %>% 
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
                service_id = "1",
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
         stop_id = map2_chr(trip_id,stop_sequence,~str_split(.x,"-",simplify = T)[[ifelse(1==.y,2,1)]])) %>% 
  as.data.table()

frequencies <- tibble(trip_id = lines_alg$name,
                      start_time= "00:00:00",
                      end_time= "23:59:59",
                      headway_secs = as.integer(180)) %>% 
  as.data.table()
stops <- tibble(stop_id = stops_alg$values,
                stop_name = stop_id,
                stop_lat = stops_alg %>% st_coordinates() %>% `[`(,2),
                stop_lon = stops_alg %>% st_coordinates() %>% `[`(,1))%>% 
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
gtfs <- frequencies_to_stop_times(gtfs)
write_gtfs(gtfs,"ashdod_gtfs.zip")
# gtfs %>% gtfstools::get_trip_geometry(file = "shapes") %>% mapview()
gtfs %>% gtfstools::get_trip_length(file = "shapes") %>% pull(length) %>% sum() %>% `*`(365) %>% `/`(230000)
gtfs %>%gtfstools::get_trip_speed()
# validate_gtfs(gtfs,"C:/Users/idshk/Downloads/valid","C:/Users/idshk/Downloads/valid/gtfs-validator-v4.0.0.jar")
timer <- gtfs2gps(gtfs)
timer%>% filter(timestamp<as.ITime("01:00:00")) %>% st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4326) %>% st_write("anim.gpkg",delete_layer =T)

