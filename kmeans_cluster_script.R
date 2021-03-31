library(sf)
library(ggplot2)
library(tidyverse)
library(stats)
library(leaflet)
Sys.setlocale("LC_TIME","English")

#****************************************************
#*kmeans clustering
cluster_trips <- function(stop_sf, neighbor_shp, center_quant){
  #* Dynamic of the NYC:pick-up and drop-off together
  #* 
  #Select NYC neighborhood centers which contains more than 500 points as initial clustering centers
  stop_within_flag <- st_contains(neighbor_shp, stop_sf)
  neighbor_shp$stop_count <- map_dbl(stop_within_flag, ~length(.))
  stop_neighborhood <- neighbor_shp[map_dbl(stop_within_flag, ~length(.))>quantile(neighbor_shp$stop_count)[center_quant],]

  stop_nbrhd_centroid <- st_centroid(stop_neighborhood)
  # prepare the data.frame data and matrix data for kmeans clustering
  stop_nbrhd_xy <- as.matrix(st_coordinates(stop_nbrhd_centroid)[,c("X","Y")])
  stop_xy <-  as.matrix(st_coordinates(stop_sf)[,c("X","Y")])
  # kmeans clustering
  stop_cluster <- kmeans(stop_xy, stop_nbrhd_xy)
  # assign clustering result to sf
  stop_sf$cluster_id <- stop_cluster$cluster
  # assign center info
  cluster_centroid <- st_set_crs(st_as_sf(as.data.frame(stop_cluster$centers), coords=c("X","Y")), 4326)
  print(cluster_centroid)
  cluster_centroid <- cluster_centroid %>%
    mutate(cluster_size=stop_cluster$size, cluster_id=1:nrow(cluster_centroid)) %>% arrange(desc(cluster_size))
  size_match <- left_join(as.data.frame(stop_sf)%>%
                            select(cluster_id), as.data.frame(cluster_centroid)%>%
                            select(cluster_id, cluster_size), by="cluster_id")
  stop_sf$cluster_size <- size_match$cluster_size
  return(list(stop_sf, cluster_centroid))
}

#****************************************************
#*plot cluster centroids by leaflet map
centroid_plot <- function(centroid_data, neighbor_shp, scale_rate, mark_col){
  leaflet() %>% setView(lng=-73.93,lat=40.73,zoom = 12) %>%
    addTiles()%>%
    addPolygons(data=neighbor_shp, fill = NA,color = "black", fillOpacity = 0.2, weight = 1.5)%>%
    addCircleMarkers(data = centroid_data, radius = centroid_data$cluster_size*scale_rate, color = mark_col)%>%
    addCircles(data = st_centroid(neighbor_shp), color = "red", weight = 3)
}
#****************************************************
#*cluster plot by ggplot
cluster_plot <- function(stop_sf, neighbor_shp){
  ggplot() + geom_sf(data=stop_sf, col=stop_sf$cluster_id, cex=0.002)+
    geom_sf(data=neighbor_shp,col="black",fill=NA, lwd=0.1)+
    labs( title = waiver(),
          subtitle = waiver(),
          caption =waiver(),
          tag = waiver(),
          x = "Longitude",
          y = "Latitude"
    ) 
}

#****************************************************
#*Load data
# read neighborhood shp file to provide base map and initial clustering centers
neighborhood <- read_sf("Data/NYC_neighborhood/geo_export_19272ca9-c335-4aad-9f0e-b412d69a3c56.shp")
neighborhood <- st_make_valid(st_transform(neighborhood, crs = 4326))

# read the first 1500000 rows which cover more than two-month trips 
raw_trips <- read.csv("Data/2016_Green_Taxi_Trip_Data.csv", nrows = 1500000)
raw_copy <- raw_trips
raw_trips$lpep_pickup_datetime <- strptime(raw_trips$lpep_pickup_datetime,"%m/%d/%Y %I:%M:%S %p")
raw_trips$Lpep_dropoff_datetime <- strptime(raw_trips$Lpep_dropoff_datetime,"%m/%d/%Y %I:%M:%S %p")
raw_trips <- raw_trips%>%
  mutate(trip_duration=difftime(Lpep_dropoff_datetime, lpep_pickup_datetime, units = "secs"),
         pickup_hr=unclass(as.POSIXlt(lpep_pickup_datetime))$hour,
         pickup_wday=unclass(as.POSIXlt(lpep_pickup_datetime))$wday,
         dropoff_hr=unclass(as.POSIXlt(Lpep_dropoff_datetime))$hour,
         dropoff_wday=unclass(as.POSIXlt(Lpep_dropoff_datetime))$wday
         )
raw_trips <- raw_trips%>%select(-c("PULocationID","DOLocationID","Ehail_fee","Store_and_fwd_flag","RateCodeID"))

#****************************************************
#*data pre-processing
# transform time format of 01/02/2016 08:33:59 AM" to standard 24-hour time format
taxi_trips <- raw_trips
# only select trips generated from 2016-01-04 (MON of 1st week) to 2016-01-31 (SUN of the 4th week)
taxi_trips <- taxi_trips%>% 
  filter(taxi_trips$lpep_pickup_datetime >= "2016-01-04 00:00:00 CST" &
                   taxi_trips$lpep_pickup_datetime < "2016-02-01 00:00:00 CST")

# Remove rides with false trips
taxi_trips <- taxi_trips%>% filter (trip_duration > 0 & 
                                      Passenger_count < 4 &Trip_distance > 0)

# Remove rides to and from far away areas
xy_lim = c(-74.25, -73.75, 40.5, 40.95)
taxi_trips <- taxi_trips%>%
  filter(between(Pickup_longitude, xy_lim[1], xy_lim[2]))%>%
  filter(between(Pickup_latitude, xy_lim[3], xy_lim[4]))%>%
  filter(between(Dropoff_longitude, xy_lim[1], xy_lim[2]))%>%
  filter(between(Dropoff_latitude, xy_lim[3], xy_lim[4]))

#********************************************************************************************
#*generate spatial data frames
# generate pickup spatial data.frame
pickup_sf <- st_as_sf(taxi_trips, coords = c("Pickup_longitude", "Pickup_latitude"))
pickup_sf <- st_set_crs(pickup_sf, 4326)
# pickup_sf <- st_cast(st_crop(st_union(pickup_sf), st_union(neighborhood)), "POINT")

# generate drop-off spatial data.frame
dropoff_sf <- st_as_sf(taxi_trips, coords =  c("Dropoff_longitude", "Dropoff_latitude"))
dropoff_sf <- st_set_crs(dropoff_sf, 4326)
# dropoff_sf <- st_cast(st_crop(st_union(dropoff_sf), st_union(neighborhood)), "POINT")

# generate spatial data.frame of taxi stops
pickup_sf <- pickup_sf%>%select(!c("Dropoff_longitude", "Dropoff_latitude"))
dropoff_sf <- dropoff_sf%>%select(!c("Pickup_longitude", "Pickup_latitude"))

parking_sf <- rbind(pickup_sf, dropoff_sf)
#****************************************************
parking_res <- cluster_trips(parking_sf, neighbor_shp = neighborhood, center_quant = 1)
#******
#*leaflet clustering centroid map

#******
#********************************************************************************************
#*morning rush hour
#********************************************************************************************
#*Pickup during morning rush hours
mrpickup_sf <- pickup_sf%>%filter(pickup_hr %in% c(6, 7,8,9) & pickup_wday >0 & pickup_wday < 6)
mrpickup_res <- cluster_trips(mrpickup_sf, neighbor_shp = neighborhood, center_quant = 2)

#***********************************
#*dropoff during morning rush hours
mrdropoff_sf <- dropoff_sf%>%filter(dropoff_hr %in% c(6, 7,8,9) & dropoff_wday >0 & dropoff_wday < 6)
mrdropoff_res <- cluster_trips(mrdropoff_sf, neighbor_shp = neighborhood, center_quant = 2)

#********************************************************************************************
#*evening rush hour
#********************************************************************************************
#*Pickup during evening rush hours
erpickup_sf <- pickup_sf%>%filter(pickup_hr %in%  c(16,17,18, 19) & pickup_wday >0 & pickup_wday < 6)
erpickup_res <- cluster_trips(erpickup_sf, neighbor_shp = neighborhood, center_quant = 2)

#***********************************
#*dropoff during morning rush hours
erdropoff_sf <- dropoff_sf%>%filter(dropoff_hr %in% c(16,17,18, 19) & dropoff_wday >0 & dropoff_wday < 6)
erdropoff_res <- cluster_trips(erdropoff_sf, neighbor_shp = neighborhood, center_quant = 2)

#********************************************************************************************
#*centroid plot of drop_off sf
#*
dropoff_res <- cluster_trips(dropoff_sf, neighbor_shp = neighborhood, center_quant = 2)
centroid_plot(dropoff_res[[2]], neighborhood, scale_rate = 0.0008, mark_col = "darkviolet")
#********************************************************************************************
cluster_plot(parking_res[[1]], neighborhood) 

ggplot() + geom_sf(data=st_centroid(neighborhood), col="blue")+geom_sf(data=neighborhood,col="black",fill=NA, lwd=0.1)+
  labs( title = waiver(),
        subtitle = waiver(),
        caption =waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  ) 
#plot pick-up and drop-off locations
ggplot() + geom_sf(data=parking_sf, cex=0.002)+geom_sf(data=neighborhood,col="black",fill=NA, lwd=0.1)+
  labs( title = waiver(),    
        subtitle = waiver(),
        caption =waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  ) 
#plot taxi ride clusters
ggplot() + geom_sf(data=parking_sf, col=parking_sf$cluster_id, cex=0.002)+
  geom_sf(data=neighborhood,col="black",fill=NA, lwd=0.1)+
  labs( title = waiver(),
        subtitle = waiver(),
        caption =waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  ) 
#plot pick-up locations
ggplot() + geom_sf(data=pickup_sf,  cex=0.002)+
  geom_sf(data=neighborhood,col="black",fill=NA, lwd=0.1)+
  labs( title = waiver(),
        subtitle = waiver(),
        caption =waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  ) 

#plot drop-off locations
ggplot() + geom_sf(data=dropoff_sf,  cex=0.002)+
  geom_sf(data=neighborhood,col="black",fill=NA, lwd=0.1)+
  labs( title = waiver(),
        subtitle = waiver(),
        caption =waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  ) 


ggplot() + geom_sf(data=mrpickup_res[[1]], col=mrpickup_res[[1]]$cluster_id, cex=0.002)+
  geom_sf(data=neighborhood,col="black",fill=NA, lwd=0.1)+
  labs( title = waiver(),
        subtitle = waiver(),
        caption =waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  ) 

#********************************************************************************************
centroid_plot(parking_res[[2]], neighborhood, scale_rate = 0.0005, mark_col = "blue")

centroid_plot(mrpickup_res[[2]], neighborhood, scale_rate = 0.005, mark_col = "green")
centroid_plot(mrdropoff_res[[2]], neighborhood, scale_rate = 0.005, mark_col = "magenta")

centroid_plot(erdropoff_res[[2]], neighborhood, scale_rate = 0.002, mark_col = "magenta")
centroid_plot(erpickup_res[[2]], neighborhood, scale_rate = 0.002, mark_col = "green")

#***********************************
#*morning rush hour together for comparison
leaflet() %>% setView(lng=-73.93,lat=40.73,zoom = 12) %>%
  addTiles()%>%
  addPolygons(data=neighborhood, fill = NA,color = "black", fillOpacity = 0.2, weight = 1.5)%>%
  addCircleMarkers(data = mrpickup_res[[2]], radius = mrpickup_res[[2]]$cluster_size*0.003, color = "green")%>%
  addCircleMarkers(data = mrdropoff_res[[2]], radius = mrdropoff_res[[2]]$cluster_size*0.003, color = "magenta")%>%
  addCircles(data = st_centroid(neighborhood), color = "red", weight = 3)

#***********************************
#*evening rush hour together for comparison
leaflet() %>% setView(lng=-73.93,lat=40.73,zoom = 12) %>%
  addTiles()%>%
  addPolygons(data=neighborhood, fill = NA,color = "black", fillOpacity = 0.2, weight = 1.5)%>%
  addCircleMarkers(data = erpickup_res[[2]], radius = erpickup_res[[2]]$cluster_size*0.002, color = "forestgreen")%>%
  addCircleMarkers(data = erdropoff_res[[2]], radius = erdropoff_res[[2]]$cluster_size*0.002, color = "darkviolet")%>%
  addCircles(data = st_centroid(neighborhood), color = "red", weight = 3)