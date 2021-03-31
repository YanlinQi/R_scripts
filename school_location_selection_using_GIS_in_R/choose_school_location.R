library(sf)
library(ggplot2)
library(tidyverse)


get_ed_enrollment <- function(x, spare_mtx){
  num__students <- c()
  for (i in 1:length(spare_mtx)){
    if (!is_empty(spare_mtx[[i]])){
      num__students[i] <- sum(x[spare_mtx[[i]],]$ENROLLMENT)
    }else{
      num__students[i] <- 0
    }
  }
  return(num__students)
}

get_school_ed <- function(x, spare_mtx){
  school_ed <- c()
  for (i in 1:length(spare_mtx)){
    if (!is_empty(spare_mtx[[i]])){
      school_ed[i] <- x[spare_mtx[[i]],]$DED_NAME
    }else{
      school_ed[i] <- NA
    }
  }
  return(school_ed)
}

#***********************
# read ed shape and calculate pop_change
dublin_eds<-read_sf('Data/DublinEDs.shp')
dublin_eds$pop_change <- dublin_eds$POP06 - dublin_eds$POP02
#***********************
# read school information and transform csv to spatial dataframe
schools <- read.csv("Data/Dublin_PrimarySchools.csv")
sf_schools <- st_as_sf(schools, coords = c("EASTING_", "NORTHING"))
#***********************
# read road shp
dublin_roads<-read_sf("Data/DublinRoads.shp")
#***********************
#*plot raw spatial data distribution map
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = sf_schools, aes(lty="Primary School"), cex=0.5) +
  geom_sf(data = dublin_roads, aes(color="Main Roads")) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  labs( title = "Map of EDs, mainroads and school locations in Dublin",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
#***********************
#*plot pop change map
ggplot()+geom_sf(data = dublin_eds, aes(fill=pop_change)) +
  scale_fill_continuous(name="POP_change", low = "navy",high = "cyan")+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  geom_sf(data = sf_schools, color="red", cex=0.5) +
  geom_sf(data = dublin_roads, color="green") +
  labs( title = "Map of population change from 2002 to 2006 in Dublin",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
#***********************
#*plot enrollment supply of individual schools
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = sf_schools, aes(colour=ENROLLMENT), cex=1) +
  scale_color_continuous(name="Enrollment Supply", low = "cyan",high = "orangered")+
  geom_sf(data = dublin_roads, color="green") +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  labs( title = "Map of Enrollment supply of individual schools in Dublin",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )

#***********************
# calculate enrollment supply for each ed
spare_mtx_enroll <- st_intersects(dublin_eds, sf_schools)
dublin_eds$num_enrollment <- get_ed_enrollment(sf_schools, spare_mtx_enroll)
#***********************
#*plot county enrollment supply
ggplot()+geom_sf(data = dublin_eds, aes(fill=num_enrollment)) +
  scale_fill_continuous(name="Enrollment Supply", low = "olivedrab",high = "yellow")+
  geom_sf(data = dublin_roads, color="green") +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  labs( title = "Map of enrollment supply of EDs in Dublin",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
#***********************
#*calculate ed enrollment needs
dublin_eds$enroll_needs <- (dublin_eds$num_enrollment/dublin_eds$POP02-
                              dublin_eds$num_enrollment/dublin_eds$POP06)*dublin_eds$POP06
dublin_eds_for_enroll <- dublin_eds%>%filter(enroll_needs>0)%>%arrange(desc(enroll_needs))
#***********************
#* plot ed enrollment needs
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = dublin_eds_for_enroll, aes(fill=enroll_needs)) +
  scale_fill_continuous(name="Enrollment Need", low = "cyan",high = "orangered")+
  geom_sf(data = dublin_roads, color="green") +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  labs( title = "Map of enrollment needs of EDs in Dublin",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
#***********************
#*filter eds without "MULTI DENOMINATIONAL"
spare_mtx_school_ed <- st_intersects(sf_schools, dublin_eds_for_enroll)
sf_schools$ed_name <- get_school_ed(dublin_eds_for_enroll, spare_mtx_school_ed)
sf_schools_nomulti <- sf_schools%>%filter(DENOMINATI!="MULTI DENOMINATIONAL")
dublin_eds_for_enroll_no_multi <-dublin_eds_for_enroll%>%filter(DED_NAME%in%sf_schools_nomulti$ed_name)

#*******************************
#*filter eds near mainroads

# plot roads, buffer and eds intersect with buffer
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = dublin_eds_for_enroll_no_multi, aes(fill=enroll_needs)) +
  scale_fill_continuous(name="Enrollment Need", low = "cyan",high = "orangered")+
  geom_sf(data = dublin_roads, color="green") +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  labs( title = "Map of enrollment need of EDs without multi_denominational schools",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
#*******************************
# calculate roads' buffer
roads_buf <- st_union(st_buffer(dublin_roads,dist=2000))
#*******************************
#* calculate intersection of road buffer and dublin_eds_for_enroll_no_multi
dublin_eds_for_enroll_no_multi_buffer <- st_intersection(dublin_eds_for_enroll_no_multi, roads_buf)
#*******************************
#*plot road buffer
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = dublin_eds_for_enroll_no_multi, aes(fill=enroll_needs)) +
  scale_fill_continuous(name="Enrollment Need", low = "cyan",high = "orangered")+
  geom_sf(data = dublin_roads, color="green") +
  geom_sf(data = roads_buf, fill=alpha("green", alpha = 0.5), lwd=0.2)+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))+
  labs( title = "Map of enrollment need of EDs without multi_denominational schools",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
#*******************************
final_eds_for_school <- dublin_eds_for_enroll_no_multi_buffer
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = final_eds_for_school, aes(fill=enroll_needs)) +
  scale_fill_continuous(name="Enrollment Need", low = "cyan",high = "orangered")+
  geom_sf(data = dublin_roads, color="green")+
  geom_sf(data = roads_buf, fill=alpha("green", alpha = 0.05), lwd=0.2)+
  theme(axis.text.x=element_text(angle=90,vjust = 0.5, hjust=1))+
  labs( title = "Map of candidate regions for new nulti-denominational schools",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )

#*******************************
#*plot top3_locations
final__topeds_for_school <- dublin_eds_for_enroll_no_multi_buffer%>%arrange(desc(enroll_needs))
final__top3eds_for_school <- final__topeds_for_school[1:3,]
ggplot()+geom_sf(data = dublin_eds, fill="white") +
  geom_sf(data = final__top3eds_for_school, aes(fill=enroll_needs)) +
  scale_fill_continuous(name="Enrollment Need", low = "cyan",high = "orangered")+
  geom_sf(data = dublin_roads, color="green")+
  geom_sf(data = sf_schools, color="blue", cex=0.1) +
  theme(axis.text.x=element_text(angle=90,vjust = 0.5, hjust=1))+
  labs( title = "Map of top-3 regions for new nulti-denominational schools",
        subtitle = waiver(),
        caption = waiver(),
        tag = waiver(),
        x = "Longitude",
        y = "Latitude"
  )
