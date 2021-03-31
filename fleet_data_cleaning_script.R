library(tidyverse)
library(ggplot2)
fleet_data <- as_tibble(read_csv('Data/fleet-asset-management-system-open-data-2015-2019.csv'))
# fleet_data <- fleet_data%>%filter(Acquisition_Delivery_Date!="Null")

library(tidyverse)
library(ggplot2)
library(magrittr)
fleet_data <- fleet_data %>% mutate_all(~replace(., .=="Null"|.=="No Data","NA"))
fleet_data_test<- fleet_data%>%filter(Equipment_Number!="NA")

fleet_data_test <- mutate(fleet_data_test,fleet_data_test%>%
                            select(Model_Year, Report_Year, Payload_Rating, Shipping_Weight,Disposition_Mileage,
                                   Disposition_Sold_Amount, Total_Miles)%>%
                            mutate_all(parse_number))

fleet_data_test$Purchase_Price<- fleet_data_test$Purchase_Price%>%parse_number()
fleet_data_test <- fleet_data_test%>%
  rename(Purchase_Price_in_Dollar=Purchase_Price)

fleet_data_test <- fleet_data_test %>%
  mutate(Disposed=case_when(Disposed == "Yes" ~ "TRUE",
                            Disposed == "No" ~ "FALSE",
                            TRUE ~ NA_character_),
         Passenger_Vehicle=case_when(Passenger_Vehicle == "Yes" ~ "TRUE",
                                     Passenger_Vehicle == "No" ~ "FALSE",
                                     TRUE ~ NA_character_))
fleet_data_test$Disposed<- fleet_data_test$Disposed%>%as.logical()
fleet_data_test$Passenger_Vehicle<- fleet_data_test$Passenger_Vehicle%>%as.logical()

transform_date <- function(x) {
  i <- 1
  for (x_i in x) {
    if(str_detect(x_i, "/",negate = TRUE) & x_i!="NA"){
      x_i <- as.numeric(x_i)
      x_i <- as.Date(x_i,origin="1900-01-01")
      x_i <- format(x_i,"%m/%d/%Y")
      x_i <- as.character(x_i)
      x[i] <- x_i
      # print(x[i])
    }
    i <- i+1
  }
  return(x)
}

parse_item <- function(x){
  i <- 1
  for (x_i in x) {
    if(str_detect(x_i, "/") & x_i!="NA"){
      x_i <- as.numeric(x_i)
      x_i <- as.Date(x_i,origin="1900-01-01")
      x_i <- format(x_i,"%m/%d/%Y")
      x_i <- as.character(x_i)
      x[i] <- x_i
      # print(x[i])
    }
    i <- i+1
  }
  return(x)
}

fleet_data_test$Acquisition_Delivery_Date <- transform_date(fleet_data_test$Acquisition_Delivery_Date)
fleet_data_test$Disposition_Date <- transform_date(fleet_data_test$Disposition_Date)

fleet_data_test <- fleet_data_test%>% separate(Acquisition_Delivery_Date, into = c("Acquisition_Month","Acquisition_Day","Acquisition_Year"), convert = TRUE)
fleet_data_test <- fleet_data_test%>%separate(Disposition_Date, into = c("Disposition_Month","Disposition_Day","Disposition_Year"), convert = TRUE)

fleet_data_test <- mutate(fleet_data_test,fleet_data_test%>%
                            select(Acquisition_Month, Acquisition_Day, Acquisition_Year, Disposition_Month,Disposition_Day, Disposition_Year)%>%
                            mutate_all(as.numeric))
############
transfrom_agency <- function(x){
  # cat(c("x is ", x,"\n"), sep = " ")
  i <- 1
  for(x_i in x){
    if ("of" %in% unlist(str_split(x_i, pattern = " "))){
      x_i <- paste("Dept. of",unlist( str_split(x_i, pattern = ","))[1], sep = " ")
      x[i] <- x_i
    }
    # cat(c("x_i changes to ", x_i,"\n"), sep = " ")
    i <- i+1
  }
  return(x)
}
fleet_data_test[["Agency"]] <- fleet_data_test$Agency%>%transfrom_agency()
write_csv(fleet_data_test, "Data/fleet_data_processed.csv")

####================3
fleet_count <- fleet_data_test %>% filter(Report_Year==2018)%>%distinct(Equipment_Number, .keep_all = TRUE)%>%
  group_by(Agency) %>% 
  summarise(fleet_size=n())%>%
  arrange(desc(fleet_size))
ggplot(fleet_count[1:10,], aes(x = Agency, y = fleet_size))+
  geom_bar(stat = "identity", fill = "steelblue", colour = "white",position=position_dodge(0.1), width=0.6)+
  geom_text(aes(label=fleet_size[1:10]), vjust=1.6, color="white", size=2.0)+
  theme(axis.text.x=element_text(angle=90, hjust=1))
####================4
fleet_data_distinct <- fleet_data_test%>%distinct(Equipment_Number, .keep_all = TRUE)
veh_acq_per_months <- fleet_data_distinct%>% 
  group_by(Acquisition_Month)%>% 
  summarise(veh_acquisition_per_month=n())%>%
  arrange(Acquisition_Month)
veh_acq_per_months <- veh_acq_per_months[1:12,]
ggplot(veh_acq_per_months, aes(x = Acquisition_Month, y = veh_acquisition_per_month))+
  geom_bar(stat = "identity", fill = "steelblue", colour = "white",position=position_dodge(0.1), width=0.6)+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10","11", "12"))+
  geom_text(aes(label=veh_acquisition_per_month), vjust=1.6, color="white", size=2.0)
####================5
top_makes <- fleet_data_distinct%>%group_by(Make_Model)%>%
  summarise(Make_count=n())%>%
  arrange(desc(Make_count))

cat(c(top_makes$Make_Model[1], "is the most popular make of vehicles in state fleets."),
    c("The count number of this make is", top_makes$Make_count[1]), sep = " ")
####================6
fleet_data_test1 <- fleet_data_distinct
fleet_data_test1[["serve_year"]] <- fleet_data_distinct[["Disposition_Year"]]-
  fleet_data_distinct[["Acquisition_Year"]]

average_serve_year <- fleet_data_test1$serve_year%>%mean(.,na.rm=TRUE)
cat(c("In state fleets, a vehicle serves", round(average_serve_year,2), "years on average."), sep = " ")

avg_serve_by_Agency <- fleet_data_test1%>%group_by(Agency)%>%summarise(avg_serve_year=mean(serve_year, na.rm=TRUE))
std_serve_year_by_Agency <- var(avg_serve_by_Agency$avg_serve_year, na.rm=TRUE) 
if(std_serve_year_by_Agency==0){
  cat(c("The variance of the years that a vehicle serves on average in different fleets is", 
        std_serve_year_by_Agency, "which means there is no difference."), sep = " ")
} else{
  cat(c("The variance of the years that a vehicle serves on average in different fleets is", 
        round(std_serve_year_by_Agency,2), 
        "which is much larger than 0. Hence, the serving years are different in different fleets."), sep = " ")
}
####================7
library(zipcodeR)
library(ggplot2)
library(maps)
library(dplyr)
fleet_count_by_postal <- fleet_data_test %>% filter(Report_Year==2019)%>% 
  distinct(Equipment_Number,.keep_all = TRUE) %>% 
  group_by(Postal_Code) %>% 
  summarise(fleet_size=n()) %>%
  arrange(desc(fleet_size))
fleet_count_by_postal <- fleet_count_by_postal%>% rename(zipcode=Postal_Code)
zipcode_db <- as.tibble(zip_code_db)%>%select(zipcode, county, state, lat, lng, bounds_west, bounds_east, bounds_north, bounds_south)

sp_data <- left_join(fleet_count_by_postal, zipcode_db, by="zipcode")
sp_data_for_plot <- sp_data
sp_data_for_plot$fleet_size <- log(sp_data_for_plot$fleet_size)
usa_county <- map_data("county")
ca_county <- usa_county%>%filter(region=="california")

ggplot(data=ca_county)+
  geom_polygon(mapping=aes(x=long, y=lat, group=group),fill="white", color="black")+geom_point(data=sp_data_for_plot, mapping=aes(x=lng, y=lat,colour=fleet_size))+
  coord_fixed(1.3)+
  scale_color_continuous(name="Fleet_size by Zipcode", 
                         breaks=c(log(1),log(50),log(100),log(300),log(600)), 
                         labels=c(as.character(exp(log(1))),as.character(exp(log(50))), as.character(exp(log(100))),as.character(exp(log(300))), as.character(exp(log(600)))),
                         low = "cyan",high = "tomato")

fleet_subset <- fleet_data_test %>% select(Agency,Equipment_Number,Postal_Code,Report_Year)
fleet_subset <- fleet_subset[complete.cases(fleet_subset),]

fleet_subset <- fleet_subset %>% filter(Report_Year==2019) %>% distinct(Equipment_Number,.keep_all = TRUE)

fleet_zip <- fleet_subset %>% group_by(Postal_Code) %>% summarize(Fleet_size=n()) %>% arrange(desc(Fleet_size))

fleet_zip <- fleet_zip %>% filter(Postal_Code!="8kia730" & Postal_Code!="Not Applicable")

get_county <- function(x){
  County=c()
  i=1
  
  for (val in x)
  {
    County[i] <- reverse_zipcode(val)$county
    i=i+1
  }
  return(County)
}

fleet_zip <- fleet_zip %>% mutate(County=get_county(fleet_zip$Postal_Code))

fleet_county <- fleet_zip %>% 
  group_by(County) %>% 
  summarize(Fleet_total=sum(Fleet_size))  %>% 
  arrange(desc(Fleet_total))


CA_county <- map_data("county") %>% filter(region=="california")

fleet_county <- fleet_county %>% mutate(County=str_to_lower(County)) %>% 
  mutate(County=word(County,1,-2))

fleet_county_count <- CA_county %>% left_join(fleet_county,by=c("subregion"="County"))
fleet_county_count_for_plot <- fleet_county_count
fleet_county_count_for_plot$Fleet_total <- log(fleet_county_count$Fleet_total)
ggplot(fleet_county_count_for_plot)+geom_polygon(aes(x=long,y=lat,group=group,fill=Fleet_total))+
  coord_fixed(1.3)+
  scale_fill_continuous(name="Fleet_size by County", 
                        breaks=c(1,log(50),log(100),log(200),log(400),log(1000),log(2000),log(4000), log(8000)), labels=c("1",as.character(exp(log(50))),as.character(exp(log(100))),as.character(exp(log(200))), as.character(exp(log(400))),as.character(exp(log(1000))),as.character(exp(log(2000))),as.character(exp(log(4000))), as.character(exp(log(8000)))),
                        low = "cyan",high = "red")