library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('alluvial') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('xgboost') # modelling
library('caret') # modelling


# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

#argument ... -> function that takes an arbitrary number of arguments
#ex. c function (...) -> c(a=42, 73.9, c=NA)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

train <- as_tibble(fread("/Users/anastazijaverovic/Desktop/NYC_TAXI_TRIP_DURATION_PREDICTION_1/train.csv"))
test <- as_tibble(fread("/Users/anastazijaverovic/Desktop/NYC_TAXI_TRIP_DURATION_PREDICTION_1/test.csv"))
sample_submit <- as_tibble(fread("/Users/anastazijaverovic/Desktop/NYC_TAXI_TRIP_DURATION_PREDICTION_1/sample_submission.csv"))


#datasets overview, first train then test
summary(train)
glimpse(train)

summary(test)
glimpse(test)

summary(sample_submit)
glimpse(sample_submit)

#missing values in data
sum(is.na(train))
sum(us.na(test))

#combining train and test datasets for consistency checks

#rbind - append two dataframes with the same number of columns together
#%>% as pipe: train %>% mutate(dset = "train") == mutate(train,dset="train")
#mutate is used to create new variable from the data
#new variable is named dset and has the value: train or test

#combine <- bind_rows(train %>% mutate(dset = "train"))
#same as: combine <- bind_rows(mutate(train, dset = "train"))

combine <- bind_rows(train %>% mutate(dset = "train"),
                     test %>% mutate(dset = "test",
                                     dropoff_datetime = NA,
                                     trip_duration = NA))
combine <- combine %>% mutate(dset = factor(dset))

#date and time - from characters to date

#vendor_id as factor

train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))

#consistency check if trip_durations = dropoff_datetime - pickup_datetime
#if consistent - TRUE

#int_length - get the length of an interval in seconds
#interval() - creates an interval object with start and end dates
#if start date occurs before the end date - interval() > 0, else < 0

#ex. if dropoff happened after pickup -> < 0
#ex. interval(dropoff - pickup) = -1h
#ex. trip duration = 1h
#ex. 1h + 1h = 0 > 0  FALSE

train %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime))
                     + trip_duration) > 0) %>%
  select(check, pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()

#individual feature visualisations

set.seed(1234)    #seed of random number generator
foo <- sample_n(train, 8e3)   #get random numbers

#1 pickup longitude, latitude visualization
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "yellow", fillOpacity = 1)


#2 taxi trip duration distribution

train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "blue", bins = 150) +
  scale_x_log10() +
  labs(x = "Trip duration", y = "Number of rides")


#2.1 select top 10 biggest trip durations (measured in seconds)

train %>%
  arrange(desc(trip_duration)) %>%
  select(trip_duration, pickup_datetime, dropoff_datetime, everything()) %>%
  head(10)

#2.2 trip durations by days

train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday) %>%
  summarize(median_duration = median(trip_duration/60)) %>%
  ggplot(aes(wday,median_duration)) +
  geom_point(size = 4, colour = "blue") +
  labs(x = "Day of the week", y = "Median trip duration [minutes]")

#2.3 trip durations by hours in the day

train %>%
  mutate(hour = hour(pickup_datetime)) %>%
  group_by(hour) %>%
  summarize(median_duration = median(trip_duration/60)) %>%
  ggplot(aes(hour,median_duration)) +
  geom_line(size = 1.5, colour = "blue") +
  labs(x = "Hour of the day", y = "Median trip duration [minutes]")

#2.4 trip durations by days in the week and times of the day

train %>%
  group_by(wday, hour) %>%
  summarize(median_duration = median(trip_duration/60)) %>%
  ggplot(aes(hour, wday, fill = median_duration)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral")

#2.5 trip durations and passenger count

train %>%
  group_by(passenger_count) %>%
  summarize(median_duration = median(trip_duration/60)) %>%
  ggplot(aes(passenger_count, median_duration)) +
  geom_point()

#2.6 trip duration densities by vendors

train %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_density(position = "stack") +
  scale_x_log10()

#2.7 mean and median values of trip duration by each vendor

train %>%
  group_by(vendor_id) %>%
  summarise(median = median(trip_duration),
            mean = mean(trip_duration))

#2.8 no connection to server flag connection to trip duration

train %>%
  ggplot(aes(store_and_fwd_flag, dur = (trip_duration/60))) +
  geom_bar()

#3 pickup and dropoff dates

#3.1 pickup and dropoff dates distributions over the year

p1 <- train %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120) +
  labs(x = "Pickup dates", y="Number of rides")

p2 <- train %>%
  ggplot(aes(dropoff_datetime)) +
  geom_histogram(fill = "blue", bins = 120) +
  labs(x = "Dropoff dates", y="Number of rides")

layout <- matrix(c(1,2),2,1,byrow = FALSE)
multiplot(p1, p2, layout = layout)

#3.2 insight into drop between January and February in pickup, dropoff times

train %>%
  filter(pickup_datetime > ymd("2016-01-20") & pickup_datetime < ymd("2016-02-10")) %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120)

#3.3 pickup distributions by days of the week

p3 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")

#3.4 pickup distributions by times of the day

p4 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Time of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

#3.5 pickup distributions by months and times of the day

train %>%
  mutate(hpick = hour(pickup_datetime),
         month = factor(month(pickup_datetime))) %>%
  group_by(hpick, month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = month)) +
  geom_line(size = 1.5)

#3.6 pickup distributions by days of the week and times of the day

train %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5)


#4 passenger count

p5 <- train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")

#5 vendor

train %>%
  ggplot(aes(vendor_id, fill = vendor_id)) +
  geom_bar()

#6 no connection to server

#6.1 no connection to server - trip information was held in vehicle memory if flag = Y

p6 <- train %>%
  ggplot(aes(store_and_fwd_flag, fill = store_and_fwd_flag)) +
  geom_bar() +
  scale_y_log10()

layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)

#6.2 trip information held in vehicle memory by vendor

train %>%
  group_by(vendor_id, store_and_fwd_flag) %>%
  count()

#6.3 trip information held in vehicle memory and trip duration correlation

train %>%
  ggplot(aes(store_and_fwd_flag, trip_duration, color = store_and_fwd_flag)) +
  geom_boxplot() +
  scale_y_log10()

#7 number of passengers and trip duration correlation

train %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(y = "Trip duration [s]", x = "Number of passengers")


#feature engineering - building new features from existing ones

#1 jfk and la guradia airports coordinates (from Wikipedia)

jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

#2 pickup and dropoff coordinates

pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)

drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)

#3 distance and direction between pickup and dropoff coordinates

#distCosine - the shortest distance between two points on a spherical earth

train$dist <- distCosine(pick_coord, drop_coord)

train$bearing = bearing(pick_coord, drop_coord)

#3.1 distances between pickup and dropoff locations and airports

train$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)

train$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)

train$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)

train$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)


#add columns

train <- train %>%
  
  mutate(
    
    speed = dist/trip_duration*3.6,
    
    date = date(pickup_datetime),
    
    month = month(pickup_datetime, label = TRUE),
    
    wday = wday(pickup_datetime, label = TRUE),
    
    wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
    
    hour = hour(pickup_datetime),
    
    work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
    
    #| performs element-wise operation producing result having length of the longer operand
    jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3), #2e3 is the same as 2 x 10^3 = 2000
    
    lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
    
    blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )


#3.2 direct distances and trip duration correlation

set.seed(4321)

train %>%
  sample_n(5e4) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

#removing extreme values

train %>%
  filter(trip_duration > 120 & trip_duration < 3600) %>%
  filter(dist > 100 & dist < 100e3) %>%
  sample_n(5e4) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

#3.3 travel speed distribution

train %>%
  filter(speed > 2 & speed < 1e2) %>%
  ggplot(aes(speed)) +
  geom_histogram(fill = "red", bins = 50) +
  labs(x = "Average speed [km/h] (direct distance)", y = "Number of drives")


#3.4 average speed per day of the week

train %>%
  group_by(wday) %>%
  summarize(median_speed = median(speed)) %>%
  ggplot(aes(wday,median_speed)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median speed [km/h]")

#3.5 average speed per time of the day

train %>%
  group_by(hour) %>%
  summarize(median_speed = median(speed)) %>%
  ggplot(aes(hour,median_speed)) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Median speed [km/h]")

#3.6 average speed per day and time of the week

train %>%
  group_by(hour, wday) %>%
  summarize(median_speed = median(speed)) %>%
  ggplot(aes(hour, wday, fill = median_speed)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral") +
  labs(x="Hour of the day", y="Day of the week")

#4 airport distances
#4.1 pickup-JFK

train %>%
  ggplot(aes(jfk_dist_pick)) +
  geom_histogram(bins = 50, fill="red") +
  scale_x_log10() +
  scale_y_sqrt() +
  labs(x="Distance between pickup location and JFK", y="Number of rides")

#4.2 dropoff-JFK

train %>%
  ggplot(aes(jfk_dist_drop)) +
  geom_histogram(bins = 50, fill="red") +
  scale_x_log10() +
  scale_y_sqrt() +
  labs(x="Distance between dropoff location and JFK", y="Number of rides")

#4.3 pickup-La Guardia

train %>%
  ggplot(aes(lg_dist_pick)) +
  geom_histogram(bins = 50, fill="blue") +
  scale_x_log10() +
  scale_y_sqrt() +
  labs(x="Distance between pickup location and La Guardia", y="Number of rides")

#4.4 dropoff-La Guardia

train %>%
  ggplot(aes(lg_dist_drop)) +
  geom_histogram(bins = 50, fill="blue") +
  scale_x_log10() +
  scale_y_sqrt() +
  labs(x="Distance between dropoff location and La Guardia", y="Number of rides")

#4.5 JFK trip durations

train %>%
  filter(trip_duration < 23*3600) %>%
  ggplot(aes(jfk_trip, trip_duration, color = jfk_trip)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "JFK trip")

#4.6 La Guardia trip durations

train %>%
  filter(trip_duration < 23*3600) %>%
  ggplot(aes(lg_trip, trip_duration, color = lg_trip)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "La Guardia trip")

#data cleaning

#cleaning training data
#5.1 extreme trip durations - trips longer than 24 hours

day_plus_trips <- train %>%
  filter(trip_duration > 24*3600)

day_plus_trips %>% select(pickup_datetime, dropoff_datetime, speed)

#save map as data frame
ny_map <- as_tibble(map_data("state", region = "new york:manhattan"))

#pickup longitude and latitude
tpickup <- day_plus_trips %>%
  select(longitude = pickup_longitude, latitude = pickup_latitude)

#dropoff longitude and latitude
tdropoff <- day_plus_trips %>%
  select(longitude = dropoff_longitude, latitude = dropoff_latitude)

#geom_polygon - visualization of start and end points connected
p1 <- ggplot() +
  geom_polygon(data = ny_map, aes(x = long, y = lat), fill="grey") +
  geom_point(data = tpickup, aes(x = longitude, y = latitude), color="red") +
  geom_point(data = tdropoff, aes(x = longitude, y = latitude), color="blue")

#for every row - draw line between pickup and dropoff points
#seq() function generates a sequence of numbers
#gcIntermediate - gets the 2D projection of a line moving between two points on a sphere
#addStartEnd - add longitudes/latitudes to result
for (i in seq(1,nrow(tpickup))){
  inter <- as_tibble(gcIntermediate(tpickup[i,],  tdropoff[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 + geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.75)
}


#5.2.1 top 5 trips between 22 hours and 24 hours - list

close_to_day_trips <- train %>% filter(trip_duration > 22*3600 & trip_duration < 24*3600)

close_to_day_trips %>%
  arrange(desc(dist)) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed) %>%
  head(5)

#distance in m - biggest distance is 60km

#5.2.2 trips between 22 hours and 24 hours - map

#take sample of random 200 trips
set.seed(2017)

close_to_day_trips <- close_to_day_trips %>% sample_n(200)

#pickup longitude and latitude
tpickup <- close_to_day_trips %>%
  select(longitude = pickup_longitude, latitude = pickup_latitude)

#dropoff longitude and latitude
tdropoff <- close_to_day_trips %>%
  select(longitude = dropoff_longitude, latitude = dropoff_latitude)

p1 <- ggplot() +
  geom_polygon(data = ny_map, aes(x = long, y = lat), fill="grey") +
  geom_point(data = tpickup, aes(x = longitude, y = latitude), color="red") +
  geom_point(data = tdropoff, aes(x = longitude, y = latitude), color="blue")


for (i in seq(1,nrow(tpickup))){
  inter <- as_tibble(gcIntermediate(tpickup[i,],  tdropoff[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 + geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.75)
}

p1 + ggtitle("Trips with duration close to a day in relation to Manhattan")

p1 <- 1

#5.3 trips shorter than a few minutes

short_trips <- train %>% filter(trip_duration < 5*60)

short_trips %>%
  arrange(dist) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed) %>%
  head(5)

#5.3.1 short trips with near zero distance

zero_dist_trips <- train %>%
  filter(near(dist,0))

#nrow() - number of rows
nrow(zero_dist_trips)

#5.3.1.1 longest trips with near zero distance
zero_dist_trips %>%
  arrange(desc(trip_duration)) %>%
  select(trip_duration, pickup_datetime, dropoff_datetime, vendor_id) %>%
  head(10)

#trip duration distribution after removal of extreme cases
zero_dist_trips %>% filter(trip_duration < 6000) %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(bins=50) +
  scale_x_log10()

#5.3.1.2 short trips with biggest speed
min_trips <- train %>%
  filter(trip_duration < 5*60 & dist > 0)

min_trips %>%
  arrange(desc(speed)) %>%
  select(trip_duration,dist,speed) %>%
  head(10)

#5.4 trips more than 300km from airport
#3e5 = 3 and 5 zeros = 300 000 meters
#jfk_dist_pick - distances between pickup location and airport (representing one location - NY)

long_distance_trips <- train %>%
  filter((jfk_dist_pick > 3e5) | (jfk_dist_drop > 3e5))

long_distance_coord <- long_distance_trips %>%
  select(lon = pickup_longitude, lat = pickup_latitude)

long_distance_trips %>%
  arrange(desc(dist)) %>%
  select(id, dist, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude)


leaflet(long_distance_coord) %>%
  addTiles() %>%
  setView(-92.00, 41.0, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(popup = ~as.character(long_distance_trips$dist), label = ~as.character(long_distance_trips$id))

#next1 - some distant places are missing from the map

#final cleaning - removal of filtered data

train <- train %>%
  
  filter(trip_duration < 22*3600,   #trip_duration < 22h
         
         dist > 0 | (near(dist, 0) & trip_duration < 60),  #minimal distances and duration < 1 min
         
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,  #more than 300km away from jfk
         
         trip_duration > 10,
         
         speed < 100)

#next2 - weather reports from that period
