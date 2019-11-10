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

#data and time - from characters to date

#vendor_id as factor

train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))

#consistency check if trip_durations = dropoff_datetime - pickup_datetime
#if consistent - FALSE

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

#2.4 trip durations and passenger count

train %>%
  group_by(passenger_count) %>%
  summarize(median_duration = median(trip_duration/60)) %>%
  ggplot(aes(passenger_count, median_duration)) +
  geom_point()

#2.5 trip duration densities by vendors

train %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_density(position = "stack") +
  scale_x_log10()

#2.6 mean and median values of trip duration by each vendor

train %>%
  group_by(vendor_id) %>%
  summarise(median = median(trip_duration),
            mean = mean(trip_duration))

#2.7 no connection to server flag connection to trip duration

train %>%
  ggplot(aes(store_and_fwd_flag, dur = (trip_duration/60))) +
  geom_bar()

#3 pickup and dropoff dates distributions over the year

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

#3.1 insight into drop between January and February in pickup, dropoff times

train %>%
  filter(pickup_datetime > ymd("2016-01-20") & pickup_datetime < ymd("2016-02-10")) %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120)

#3.2 pickup distributions by days of the week

p3 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")

#3.3 pickup distributions by times of the day

p4 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Time of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

#3.4 pickup distributions by months and times of the day

train %>%
  mutate(hpick = hour(pickup_datetime),
         month = factor(month(pickup_datetime))) %>%
  group_by(hpick, month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = month)) +
  geom_line(size = 1.5)

#3.5 pickup distributions by days of the week and times of the day

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

#6 no connection to server - trip information was held in vehicle memory if flag = Y

p6 <- train %>%
  ggplot(aes(store_and_fwd_flag, fill = store_and_fwd_flag)) +
  geom_bar() +
  scale_y_log10()

layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)



