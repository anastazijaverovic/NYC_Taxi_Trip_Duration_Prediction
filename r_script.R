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

train <- as_tibble(fread("/Users/anastazijaverovic/Desktop/NYC_TAXI_TRIP_DURATION_PREDICTION/train.csv"))
test <- as_tibble(fread("/Users/anastazijaverovic/Desktop/NYC_TAXI_TRIP_DURATION_PREDICTION/test.csv"))
sample_submit <- as_tibble(fread("/Users/anastazijaverovic/Desktop/NYC_TAXI_TRIP_DURATION_PREDICTION/sample_submission.csv"))


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

leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "yellow", fillOpacity = 1)

