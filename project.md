# Cyclistic Bike-Share

## Introduction:
Cyclistic is a bike-share company based in Chicago. It's users consists of either a casual or an annual member status rider. 
The Finance team at Cyclistic have concluded that riders who are members are more profitable. 
Maximizing then number of annual members is the key for future growth at Cyclistic. The Marketing is now tasked with designing marketing strategies to convert casual riders into members. 
We the analysts in the marketing team must figure out the differences in how casual members and annual members use the bikes.

## Phase 1: Ask
A clear statement of the business task
* To identify how casual riders and annual members use bikes differently to be able to design a marketing campaign to convert casual riders into members.

## Phase 2: Prepare
A description of all data used.
* The data we will be using is the historical data of bike rides the occured over the year in 2021. 
The data contains information about bike rides including; 
bike type, start & end stations, start & end times, start & end lattitude and longitude, and member or casual user.

## Phase 3: Process
Documentation of any cleaning or manipulation of data.
* Before we can start any cleaning we need to setup our RStudio environment

```
##Set up R environment
install.packages(tidyverse)
install.packages(readr)
install.packages(lubridate)
library(tidyverse)
library(readr)
library(lubridate)
```

Then we'll load in our data.


```
##Load Cyclistic data from each month of the year 2021 into their respective variables
tripdata_jan2021 <- read_csv("202101-divvy-tripdata.csv")
tripdata_feb2021 <- read_csv("202102-divvy-tripdata.csv")
tripdata_mar2021 <- read_csv("202103-divvy-tripdata.csv")
tripdata_apr2021 <- read_csv("202104-divvy-tripdata.csv")
tripdata_may2021 <- read_csv("202105-divvy-tripdata.csv")
tripdata_jun2021 <- read.csv("202106-divvy-tripdata.csv")
tripdata_jul2021 <- read.csv("202107-divvy-tripdata.csv")
tripdata_aug2021 <- read.csv("202108-divvy-tripdata.csv")
tripdata_sep2021 <- read.csv("202109-divvy-tripdata.csv")
tripdata_oct2021 <- read.csv("202110-divvy-tripdata.csv")
tripdata_nov2021 <- read.csv("202111-divvy-tripdata.csv")
tripdata_dec2021 <- read.csv("202112-divvy-tripdata.csv")

##Combine the data into one data.frame containing the whole year.
tripdata <- rbind(tripdata_jan2021,
                  tripdata_feb2021,
                  tripdata_mar2021,
                  tripdata_apr2021,
                  tripdata_may2021,
                  tripdata_jun2021,
                  tripdata_jul2021,
                  tripdata_aug2021,
                  tripdata_sep2021,
                  tripdata_oct2021,
                  tripdata_nov2021,
                  tripdata_dec2021)

## Let's preview our data
str(tripdata)
summary(tripdata)
```

Now let's start cleaning.
```
## We'll start with removing rows containing null values.
tripdata <- na.omit(tripdata)

## Remove any duplicates
tripdata <- distinct(tripdata)
```

Then we'll manipulate our data to create columns we can analyze.

```
## Lets start transforming data to understand it easier.
## Lets create a column containing ride length by time by calculating the difference from start to end time.
tripdata <- tripdata %>%
  mutate(ride_length = as.numeric(tripdata$ended_at - tripdata$started_at) / 60)
summary(tripdata$ride_length)

## Let's add a separate column for the month the bike ride occurred.
tripdata <- tripdata %>%
  mutate(year_month = paste(strftime(tripdata$started_at, "%Y"), "-", 
                            strftime(tripdata$started_at, "%m"),
                            paste("(", strftime(tripdata$started_at, "%b"), ")",
                                  sep = "")))
unique(tripdata$year_month)

## Then we'll do the same for the weekday
tripdata <- tripdata %>%
  mutate(weekday = paste(strftime(tripdata$started_at, "%u"), "-",
         strftime(tripdata$started_at, "%a")))
unique(tripdata$weekday)

## Next we'll add a column for the distance traveled on bike.
## But first, we'll have to install and load a new package "geosphere"
install.packages("geosphere")
library(geosphere)

tripdata$ride_distance <- distGeo(matrix(c(tripdata$start_lng, tripdata$start_lat), ncol = 2),
                                  matrix(c(tripdata$end_lng, tripdata$end_lat), ncol = 2)) / 1000


##Let's save our clean data in a new file.
tripdata %>%
  write.csv("tripdata_clean.csv")
 ```
 
 ## Phase 4: Analyze
 A summary of our analysis
 
 
```
## First lets get an idea of how much the data is about members  vs. casuals.
tripdata %>%
  group_by(member_casual) %>%
  summarize(count = length(ride_id),
            "%" = (length(ride_id) / nrow(tripdata)) * 100)

## Visualize with a bar graph
ggplot(tripdata, mapping = aes(member_casual, fill=member_casual)) +
  geom_bar() +
  labs(x="Casual and Member Users", title="Casual & Member distribution")



## Let's do the same with member vs casual's ride use in time and distance
## Earlier we noticed that ride length included negative values

summary(tripdata$ride_length)

## we see that there are outliers in the ride time with -118min and 55944. This doesn't make sense so we can
## exclude these data when we analyze ride times.

## The function quantile produces sample quantities at the given probabilities
ventiles <- quantile(tripdata$ride_length, seq(0, 1, by=0.05))
ventiles

##This filters out the 5% at the ends of the distributions
tripdata_clean <- tripdata %>%
  filter(between(ride_length, 2.8, 56))
##Note, I manually inserted the integers instead of variables because I couldn't simply get it to function properly,
##This will need to be manually changed if it needs to be updated.




##Let's use this new clean data to find our averages.
user_averages <- tripdata_clean %>%
  group_by(member_casual) %>%
  summarize(mean_time = mean(ride_length), mean_distance = mean(ride_distance))
  
```

## Phase 5: Share
Supporting visualizations and key findings.

```
user_vs_time <- ggplot(user_averages) +
  geom_col(mapping=aes(x=member_casual, y=mean_time, fill=member_casual))
  labs(x="Member vs Casual distribution", y="Average Time", title="Averages of ride times between users in minutes")
user_vs_time

user_vs_distance <- ggplot(user_averages) +
  geom_col(mapping=aes(x=member_casual, y=mean_distance, fill=member_casual))+
  labs(x="Member vs Casual", y="Average Distance", title="Averages of Ride Distance between users")
user_vs_distance

user_vs_weekday <- ggplot(tripdata, mapping=aes(weekday, fill=member_casual)) +
  geom_bar() + 
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x="Weekday", y="Count of Rides", title="Bike Rides throughout the week")
user_vs_weekday

user_vs_months <- ggplot(tripdata, mapping=aes(year_month, fill=member_casual)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Month of the year 2021", y="Count of Rides", title="Distribution of Bike Rides in each Month of 2021")
user_vs_months

user_vs_rideable_type <- ggplot(tripdata, mapping=aes(rideable_type, fill=member_casual)) +
  geom_bar() +
  labs(x="Type of Bike",y="Count of Rides", title="Types of Bikes used by user")
user_vs_rideable_type

```
Analysis:

We can see that annual members have more preference to use classic bikes when compared to casual riders.

## Phase 6: Act
My top 3 recommendations based on my analysis.

1. Since casual riders tend to use the bikes over the course of the weekend, creating a campaign that shows different case uses throughout the week to create a more consistent use of the service.
2. Adjusting the service so that the longer you use the bike it can be cheaper as an annual member rather than a day-rider.
3. Creating summer promotions when bike-riding is at it's peak.

## Conclusion
Thank you for coming along my first data analysis project. As I'm sure there is much to improve on, I've learned a lot with this first course and am excited to continue building on my skills. Feel free to use, comment, critique this notebook. Thanks again!
 
 
