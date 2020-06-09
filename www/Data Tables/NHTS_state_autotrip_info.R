#Mike McQueen

#Load packages
library(dplyr)
library(stats)

#Load data
trips <- read.csv("../../NHTS/trippub.csv")

#Sort for auto trips weighted average trip count and trip length (basically VMT), group by state
d <- trips %>% 
  filter(TRPTRANS %in% c(3, 4, 5, 6), #includes car, suv, van, pickup truck
         DRVR_FLG == 1) %>% #include trips where person was the driver only (gets unique VMT basically)  
  group_by(HHSTATE, HOUSEID, VEHID) %>%  #group by state, then household, then vehicle
  summarize(mean_miles = mean(TRPMILES), #get the average miles traveled by specific vehicle for each trip
            sum_trips = n(), #get the count of trips per vehicle for the survey day
            sum_weights = sum(WTTRDFIN)) %>% #sum the weights for use in the weighted mean estimates below
  group_by(HHSTATE) %>% 
  summarize(w.mean_trips_per_veh = weighted.mean(sum_trips, sum_weights), #get the average mean trip distance for the state
     w.mean_miles_per_trip = weighted.mean(mean_miles, sum_weights)) #get the average trip count per vehicle for the state

#Write this info out
