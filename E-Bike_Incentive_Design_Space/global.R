#E-bike Incentive Impact Tool
#Global data
#Mike McQueen

#Load Packages
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(units)
library(measurements)
library(stringr)

#================================#
####Define Helper Functions####
#================================#

#Exclude Items that are not selected
exclude_items <- function(data, EBike, PHEV, BEV, FCEV) {
  if(!EBike) {
    data <- data %>% 
      filter(mode != "EBike")
  }
  if(!PHEV) {
    data <- data %>% 
      filter(mode != "PHEV")
  }
  if(!BEV) {
    data <- data %>% 
      filter(mode != "BEV")
  }
  if(!FCEV) {
    data <- data %>% 
      filter(mode != "FCEV")
  }
  return(data)
}

#================================#
#Load data tables####
#================================#
#State Info
Electricity_raw <- read.csv("www/Data Tables/State Info/Electricity.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
Incentives_raw <- read.csv("www/Data Tables/State Info/Incentives.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
Trips_raw <- read.csv("www/Data Tables/State Info/Trips.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#Vehicles
BEV_raw <- read.csv("www/Data Tables/Vehicles/BEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
PHEV_raw <- read.csv("www/Data Tables/Vehicles/PHEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
FCEV_raw <- read.csv("www/Data Tables/Vehicles/FCEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
IC_raw <- read.csv("www/Data Tables/Vehicles/IC.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
EBike_raw <- read.csv("www/Data Tables/Vehicles/EBike.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
mix_raw <- read.csv("www/Data Tables/Vehicles/mix.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) #Defines a specific combo of vehicle types

#================================#
#Clean up tables to combine units with scalar values####
#================================#
#State Info
Electricity <- Electricity_raw %>% 
  mutate(CO2_Units = ifelse(CO2_Units == "lb/MWh", "pounds / megawatthour", CO2_Units), #lb/MWh is not in units database, but pounds / megawatthour is
         CO2 = mixed_units(CO2, CO2_Units)) %>%  #Add units
  select(-CO2_Units) #Cleanup unecessary units columns

Incentives <- Incentives_raw

Trips <- Trips_raw %>% 
  mutate(Car_Trip_Avg_Length = mixed_units(Car_Trip_Avg_Length, Car_Trip_Avg_Length_Units)) %>% #Add units
  select(-Car_Trip_Avg_Length_Units) #Clean up unecessary units columns


#Vehicles
BEV <- BEV_raw %>%
  mutate(epa_econ_unit = ifelse(epa_econ_unit == "kWh/100 mi", "kilowatthour / hectomi", epa_econ_unit), #kWh/100 mi is not in units database, but kilowatthour / hectomi is
         epa_econ = mixed_units(epa_econ, epa_econ_unit)) %>% #Add units
  select(-epa_econ_unit) #Cleanup unecessary units columns
IC <- IC_raw %>% 
  mutate(Fuel_Economy_Units = ifelse(Fuel_Economy_Units == "mpg", "mi / gal", Fuel_Economy_Units), #mpg is not in units database, but watthour is
         Fuel_Economy = mixed_units(Fuel_Economy, Fuel_Economy_Units)) %>% #Add units
  select(-Fuel_Economy_Units) #Clean up unecessary units columns
PHEV <- PHEV_raw %>% 
  mutate(epa_elec_econ_unit = ifelse(epa_elec_econ_unit == "kWh/100 mi", "kilowatthour / hectomi", epa_elec_econ_unit), #kWh/100 mi is not in units database, but kilowatthour / hectomi is
         epa_elec_econ = mixed_units(epa_elec_econ, epa_elec_econ_unit)) %>% #Add units
  mutate(epa_ic_econ_unit = ifelse(epa_ic_econ_unit == "mpg", "mi / gal", epa_ic_econ_unit),
         epa_ic_econ = mixed_units(epa_ic_econ, epa_ic_econ_unit)) %>% #Add units
  mutate(range_elec = mixed_units(range_elec, range_unit),
         range_ic = mixed_units(range_ic, range_unit)) %>% #Add units
  select(-epa_elec_econ_unit, -epa_ic_econ_unit, -range_unit) #Cleanup unecessary units columns
FCEV <- FCEV_raw %>% 
  mutate(epa_econ = mixed_units(epa_h2_econ, epa_h2_econ_unit)) %>% #add units
  select(-epa_h2_econ_unit)
EBike <- EBike_raw %>% 
  mutate(Battery_Storage_Units = ifelse(Battery_Storage_Units == "Wh", "watthour", Battery_Storage_Unit), #Wh is not in units database, but watthour is
         Battery_Storage = mixed_units(Battery_Storage, Battery_Storage_Units), #Add Units
         Range = mixed_units(Range,Range_Units)) %>%  #Add Units
  select(-Battery_Storage_Units, -Range_Units)
mix <- mix_raw %>%
  left_join(BEV_raw %>% select(Make, Model, epa_econ), by = c("make" = "Make", "model" = "Model")) %>% #join BEV fuel economy info
  left_join(PHEV_raw %>% select(Make, Model, epa_elec_econ, range_elec, epa_ic_econ), by = c("make" = "Make", "model" = "Model")) %>% 
  left_join(FCEV_raw %>% select(Make, Model, epa_h2_econ), by = c("make" = "Make", "model" = "Model")) %>% 
  group_by(mix_type, mix_name, make, model) %>%   #group by vehicle type (BEV, PHEV, etc.) and mix name (ex: OR_Feb_20)
  #this next mutate is to fix the case where agencies provide make info but not model info for their counts. In this case, the data tables have "na" for model.
  #This code takes the mean of the relevant attribute(s) for all of that make's vehicles that are in the table and uses that for the weighted mean computations
  #in the next step
  mutate(epa_econ = case_when(mix_type == "BEV" & is.na(epa_econ) ~ mean(BEV_raw %>% filter(Make == make) %>% pull(epa_econ), na.rm = T),
                              T ~ as.double(epa_econ)),
         epa_elec_econ = case_when(mix_type == "PHEV" & is.na(epa_elec_econ) ~ mean(PHEV_raw %>% filter(Make == make) %>% pull(epa_elec_econ), na.rm = T),
                                   T ~ as.double(epa_elec_econ)),
         range_elec = case_when(mix_type == "PHEV" & is.na(range_elec) ~ mean(PHEV_raw %>% filter(Make == make) %>% pull(range_elec), na.rm = T),
                                T ~ as.double(range_elec)),
         epa_ic_econ = case_when(mix_type == "PHEV" & is.na(epa_ic_econ) ~ mean(PHEV_raw %>% filter(Make == make) %>% pull(epa_ic_econ), na.rm = T),
                                 T ~ as.double(epa_ic_econ)),
         epa_h2_econ = case_when(mix_type == "FCEV" & is.na(epa_h2_econ) ~ mean(FCEV_raw %>% filter(Make == make) %>% pull(epa_h2_econ), na.rm = T),
                                 T ~ as.double(epa_h2_econ))
  ) %>% 
  ungroup() %>% #Things get borked if you don't ungroup at this point
  group_by(mix_name, mix_type) %>% 
  #Summarize the weighted average fuel economies and ranges by type and by mix. If the agency didn't specify the model, the weighted averages are calculated
  #By using the mean values for the attribute of all of the known models within the make
  summarize(epa_econ_wm = weighted.mean(epa_econ, count, na.rm = T),
            epa_elec_econ_wm = weighted.mean(epa_elec_econ, count, na.rm = T),
            range_elec_wm = weighted.mean(range_elec, count, na.rm = T),
            epa_ic_econ_wm = weighted.mean(epa_ic_econ, count, na.rm = T),
            epa_h2_econ_wm = weighted.mean(epa_h2_econ, count, na.rm = T)) 

#Remove unecessary tables
rm(BEV_raw, PHEV_raw, FCEV_raw, EBike_raw, Electricity_raw, IC_raw, Incentives_raw, Trips_raw, mix_raw)

#================================#
#Define Conversion Constants####
#================================#

#Carbon emissions from gallon of gasoline
#Check to see if it's already installed, if not, install:
error_present <-  tryCatch({ #This catches an error that gets thrown if the conversion constant is already installed
  install_conversion_constant("gal", "CO2_g", 8887) #If this doesn't cause an error, it will run.
  F #If install_conversion_constant ran, that means there was no error thrown
}, warning = function(war) {
}, error = function(err) {
  return(T) #If there was an error thrown, return T but hide the error message.
}, finally = {
})

#Electricity required for 1 kg of hydrogen (electrolysis)
#https://www.energy.gov/eere/fuelcells/doe-technical-targets-hydrogen-production-electrolysis
#Check to see if it's already installed, if not, install:
error_present <-  tryCatch({ #This catches an error that gets thrown if the conversion constant is already installed
  install_conversion_constant("kiloWatthour", "H2_kg", 1/44) #If this doesn't cause an error, it will run.
  F #If install_conversion_constant ran, that means there was no error thrown
}, warning = function(war) {
}, error = function(err) {
  return(T) #If there was an error thrown, return T but hide the error message.
}, finally = {
})
