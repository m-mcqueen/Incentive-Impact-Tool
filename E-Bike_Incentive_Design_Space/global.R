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

#~General Helper Functions####
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

#Calculate cost per kg CO2 saved
calc_costperkg <- function (incentive, CO2_saved) {
  
  #calculate cost per kg CO2 saved
  result <- incentive / CO2_saved
  
  #return result
  return(result)
}

#~IC Helper Functions####
#Calculate CO2 emissions per year by IC
calc_IC_emissions_year <- function(mileage_day, IC_Fuel_Economy) {
  IC_emissions_day <- set_units(drop_units(set_units(mileage_day / IC_Fuel_Economy, "kiloCO2_g")), "kg") #calculate emissions and convert to kg
  IC_emissions_year <- IC_emissions_day * 365 #multiply to get year total
  return(IC_emissions_year)
}

#~BEV Helper Functions####
#Calculate CO2 emissions per year by BEV
calc_BEV_emissions_year <- function(in_BEV_econ, Elec_gen_emissions, mileage_day){
  BEV_Fuel_Economy <- set_units(in_BEV_econ, "kilowatthour / hectomi")
  BEV_emissions_day <- set_units(Elec_gen_emissions * mileage_day * BEV_Fuel_Economy, "kg") #calculate emissions and convert to kg
  BEV_emissions_year <- BEV_emissions_day * 365 #multiply to get year total
  return(BEV_emissions_year)
}

#Calculate CO2 saved by BEV
calc_BEV_CO2_saved <- function(IC_emissions_year, BEV_emissions_year){
  drop_units(IC_emissions_year) - drop_units(BEV_emissions_year)
}

#~PHEV Helper Functions####
#Calculate CO2 emissions per year by PHEV
calc_PHEV_emissions_year <- function(in_PHEV_elec_econ, in_PHEV_range_elec, in_PHEV_ic_econ, mileage_day, Elec_gen_emissions){
  PHEV_elec_econ <- set_units(in_PHEV_elec_econ, "kilowatthour / hectomi")
  PHEV_range_elec <- set_units(in_PHEV_range_elec, "mi")
  PHEV_ic_econ <- set_units(in_PHEV_ic_econ, "mi / gal")
  if(PHEV_range_elec > mileage_day) { 
    elec_dist <- mileage_day #if the elec range is greater than the mileage_day, set elec_dist to mileage_day
  } else {
    elec_dist <- PHEV_range_elec #if the total distance was larger than the range, set the elec distance to the elec range
  }
  PHEV_elec_emissions_day <- set_units(Elec_gen_emissions * elec_dist * PHEV_elec_econ, "kg") #calculate emissions and convert to kg
  PHEV_ic_emissions_day <- set_units(
    if (mileage_day > PHEV_range_elec) {
      drop_units(set_units((mileage_day - PHEV_range_elec) / PHEV_ic_econ, "kiloCO2_g"))
    } else {0}
    , "kg")
  PHEV_total_emissions_day <- PHEV_elec_emissions_day + PHEV_ic_emissions_day
  PHEV_total_emissions_year <- PHEV_total_emissions_day * 365
  return(PHEV_total_emissions_year)
}

#Calculate CO2 saved by PHEV
calc_PHEV_CO2_saved <- function(IC_emissions_year, PHEV_emissions_year){
  return(drop_units(IC_emissions_year) - drop_units(PHEV_emissions_year))
  }

#~FCEV Helper Functions####
#Calculate CO2 emissions per year by FCEV
calc_FCEV_emissions_year <- function(in_FCEV_econ, in_renew_energy_ratio, mileage_day, Elec_gen_emissions){
  FCEV_H2_econ <- set_units(in_FCEV_econ, "mi / kg") #get H2 fuel economy
  FCEV_renew_energy_ratio <- in_renew_energy_ratio #get ratio of energy required for electrolysis that is 100% renewable (zero emissions)
  FCEV_H2_required_day <- mileage_day / FCEV_H2_econ #get required H2 per day
  FCEV_elec_required_day <- set_units(set_units(drop_units(FCEV_H2_required_day), "H2_kg"), "kiloWatthour") #get electricity required for 1 day of average driving defined on trips tab
  FCEV_nonrenew_elec_required_day <- FCEV_elec_required_day * (1 - FCEV_renew_energy_ratio) #get non-renewable electricity total that is required
  FCEV_emissions_day <- set_units(FCEV_nonrenew_elec_required_day * Elec_gen_emissions, "kg") #calculate emissions from non-renewable electricity and convert to kg based on generation profile selected
  FCEV_emissions_year <- FCEV_emissions_day * 365 #multiply to get year total
  return(FCEV_emissions_year)
}

#Calculate CO2 saved by FCEV
calc_FCEV_CO2_saved <- function(IC_emissions_year, FCEV_emissions_year){
  return(drop_units(IC_emissions_year) - drop_units(FCEV_emissions_year))
  }

#~E-Bike Helper Functions####
#Calculate CO2 emissions per year by E-Bike
calc_EBike_emissions_year <- function(in_EBike_Battery_Storage, in_EBike_Range, Elec_gen_emissions, mileage_day, VMT_r) {
  EBike_Battery_Storage <- set_units(in_EBike_Battery_Storage, "Watthour")
  EBike_Range <- set_units(in_EBike_Range, "mi")
  EBike_Fuel_Economy <- EBike_Range / EBike_Battery_Storage #calculate "fuel economy" based on range and battery storage
  EBike_emissions_day <- set_units(Elec_gen_emissions * (mileage_day * VMT_r) / EBike_Fuel_Economy, "kg") #calculate emissions (taking into account portion of miles travelled by e-bike) and convert to kg
  EBike_emissions_year <- EBike_emissions_day * 365 #multiply to get year total
  return(EBike_emissions_year)
}
#Calculate CO2 saved by E-Bike
calc_EBike_CO2_saved <- function(mileage_day, VMT_r, IC_Fuel_Economy, IC_emissions_year, EBike_emissions_year) {
  IC_remaining_emissions_day <- set_units(drop_units(set_units(mileage_day * (1 - VMT_r) / IC_Fuel_Economy, "kiloCO2_g")), "kg") #Get the remaining emissions from the remaining IC mileage. Change to kg
  IC_remaining_emissions_year <- IC_remaining_emissions_day * 365 #multiply to get year total
  EBike_CO2_saved <- drop_units(IC_emissions_year) - (drop_units(EBike_emissions_year) + drop_units(IC_remaining_emissions_year))
  return(EBike_CO2_saved)
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
