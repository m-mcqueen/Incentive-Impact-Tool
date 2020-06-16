########################################################################~
# Electric Vehicle Incentive Cost and Impact Tool
#    Copyright (C) 2020  Michael McQueen
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.
########################################################################~

#Global data

#Load Packages
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(units)
library(measurements)
library(stringr)

#================================#
#Load data tables####
#================================#
load("www/Data Tables/data.RData")

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
ICE <- ICE_raw %>% 
  mutate(Fuel_Economy_Units = ifelse(Fuel_Economy_Units == "mpg", "mi / gal", Fuel_Economy_Units), #mpg is not in units database, but watthour is
         Fuel_Economy = mixed_units(Fuel_Economy, Fuel_Economy_Units)) %>% #Add units
  select(-Fuel_Economy_Units) #Clean up unecessary units columns
PHEV <- PHEV_raw %>% 
  mutate(epa_elec_econ_unit = ifelse(epa_elec_econ_unit == "kWh/100 mi", "kilowatthour / hectomi", epa_elec_econ_unit), #kWh/100 mi is not in units database, but kilowatthour / hectomi is
         epa_elec_econ = mixed_units(epa_elec_econ, epa_elec_econ_unit)) %>% #Add units
  mutate(epa_ICE_econ_unit = ifelse(epa_ICE_econ_unit == "mpg", "mi / gal", epa_ICE_econ_unit),
         epa_ICE_econ = mixed_units(epa_ICE_econ, epa_ICE_econ_unit)) %>% #Add units
  mutate(range_elec = mixed_units(range_elec, range_unit),
         range_ICE = mixed_units(range_ICE, range_unit)) %>% #Add units
  select(-epa_elec_econ_unit, -epa_ICE_econ_unit, -range_unit) #Cleanup unecessary units columns
FCEV <- FCEV_raw %>% 
  mutate(epa_econ = mixed_units(epa_h2_econ, epa_h2_econ_unit)) %>% #add units
  select(-epa_h2_econ_unit)
EBike <- EBike_raw %>% 
  mutate(EBike_econ_unit = ifelse(econ_unit == "kWh/100 mi", "kilowatthour / hectomi", econ_unit), #kWh/100 is not in units database, but kilowatthour / hectomi is
         EBike_min_econ = mixed_units(min_econ, EBike_econ_unit), #Add Units
         EBike_max_econ = mixed_units(max_econ, EBike_econ_unit)) %>% 
  select(-EBike_econ_unit)
mix <- mix_raw %>%
  left_join(BEV_raw %>% select(Make, Model, epa_econ), by = c("make" = "Make", "model" = "Model")) %>% #join BEV fuel economy info
  left_join(PHEV_raw %>% select(Make, Model, epa_elec_econ, range_elec, epa_ICE_econ), by = c("make" = "Make", "model" = "Model")) %>% 
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
         epa_ICE_econ = case_when(mix_type == "PHEV" & is.na(epa_ICE_econ) ~ mean(PHEV_raw %>% filter(Make == make) %>% pull(epa_ICE_econ), na.rm = T),
                                 T ~ as.double(epa_ICE_econ)),
         epa_h2_econ = case_when(mix_type == "FCEV" & is.na(epa_h2_econ) ~ mean(FCEV_raw %>% filter(Make == make) %>% pull(epa_h2_econ), na.rm = T),
                                 T ~ as.double(epa_h2_econ)),
         EBike_min_econ = case_when(mix_type == "EBike" ~ mean(EBike_raw %>% filter(Make == make, Model == model) %>% pull(min_econ))),
         EBike_max_econ = case_when(mix_type == "EBike" ~ mean(EBike_raw %>% filter(Make == make, Model == model) %>% pull(max_econ)))
  ) %>% 
  ungroup() %>% #Things get borked if you don't ungroup at this point
  group_by(mix_name, mix_type) %>% 
  #Summarize the weighted average fuel economies and ranges by type and by mix. If the agency didn't specify the model, the weighted averages are calculated
  #By using the mean values for the attribute of all of the known models within the make
  summarize(epa_econ_wm = weighted.mean(epa_econ, count, na.rm = T),
            epa_elec_econ_wm = weighted.mean(epa_elec_econ, count, na.rm = T),
            range_elec_wm = weighted.mean(range_elec, count, na.rm = T),
            epa_ICE_econ_wm = weighted.mean(epa_ICE_econ, count, na.rm = T),
            epa_h2_econ_wm = weighted.mean(epa_h2_econ, count, na.rm = T),
            EBike_min_econ = weighted.mean(EBike_min_econ, count, na.rm = T),
            EBike_max_econ = weighted.mean(EBike_max_econ, count, na.rm = T))

#Remove unecessary tables
rm(BEV_raw, PHEV_raw, FCEV_raw, EBike_raw, Electricity_raw, ICE_raw, Incentives_raw, Trips_raw, mix_raw)

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
