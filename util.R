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

#This file contains helper functions

#================================#
#General Helper Functions####
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

#Calculate cost per kg CO2 saved
calc_costperkg <- function (incentive, CO2_saved) {
  
  #calculate cost per kg CO2 saved
  result <- incentive / CO2_saved
  
  #return result
  return(result)
}

#~ICE Helper Functions####
#Calculate CO2 emissions per year by ICE
calc_ICE_emissions_year <- function(mileage_day, ICE_Fuel_Economy) {
  ICE_emissions_day <- set_units(drop_units(set_units(mileage_day / ICE_Fuel_Economy, "kiloCO2_g")), "kg") #calculate emissions and convert to kg
  ICE_emissions_year <- ICE_emissions_day * 365 #multiply to get year total
  return(ICE_emissions_year)
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
calc_BEV_CO2_saved <- function(ICE_emissions_year, BEV_emissions_year){
  drop_units(ICE_emissions_year) - drop_units(BEV_emissions_year)
}

#~PHEV Helper Functions####
#Calculate CO2 emissions per year by PHEV
calc_PHEV_emissions_year <- function(in_PHEV_elec_econ, in_PHEV_range_elec, in_PHEV_ICE_econ, mileage_day, Elec_gen_emissions){
  PHEV_elec_econ <- set_units(in_PHEV_elec_econ, "kilowatthour / hectomi")
  PHEV_range_elec <- set_units(in_PHEV_range_elec, "mi")
  PHEV_ICE_econ <- set_units(in_PHEV_ICE_econ, "mi / gal")
  if(PHEV_range_elec > mileage_day) { 
    elec_dist <- mileage_day #if the elec range is greater than the mileage_day, set elec_dist to mileage_day
  } else {
    elec_dist <- PHEV_range_elec #if the total distance was larger than the range, set the elec distance to the elec range
  }
  PHEV_elec_emissions_day <- set_units(Elec_gen_emissions * elec_dist * PHEV_elec_econ, "kg") #calculate emissions and convert to kg
  PHEV_ICE_emissions_day <- set_units(
    if (mileage_day > PHEV_range_elec) {
      drop_units(set_units((mileage_day - PHEV_range_elec) / PHEV_ICE_econ, "kiloCO2_g"))
    } else {0}
    , "kg")
  PHEV_total_emissions_day <- PHEV_elec_emissions_day + PHEV_ICE_emissions_day
  PHEV_total_emissions_year <- PHEV_total_emissions_day * 365
  return(PHEV_total_emissions_year)
}

#Calculate CO2 saved by PHEV
calc_PHEV_CO2_saved <- function(ICE_emissions_year, PHEV_emissions_year){
  return(drop_units(ICE_emissions_year) - drop_units(PHEV_emissions_year))
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
calc_FCEV_CO2_saved <- function(ICE_emissions_year, FCEV_emissions_year){
  return(drop_units(ICE_emissions_year) - drop_units(FCEV_emissions_year))
}

#~E-Bike Helper Functions####
#Calculate CO2 emissions per year by E-Bike
calc_EBike_emissions_year <- function(in_EBike_econ, Elec_gen_emissions, mileage_day, VMT_r) {
  EBike_econ <- set_units(in_EBike_econ, "kilowatthour / hectomi")
  EBike_emissions_day <- set_units(Elec_gen_emissions * (mileage_day * VMT_r) * EBike_econ, "kg") #calculate emissions (taking into account portion of miles travelled by e-bike) and convert to kg
  EBike_emissions_year <- EBike_emissions_day * 365 #multiply to get year total
  return(EBike_emissions_year)
}
#Calculate CO2 saved by E-Bike
calc_EBike_CO2_saved <- function(mileage_day, VMT_r, ICE_Fuel_Economy, ICE_emissions_year, EBike_emissions_year) {
  ICE_remaining_emissions_day <- set_units(drop_units(set_units(mileage_day * (1 - VMT_r) / ICE_Fuel_Economy, "kiloCO2_g")), "kg") #Get the remaining emissions from the remaining ICE mileage. Change to kg
  ICE_remaining_emissions_year <- ICE_remaining_emissions_day * 365 #multiply to get year total
  EBike_CO2_saved <- drop_units(ICE_emissions_year) - (drop_units(EBike_emissions_year) + drop_units(ICE_remaining_emissions_year))
  return(EBike_CO2_saved)
}

#================================#
#Plotting helper functions####
#================================#
#~~g1: Cost per kg CO2 saved by mode####
g1plot <- function(costperkg, test_points, costperkg_x, costperkg_y, mode_scale_colors) {
  xlab <- seq(costperkg_x[1], costperkg_x[2], 1000) #Set axis breaks manually
  
  g1 <-  ggplot(costperkg, aes(incentive, costperkg, color=mode)) +
    labs(title = "Cost per kg CO2 saved (1 year)",
         x = "Incentive Value ($)",
         y = "Cost per CO2 saved ($/kg), one mode only") +
    coord_cartesian(xlim = costperkg_x, ylim = costperkg_y) +
    geom_line(size = 1.5) + #plot mode lines
    geom_point(data = test_points,
               aes(incentive,
                   costperkg,
                   color = mode), size = 5) + #plot specific test points
    geom_segment(data = test_points,
                 aes(x = 0, y = costperkg, xend = incentive, yend = costperkg, color = mode),
                 linetype = "dashed",
                 size = 1.5) +
    geom_segment(data = test_points,
                 aes(x = incentive, y = 0, xend = incentive, yend = costperkg, color = mode),
                 linetype = "dashed",
                 size = 1.5) +
    mode_scale_colors +
    scale_x_continuous(labels = paste0("$", xlab),
                       breaks = xlab)
  return(g1)
}
#~~g2: Number incentivized####
g2plot <- function(num_incentivized, test_budget_points, num_x, num_y, mode_scale_colors) {
  xlab <- seq(num_x[1], num_x[2], 1e6) / 1e6 #Set axis breaks manually
  ylab <- seq(num_y[1], num_y[2], 2000)
  
  ggplot(num_incentivized, aes(budget, num, color = mode)) +
    labs(title = "Number of Vehicles Incentivized",
         x = "Total Budget ($)",
         y = "Total incentives provided, one mode only") +
    geom_line(size = 1.5) + #plot mode lines
    coord_cartesian(xlim = num_x,
                    ylim = num_y) +
    geom_point(data = test_budget_points,
               aes(budget, num, color = mode),
               size = 5) + #plot specific test points
    geom_segment(data = test_budget_points,
                 aes(x = 0, y = num, xend = budget, yend = num, color = mode),
                 linetype = "dashed",
                 size = 1.5) +
    geom_segment(data = test_budget_points,
                 aes(x = budget, y = 0, xend = budget, yend = num, color = mode),
                 linetype = "dashed",
                 size = 1.5) +
    mode_scale_colors +
    scale_x_continuous(labels = paste0("$", xlab, "M"),
                       breaks = xlab * 1e6) +
    scale_y_continuous(labels = ylab,
                       breaks = ylab)
}
#~~g3: CO2 saved####
g3plot <- function(CO2_saved, test_budget_points_w_CO2, num_incentivized, CO2_saved_x, CO2_saved_y, mode_scale_colors) {
  xlab <- seq(CO2_saved_x[1], CO2_saved_x[2], 1e6) / 1e6 #Set axis breaks manually
  ylab <- seq(CO2_saved_y[1], CO2_saved_y[2], 2e6) / 1e6
  
  ggplot(CO2_saved, aes(budget, CO2_saved, color = mode)) +
    labs(title = "Total CO2 saved (1 year)",
         x = "Total Budget ($)",
         y = "Total CO2 saved (kg), one mode only") +
    geom_line(size = 1.5) +
    coord_cartesian(xlim = CO2_saved_x,
                    ylim = CO2_saved_y) +
    geom_point(data = test_budget_points_w_CO2,
               aes(budget, total_CO2_saved),
               size = 5) +
    geom_segment(data = test_budget_points_w_CO2,
                 aes(x = 0, y = total_CO2_saved, xend = budget, yend = total_CO2_saved, color = mode),
                 linetype = "dashed",
                 size = 1.5) +
    geom_segment(data = test_budget_points_w_CO2,
                 aes(x = budget, y = 0, xend = budget, yend = total_CO2_saved, color = mode),
                 linetype = "dashed",
                 size = 1.5) +
    mode_scale_colors +
    scale_x_continuous(labels = paste0("$", xlab, "M"),
                       breaks = xlab * 1e6) +
    scale_y_continuous(labels = paste0(ylab, "M"),
                       breaks = ylab * 1e6)
}

#~~g4: Budget distribution specific number incentivized####
g4plot <- function(num_incentivized, num_incentivized_distrib, test_budget_points_distrib, num_x, num_y, mode_scale_colors, mode_scale_fill) {
  xlab <- seq(num_x[1], num_x[2], 1e6) / 1e6 #Set axis breaks manually
  
  ggplot(num_incentivized_distrib, aes(budget, num, fill = mode)) +
    labs(title = "Number of Vehicles Incentivized, Budget Distribution Specific",
         x = "Total Budget ($)",
         y = "Total Number of Incentives Provided") +
    geom_area() + 
    coord_cartesian(xlim = num_x,
                    ylim = num_y) +
    geom_point(data = test_budget_points_distrib,
               aes(budget, sum(num)),
               size = 5) + #plot specific test points
    geom_segment(data = test_budget_points_distrib, #horizontal line
                 aes(x = 0,
                     y = sum(num),
                     xend = budget,
                     yend = sum(num)),
                 linetype = "dashed",
                 size = 1.5) +
    geom_segment(data = test_budget_points_distrib, #vertical line
                 aes(x = budget, y = 0,
                     xend = budget,
                     yend = sum(num)),
                 linetype = "dashed",
                 size = 1.5) +
    mode_scale_colors +
    mode_scale_fill +
    scale_x_continuous(labels = paste0("$", xlab, "M"),
                       breaks = xlab * 1e6)
}

#~~g5: Budget distribution specific CO2 saved####
g5plot <- function(num_incentivized, CO2_saved_distrib, test_budget_points_w_CO2_distrib, CO2_saved_x, CO2_saved_y, mode_scale_colors, mode_scale_fill) {
  xlab <- seq(CO2_saved_x[1], CO2_saved_x[2], 1e6) / 1e6 #Set axis breaks manually
  ylab <- seq(CO2_saved_y[1], CO2_saved_y[2], 2e6) / 1e6
  
  ggplot(CO2_saved_distrib, aes(budget, CO2_saved, fill = mode)) +
    labs(title = "Total CO2 saved (1 year), Budget Distribution Specific",
         x = "Total Budget ($)",
         y = "Total CO2 Saved (kg)") +
    geom_area() +
    coord_cartesian(xlim = CO2_saved_x,
                    ylim = CO2_saved_y) +
    geom_point(data = test_budget_points_w_CO2_distrib,
               aes(budget,sum(total_CO2_saved)),
               size = 5) +
    geom_segment(data = test_budget_points_w_CO2_distrib, #horizontal line
                 aes(x = 0,
                     y = sum(total_CO2_saved),
                     xend = budget,
                     yend = sum(total_CO2_saved)),
                 linetype = "dashed",
                 size = 1.5) +
    geom_segment(data = test_budget_points_w_CO2_distrib, #vertical line
                 aes(x = budget,
                     y = 0,
                     xend = budget,
                     yend = sum(total_CO2_saved)),
                 linetype = "dashed",
                 size = 1.5) +
    mode_scale_colors +
    mode_scale_fill +
    scale_x_continuous(labels = paste0("$", xlab, "M"),
                       breaks = xlab * 1e6) +
    scale_y_continuous(labels = paste0(ylab, "M"),
                       breaks = ylab * 1e6)
}
