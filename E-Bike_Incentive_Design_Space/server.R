#E-Vehicle Incentive Impact Tool
#Server
#Mike McQueen

#Source helper scripts
source("util.R")

#Load Packages
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(units)
library(measurements)
library(stringr)
library(viridis)



# Define server logic required calculate and draw plots
server <-  function(input, output, session) {
  
  #================================#
  #Plot Dimensions####
  #================================#
  #cost per kg CO2 saved
  costperkg_x <- reactive(input$g1_x)
  costperkg_y <- reactive(input$g1_y)
  
  #number of vehicles incentivized
  num_x <- reactive(input$g2_g4_x)
  num_y <- reactive(input$g2_g4_y)
  
  #total CO2 saved
  CO2_saved_x <- reactive(input$g3_g5_x)
  CO2_saved_y <- reactive(input$g3_g5_y)
  
  #================================#
  #Plot Colors####
  #colorblind accessible
  #================================#
  
  #color palate
  mode_scale_colors <- scale_color_manual(name = "Mode",
                                          values = c("FCEV" = viridis(1, begin = 0),
                                                     "BEV" = viridis(1, begin = .25),
                                                     "PHEV" = viridis(1, begin = .50),
                                                     "EBike" = viridis(1, begin = .75)))
  
  #fill color palate
  mode_scale_fill <- scale_fill_manual(name = "Mode",
                                       values = c("FCEV" = viridis(1, begin = 0),
                                                  "BEV" = viridis(1, begin = .25),
                                                  "PHEV" = viridis(1, begin = .50),
                                                  "EBike" = viridis(1, begin = .75)))
  
  #================================#
  #Text input preset value updaters####
  #================================#
  #~Update Trips panel with preset####
  observeEvent(input$apply_in_preset_Car_Trips_Daily_Avg, {
    #Get preset val
    in_Car_Trips_Daily_Avg_preset_val <- Trips %>%
      filter(State == input$in_preset_Car_Trips_Daily_Avg) %>%
      pull(Car_Trips_Daily_Avg)
    #Get preset val
    in_Car_Trip_Avg_Length_preset_val <- drop_units(Trips %>%
                                                      filter(State == input$in_preset_Car_Trips_Daily_Avg) %>%
                                                      pull(Car_Trip_Avg_Length))
    #update vals
    updateNumericInput(session, inputId = "in_Car_Trips_Daily_Avg",
                       value = in_Car_Trips_Daily_Avg_preset_val
    )
    updateNumericInput(session, inputId = "in_Car_Trip_Avg_Length",
                       value = in_Car_Trip_Avg_Length_preset_val
                       )
    #Save the preset vals as current state of the inputs
    states$state_in_Car_Trips_Daily_Avg <- in_Car_Trips_Daily_Avg_preset_val
    states$state_in_Car_Trip_Avg_Length <- in_Car_Trip_Avg_Length_preset_val
  })
  #~Update Power Generation panel with preset####
  observeEvent(input$apply_in_preset_elec_gen_emissions, {
    #get preset val
    in_preset_elec_gen_emissions_val <- drop_units(Electricity %>%
                                                     filter(State == input$in_preset_elec_gen_emissions) %>%
                                                     pull(CO2))
    #update val
    updateNumericInput(session, inputId = "in_elec_gen_emissions",
                       value = in_preset_elec_gen_emissions_val
                       )
    #Save the preset val as current state of the inputs
    states$state_in_preset_elec_gen_emissions <- in_preset_elec_gen_emissions_val
  })
  
  #~Update IC panel with preset####
  observeEvent(input$apply_in_preset_IC_Fuel_Economy, {
    make_and_model = str_split(input$in_preset_IC_Fuel_Economy, " ")
    #get preset val
    in_preset_IC_Fuel_Economy_val <- drop_units(IC %>%
                                                  filter(Make == make_and_model[[1]][1],
                                                         Model == make_and_model[[1]][2]) %>%
                                                  pull(Fuel_Economy)
    )
    #update val
    updateNumericInput(session, inputId = "in_IC_Fuel_Economy",
                       value = in_preset_IC_Fuel_Economy_val
    )
    #Save the preset val as current state of the input
    states$state_in_preset_IC_Fuel_Economy_val <- in_preset_IC_Fuel_Economy_val
    
  })
  #~Update E-Bike panel with preset####
  observeEvent(input$apply_in_preset_EBike, {
    make_and_model = str_split(input$in_preset_EBike, " ")
    #get preset vals
    in_preset_EBike_Battery_Storage_val <- as.numeric(EBike %>%
                                        filter(Make == make_and_model[[1]][1],
                                               Model == make_and_model[[1]][2]) %>%
                                        pull(Battery_Storage)
    )
    in_preset_EBike_Range_val <- as.numeric(EBike %>%
                                              filter(Make == make_and_model[[1]][1],
                                                     Model == make_and_model[[1]][2]) %>%
                                              pull(Range)
    )
    #update vals
    updateNumericInput(session, inputId = "in_EBike_Battery_Storage",
                       value = in_preset_EBike_Battery_Storage_val
    )
    updateNumericInput(session, inputId = "in_EBike_Range",
                       value = in_preset_EBike_Range_val
    )
    #Save the preset vals as current states of the inputs
    states$state_in_EBike_Battery_Storage <- in_preset_EBike_Battery_Storage_val
    states$state_in_EBike_Range <- in_preset_EBike_Range_val
  })
  #~Update BEV panel with preset####
  observeEvent(input$apply_in_preset_BEV, {
    #get preset val
    in_preset_BEV_econ_val <- round(as.numeric(mix %>% 
                       filter(mix_type == "BEV",
                              mix_name == input$in_preset_BEV) %>% 
                       pull(epa_econ_wm)), 2)
    #update val
    updateNumericInput(session, inputId = "in_BEV_econ",
                       value = in_preset_BEV_econ_val)
    #Save the preset val as current state of the input
    states$state_in_BEV_econ <- in_preset_BEV_econ_val
  })
  #~Update PHEV panel with preset####
  observeEvent(input$apply_in_preset_PHEV, {
    #get preset vals
    in_preset_PHEV_elec_econ <- round(as.numeric(mix %>% 
                                                   filter(mix_type == "PHEV",
                                                          mix_name == input$in_preset_PHEV) %>% 
                                                   pull(epa_elec_econ_wm)))
    in_preset_PHEV_range_elec <- round(as.numeric(mix %>% 
                                                    filter(mix_type == "PHEV",
                                                           mix_name == input$in_preset_PHEV) %>% 
                                                    pull(range_elec_wm)))
    in_preset_PHEV_ic_econ <- round(as.numeric(mix %>%
                                                 filter(mix_type == "PHEV",
                                                        mix_name == input$in_preset_PHEV) %>% 
                                                 pull(epa_ic_econ_wm)))
    #update vals
    updateNumericInput(session, inputId = "in_PHEV_elec_econ",
                       value = in_preset_PHEV_elec_econ)
    updateNumericInput(session, inputId = "in_PHEV_range_elec",
                       value = in_preset_PHEV_range_elec)
    updateNumericInput(session, inputId = "in_PHEV_ic_econ",
                       value = in_preset_PHEV_ic_econ)
    #Save the preset vals as current states of the inputs
    states$state_in_PHEV_elec_econ <- in_preset_PHEV_elec_econ
    states$state_in_PHEV_range_elec <- in_preset_PHEV_range_elec
    states$state_in_PHEV_ic_econ <- in_preset_PHEV_ic_econ
  })
  #~Update FCEV panel with preset####
  observeEvent(input$apply_in_preset_FCEV, {
    #get preset val
    in_preset_FCEV_econ = round(as.numeric(mix %>% 
                                             filter(mix_type == "FCEV",
                                                    mix_name == input$in_preset_FCEV) %>% 
                                             pull(epa_h2_econ_wm)), 2)
    #update val
  
    updateNumericInput(session, inputId = "in_FCEV_econ",
                       value = in_preset_FCEV_econ)
    #Save the preset val as current state of the input
    states$state_in_FCEV_econ <- in_preset_FCEV_econ
  })
  #================================#
  #Flags to know if a preset was applied####
  #================================#
  #Initialize global reactive values (flags and states)
  #Flags initially set to T because the CA preset values are loaded in by default
  states <- reactiveValues(state_in_Car_Trips_Daily_Avg = 0,
                           state_in_Car_Trip_Avg_Length = 0,
                           flag_preset_Car_Trips_Daily_Avg = T,
                           state_in_preset_elec_gen_emissions = 0,
                           flag_preset_elec_gen_emissions = T,
                           state_in_IC_Fuel_Economy = 0,
                           flag_in_preset_IC_Fuel_Economy = T,
                           state_in_EBike_Battery_Storage = 0,
                           state_in_EBike_Range = 0,
                           flag_in_preset_EBike = T,
                           state_in_BEV_econ = 0,
                           flag_in_preset_BEV = T,
                           state_in_PHEV_elec_econ = 0,
                           state_in_PHEV_range_elec = 0,
                           state_in_PHEV_ic_econ = 0,
                           flag_in_preset_PHEV = T,
                           state_in_FCEV_econ = 0,
                           flag_in_preset_FCEV = T
                           )
  #================================#
  #~Trips panel preset flag####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_Car_Trips_Daily_Avg, {
    
    #Change flag to T
    states$flag_preset_Car_Trips_Daily_Avg <- T
    })
  
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({
    input$in_Car_Trips_Daily_Avg
    input$in_Car_Trip_Avg_Length
    }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_preset_Car_Trips_Daily_Avg & input$in_Car_Trips_Daily_Avg != states$state_in_Car_Trips_Daily_Avg | states$flag_preset_Car_Trips_Daily_Avg & input$in_Car_Trip_Avg_Length != states$state_in_Car_Trip_Avg_Length) {
      #Change flag to F
      states$flag_preset_Car_Trips_Daily_Avg <- F
    }
  },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  #================================#
  #~Power Generation panel preset flag####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_elec_gen_emissions, {
    #Change flag to T
    states$flag_preset_elec_gen_emissions <- T
  })
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({ #watch for changes in input values
    input$in_elec_gen_emissions
  }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_preset_elec_gen_emissions & input$in_elec_gen_emissions != states$state_in_preset_elec_gen_emissions) {
      #Change flag to F
      states$flag_preset_elec_gen_emissions <- F
    }
    },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  #================================#
  #~IC panel preset flag####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_IC_Fuel_Economy, {
    #Change flag to T
    states$flag_in_preset_IC_Fuel_Economy <- T
  })
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({ #watch for changes in input values
    input$in_IC_Fuel_Economy
  }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_in_preset_IC_Fuel_Economy & input$in_IC_Fuel_Economy != states$state_in_IC_Fuel_Economy) {
      #Change flag to F
      states$flag_in_preset_IC_Fuel_Economy <- F
    }
  },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  #================================#
  #~E-Bike panel preset panel####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_EBike, {
    #Change flag to T
    states$flag_in_preset_EBike <- T
  })
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({ #watch for changes in input values
    input$in_EBike_Battery_Storage
    input$in_EBike_Range
  }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_in_preset_EBike & input$in_EBike_Battery_Storage != states$state_in_EBike_Battery_Storage |
       states$flag_in_preset_EBike & input$in_EBike_Range != states$state_in_EBike_Range) {
      #Change flag to F
      states$flag_in_preset_EBike <- F
    }
  },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  #================================#
  #~BEV panel preset flag####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_BEV, {
    #Change flag to T
    states$flag_in_preset_BEV <- T
  })
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({ #watch for changes in input values
    input$in_BEV_econ
  }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_in_preset_BEV & input$in_BEV_econ != states$state_in_BEV_econ) {
      #Change flag to F
      states$flag_in_preset_BEV <- F
    }
  },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  #================================#
  #~PHEV panel preset flag####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_PHEV, {
    #Change flag to T
    states$flag_in_preset_PHEV <- T
  })
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({ #watch for changes in input values
    input$in_PHEV_elec_econ
    input$in_PHEV_range_elec
    input$in_PHEV_ic_econ
  }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_in_preset_PHEV & input$in_PHEV_elec_econ != states$state_in_PHEV_elec_econ |
       states$flag_in_preset_PHEV & input$in_PHEV_range_elec != states$state_in_PHEV_range_elec |
       states$flag_in_preset_PHEV & input$in_PHEV_ic_econ != states$state_in_PHEV_ic_econ) {
      #Change flag to F
      states$flag_in_preset_PHEV <- F
    }
  },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  #================================#
  #~FCEV panel preset flag####
  #If a preset has been applied, change the flag to T
  observeEvent(input$apply_in_preset_FCEV, {
    #Change flag to T
    states$flag_in_preset_FCEV <- T
  })
  #If the values in the panel are changed at all, the flag reverts to F
  observeEvent({ #watch for changes in input values
    input$in_FCEV_econ
  }, {
    
    #If the input values no longer match the prest "states" (redefined in the numeric updaters above)
    if(states$flag_in_preset_FCEV & input$in_FCEV_econ != states$state_in_FCEV_econ) {
      #Change flag to F
      states$flag_in_preset_FCEV <- F
    }
  },
  ignoreInit = T) #So that the preset is shown if nothing is changed, since the default values are from a preset
  
  #================================#
  #Limit the budget inputs to sum to 100 using observers####
  #================================#
  observeEvent(input$in_BEV_per_budget, {
    #Check if the last slider action makes the slider sum to something above 100%
    if(sum(input$in_BEV_per_budget,
           input$in_EBike_per_budget,
           input$in_PHEV_per_budget,
           input$in_FCEV_per_budget) > 100) {
      #If so, update the last slider so that everything sums to 100
      updateSliderInput(session,
                        inputId = "in_BEV_per_budget",
                        value = 100 - (input$in_EBike_per_budget + input$in_PHEV_per_budget + input$in_FCEV_per_budget)
      )
    }
  })
  observeEvent(input$in_EBike_per_budget, {
    #Check if the last slider action makes the slider sum to something above 100%
    if(sum(input$in_BEV_per_budget,
           input$in_EBike_per_budget,
           input$in_PHEV_per_budget,
           input$in_FCEV_per_budget) > 100) {
      #If so, update the last slider so that everything sums to 100
      updateSliderInput(session,
                        inputId = "in_EBike_per_budget",
                        value = 100 - (input$in_BEV_per_budget + input$in_PHEV_per_budget + input$in_FCEV_per_budget)
      )
    }   
  })
  observeEvent(input$in_PHEV_per_budget, {
    #Check if the last slider action makes the slider sum to something above 100%
    if(sum(input$in_BEV_per_budget,
           input$in_EBike_per_budget,
           input$in_PHEV_per_budget,
           input$in_FCEV_per_budget) > 100) {
      #If so, update the last slider so that everything sums to 100
      updateSliderInput(session,
                        inputId = "in_PHEV_per_budget",
                        value = 100 - (input$in_BEV_per_budget + input$in_EBike_per_budget + input$in_FCEV_per_budget)
      )
    }  
  })
  observeEvent(input$in_FCEV_per_budget, {
    #Check if the last slider action makes the slider sum to something above 100%
    if(sum(input$in_BEV_per_budget,
           input$in_EBike_per_budget,
           input$in_PHEV_per_budget,
           input$in_FCEV_per_budget) > 100) {
      #If so, update the last slider so that everything sums to 100
      updateSliderInput(session,
                        inputId = "in_FCEV_per_budget",
                        value = 100 - (input$in_BEV_per_budget + input$in_EBike_per_budget + input$in_PHEV_per_budget)
      )
    }  
  })

  
  #================================#
  #Calcs####
  #================================#
  
  #~IC####
  mileage_day <- reactive({
    Car_Trips_Daily_Avg <- input$in_Car_Trips_Daily_Avg
    Car_Trip_Avg_Length <- set_units(input$in_Car_Trip_Avg_Length, "mi")
    Car_Trips_Daily_Avg * Car_Trip_Avg_Length #calculate total mileage/day
  })
  IC_Fuel_Economy <- reactive({set_units(input$in_IC_Fuel_Economy, "mi / gal")})
  IC_emissions_year <- reactive({calc_IC_emissions_year(mileage_day(), IC_Fuel_Economy())}) #Calculate IC year emissions using helper function
  
  #~BEV####
  Elec_gen_emissions <- reactive({set_units(input$in_elec_gen_emissions, "pounds / megawatthour")})
  BEV_emissions_year <- reactive({calc_BEV_emissions_year(input$in_BEV_econ, Elec_gen_emissions(), mileage_day())}) #calculate BEV year emissions using helper function
  BEV_CO2_saved <- reactive({calc_BEV_CO2_saved(IC_emissions_year(), BEV_emissions_year())}) #Calculate BEV CO2 saved using helper function
  BEV_incentive <- reactive({input$in_BEV_incentive})
  
  #~PHEV####
  PHEV_emissions_year <- reactive({calc_PHEV_emissions_year(input$in_PHEV_elec_econ, input$in_PHEV_range_elec, input$in_PHEV_ic_econ, mileage_day(), Elec_gen_emissions())})
    #calculate PHEV year emissions using helper function
  PHEV_CO2_saved <- reactive({calc_PHEV_CO2_saved(IC_emissions_year(), PHEV_emissions_year())}) #calculate PHEV CO2 saved using helper function
  PHEV_incentive <- reactive({input$in_PHEV_incentive})
  
  #~FCEV####
  FCEV_emissions_year <- reactive({calc_FCEV_emissions_year(input$in_FCEV_econ, input$in_renew_energy_ratio, mileage_day(), Elec_gen_emissions())})
    #calculate FCEV year emissions using helper function
  FCEV_CO2_saved <- reactive({calc_FCEV_CO2_saved(IC_emissions_year(), FCEV_emissions_year())}) #calculate FCEV CO2 saved using helper function
  FCEV_incentive <- reactive({input$in_FCEV_incentive})
  
  #~E-Bike#####
  VMT_r <- reactive({input$in_EBike_VMT_r}) #Ratio of car mileage replaced by e-bike
  EBike_emissions_year <- reactive({calc_EBike_emissions_year(input$in_EBike_Battery_Storage, input$in_EBike_Range, Elec_gen_emissions(), mileage_day(), VMT_r())})
    #calculate E-Bike year emissions using helper function
  EBike_CO2_saved <- reactive({calc_EBike_CO2_saved(mileage_day(), VMT_r(), IC_Fuel_Economy(), IC_emissions_year(), EBike_emissions_year())})
    #calculate E-Bike CO2 saved using helper function
  EBike_incentive <- reactive({input$in_EBike_incentive})
  
  #~Cost/kg CO2 saved####
  costperkg <- reactive({ #done
    incentive_range = costperkg_x() #Set the range of incentive dollar amounts
    BEV_costperkg <- tibble(mode = "BEV",
                            incentive = incentive_range,
                            costperkg = calc_costperkg(incentive_range, BEV_CO2_saved())) #Put these values in a tibble for plotting
    EBike_costperkg <- tibble(mode = "EBike",
                              incentive = incentive_range,
                              costperkg = calc_costperkg(incentive_range, EBike_CO2_saved())) #Put these values in a tibble for plotting
    PHEV_costperkg <- tibble(mode = "PHEV",
                             incentive = incentive_range,
                             costperkg = calc_costperkg(incentive_range, PHEV_CO2_saved())) #Put these values in a tibble for plotting
    FCEV_costperkg <- tibble(mode = "FCEV",
                             incentive = incentive_range,
                             costperkg = calc_costperkg(incentive_range, FCEV_CO2_saved())) #Put these values in a tibble for plotting
    
    tibble() %>% #combined table for plotting
      bind_rows(EBike_costperkg, BEV_costperkg, PHEV_costperkg, FCEV_costperkg) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #~~Pull specific points of interest####
  #Grab e-bike test point
  EBike_incentive_test_point <- reactive({
    #e-bike cost per kg CO2 saved
    EBike_costperkg <- calc_costperkg(incentive = EBike_incentive(), CO2_saved = EBike_CO2_saved())
    #e-bike incentive test point
    tibble(mode = "EBike", incentive = EBike_incentive(), costperkg = EBike_costperkg, CO2_saved = EBike_CO2_saved())
  })
  
  #Grab BEV test point
  BEV_incentive_test_point <- reactive({
    #BEV cost per kg CO2 saved
    BEV_costperkg <- calc_costperkg(incentive = BEV_incentive(), CO2_saved = BEV_CO2_saved())
    #BEV incentive test point
    tibble(mode = "BEV", incentive = BEV_incentive(), costperkg = BEV_costperkg, CO2_saved = BEV_CO2_saved())
  })
  
  #Grab PHEV test point
  PHEV_incentive_test_point <- reactive({
    #PHEV cost per kg CO2 saved
    PHEV_costperkg <- calc_costperkg(incentive = PHEV_incentive(), CO2_saved = PHEV_CO2_saved())
    #PHEV incentive test point
    tibble(mode = "PHEV", incentive = PHEV_incentive(), costperkg = PHEV_costperkg, CO2_saved = PHEV_CO2_saved())
  })
  
  #Grab FCEV test point
  FCEV_incentive_test_point <- reactive({
    #FCEV cost per kg CO2 saved
    FCEV_costperkg <- calc_costperkg(incentive = FCEV_incentive(), CO2_saved = FCEV_CO2_saved())
    #FCEV incentive test point
    tibble(mode = "FCEV", incentive = FCEV_incentive(), costperkg = FCEV_costperkg, CO2_saved = FCEV_CO2_saved())
  })
  
  #Bind all test points together for plotting
  test_points <- reactive({
    #bind to single testpoints tibble
    EBike_incentive_test_point() %>% 
      bind_rows(BEV_incentive_test_point(), PHEV_incentive_test_point(), FCEV_incentive_test_point()) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================#
  
  #~Total vehicles incentivized and total budget####
  num_incentivized <- reactive({
    total_budget_range = c(min(num_x()[1], CO2_saved_x()[1]), max(num_x()[2], CO2_saved_x()[2])) #This ensures that the full frame is filled even if the x axes sliders are different on the two graph sets
    BEV_num_incentivized = tibble(budget = total_budget_range,
                                  mode = "BEV",
                                  num = total_budget_range / BEV_incentive())
    EBike_num_incentivized = tibble(budget = total_budget_range,
                                    mode = "EBike",
                                    num = total_budget_range / EBike_incentive())
    PHEV_num_incentivized = tibble(budget = total_budget_range,
                                   mode = "PHEV",
                                   num = total_budget_range / PHEV_incentive())
    FCEV_num_incentivized = tibble(budget = total_budget_range,
                                   mode = "FCEV",
                                   num = total_budget_range / FCEV_incentive())
    tibble() %>% 
      bind_rows(EBike_num_incentivized, BEV_num_incentivized, PHEV_num_incentivized, FCEV_num_incentivized) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #~~Pull specific points of interest####
  test_budget_points <- reactive({
    test_budget <- input$in_test_budget
    BEV_budget_test_point <- tibble(mode = "BEV",
                                    budget = test_budget,
                                    num = budget / BEV_incentive())
    EBike_budget_test_point <- tibble(mode = "EBike",
                                      budget = test_budget,
                                      num = budget / EBike_incentive())
    PHEV_budget_test_point <- tibble(mode = "PHEV",
                                     budget = test_budget,
                                     num = budget / PHEV_incentive())
    FCEV_budget_test_point <- tibble(mode = "FCEV",
                                     budget = test_budget,
                                     num = budget / FCEV_incentive())
    EBike_budget_test_point %>% 
      bind_rows(BEV_budget_test_point, PHEV_budget_test_point, FCEV_budget_test_point) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================#
  
  #~Total CO2 saved per year and total budget####
  CO2_saved <- reactive({
    num_incentivized() %>% 
      mutate(CO2_saved = case_when(mode == "BEV" ~ num * BEV_CO2_saved(),
                                     mode == "EBike" ~ num * EBike_CO2_saved(),
                                     mode == "PHEV" ~ num * PHEV_CO2_saved(),
                                     mode == "FCEV" ~ num * FCEV_CO2_saved(),
                                     TRUE ~ 0)) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #~~Pull specific points of interest####
  test_budget_points_w_CO2 <- reactive({
    test_budget_points() %>% 
      left_join(test_points() %>% select(mode, CO2_saved), by = "mode") %>% #join CO2 saved per vehicle info
      mutate(total_CO2_saved = CO2_saved * num) %>% #calculated total CO2 saved
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include)   #Exclude items that are not selected in GUI
  })
  
  #================================#
  
  #~Budget Distribution Specific Total vehicles incentivized and total budget####
  num_incentivized_distrib <- reactive({
    total_budget_range = c(min(num_x()[1], CO2_saved_x()[1]), max(num_x()[2], CO2_saved_x()[2])) #This ensures that the full frame is filled even if the x axes sliders are different on the two graph sets
    BEV_num_incentivized = tibble(budget = total_budget_range,
                                  mode = "BEV",
                                  in_per_budget = input$in_BEV_per_budget * .01,
                                  num = total_budget_range * (input$in_BEV_per_budget * .01) / BEV_incentive())
    EBike_num_incentivized = tibble(budget = total_budget_range,
                                    mode = "EBike",
                                    in_per_budget = input$in_EBike_per_budget * .01,
                                    num = total_budget_range * (input$in_EBike_per_budget * .01) / EBike_incentive())
    PHEV_num_incentivized = tibble(budget = total_budget_range,
                                   mode = "PHEV",
                                   in_per_budget = input$in_PHEV_per_budget * .01,
                                   num = total_budget_range * (input$in_PHEV_per_budget * .01) / PHEV_incentive())
    FCEV_num_incentivized = tibble(budget = total_budget_range,
                                   mode = "FCEV",
                                   in_per_budget = input$in_FCEV_per_budget * .01,
                                   num = total_budget_range * (input$in_FCEV_per_budget * .01) / FCEV_incentive())
    tibble() %>% 
      bind_rows(EBike_num_incentivized, BEV_num_incentivized, PHEV_num_incentivized, FCEV_num_incentivized) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #~~Pull specific points of interest####
  test_budget_points_distrib <- reactive({
    test_budget <- input$in_test_budget
    BEV_budget_test_point <- tibble(mode = "BEV",
                                    budget = test_budget,
                                    budget_portion_perct = (input$in_BEV_per_budget * .01),
                                    budget_portion = (input$in_BEV_per_budget * .01) * test_budget,
                                    num = budget_portion / BEV_incentive())
    EBike_budget_test_point <- tibble(mode = "EBike",
                                      budget = test_budget,
                                      budget_portion_perct = (input$in_EBike_per_budget * .01),
                                      budget_portion = (input$in_EBike_per_budget * .01) * test_budget,
                                      num = budget_portion / EBike_incentive())
    PHEV_budget_test_point <- tibble(mode = "PHEV",
                                     budget = test_budget,
                                     budget_portion_perct = (input$in_PHEV_per_budget * .01),
                                     budget_portion = (input$in_PHEV_per_budget * .01) * test_budget,
                                     num = budget_portion / PHEV_incentive())
    FCEV_budget_test_point <- tibble(mode = "FCEV",
                                     budget = test_budget,
                                     budget_portion_perct = (input$in_FCEV_per_budget * .01),
                                     budget_portion = (input$in_FCEV_per_budget * .01) * test_budget,
                                     num = budget_portion / FCEV_incentive())

    EBike_budget_test_point %>% 
      bind_rows(BEV_budget_test_point, PHEV_budget_test_point, FCEV_budget_test_point) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================#
  
  #~Budget Distribution Specific Total CO2 saved per year and total budget####
  CO2_saved_distrib <- reactive({
    num_incentivized_distrib() %>% 
      mutate(CO2_saved = case_when(mode == "BEV" ~ num * BEV_CO2_saved(),
                                     mode == "EBike" ~ num * EBike_CO2_saved(),
                                     mode == "PHEV" ~ num * PHEV_CO2_saved(),
                                     mode == "FCEV" ~ num * FCEV_CO2_saved(),
                                     TRUE ~ 0)) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #~~Pull specific points of interest####
  test_budget_points_w_CO2_distrib <- reactive({
    test_budget_points_distrib() %>% 
      left_join(test_points() %>% select(mode, CO2_saved), by = "mode") %>% #join per vehicle CO2 saved info
      mutate(total_CO2_saved = num * CO2_saved) %>% #calculate total CO2 saved info
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================#
  #Plots####
  #================================#
  #~Cost per kg CO2 saved by mode####
  output$g1 <- renderPlot({
    g1plot(costperkg(), test_points(), costperkg_x(), costperkg_y(), mode_scale_colors)
  })
  #~Number incentivized####
  output$g2 <- renderPlot({
    g2plot(num_incentivized(), test_budget_points(), num_x(), num_y(), mode_scale_colors)
  })
  #~CO2 saved####
  output$g3 <- renderPlot({
    g3plot(CO2_saved(), test_budget_points_w_CO2(), num_incentivized(), CO2_saved_x(), CO2_saved_y(), mode_scale_colors)
  })
  #~Budget distribution specific number incentivized####
  output$g4 <- renderPlot({
    g4plot(num_incentivized(), num_incentivized_distrib(), test_budget_points_distrib(), num_x(), num_y(), mode_scale_colors, mode_scale_fill)
  })
  #~Budget distribution specific CO2 saved####
  output$g5 <- renderPlot({
    g5plot(num_incentivized(), CO2_saved_distrib(), test_budget_points_w_CO2_distrib(), CO2_saved_x(), CO2_saved_y(), mode_scale_colors, mode_scale_fill)
  })
  
  #================================#
  #Report####
  #================================#
  
  #~Generate a downloadable report####
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      #copy report file to temporary directory before processing it
      tempdir1 <- tempdir()
      tempReport <- file.path(tempdir1, "report.Rmd")
      tempLogo <- file.path(tempdir1, "logo.png")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("www/TREC_horiz4c_narrow_tag.png", tempLogo, overwrite = TRUE)
      
      #Set up parameters to pass to Rmd document
      #params <- reactiveValuesToList(input)
      
      #Knit the document, passing in the 'params' list,
      #and eval it in a child of the global environment
      #(this isolates the codes in the document from the code in this app)
      rmarkdown::render(tempReport, output_file = file,
                        params = list(Car_Trip_Avg_Length = input$in_Car_Trip_Avg_Length,
                                      EBike_per_budget = input$in_EBike_per_budget,
                                      test_budget = input$in_test_budget,
                                      PHEV_incentive = input$in_PHEV_incentive,
                                      BEV_econ = input$in_BEV_econ,
                                      FCEV_incentive = input$in_FCEV_incentive,
                                      preset_FCEV = input$in_preset_FCEV,
                                      FCEV_include = input$in_FCEV_include,
                                      preset_EBike = input$in_preset_EBike,
                                      EBike_include = input$in_EBike_include,
                                      BEV_incentive = input$in_BEV_incentive,
                                      EBike_Battery_Storage = input$in_EBike_Battery_Storage,
                                      EBike_incentive = input$in_EBike_incentive,
                                      EBike_Range = input$in_EBike_Range,
                                      BEV_per_budget = input$in_BEV_per_budget,
                                      EBike_VMT_r = input$in_EBike_VMT_r,
                                      preset_Car_Trips_Daily_Avg = input$in_preset_Car_Trips_Daily_Avg,
                                      FCEV_econ = input$in_FCEV_econ,
                                      PHEV_elec_econ = input$in_PHEV_elec_econ,
                                      PHEV_range_elec = input$in_PHEV_range_elec,
                                      preset_PHEV = input$in_preset_PHEV,
                                      BEV_include = input$in_BEV_include,
                                      elec_gen_emissions = input$in_elec_gen_emissions,
                                      preset_elec_gen_emissions = input$in_preset_elec_gen_emissions,
                                      PHEV_per_budget = input$in_PHEV_per_budget,
                                      preset_BEV = input$in_preset_BEV,
                                      FCEV_per_budget = input$in_FCEV_per_budget,
                                      IC_Fuel_Economy = input$in_IC_Fuel_Economy,
                                      Car_Trips_Daily_Avg = input$in_Car_Trips_Daily_Avg,
                                      PHEV_ic_econ = input$in_PHEV_ic_econ,
                                      renew_energy_ratio = input$in_renew_energy_ratio,
                                      PHEV_include = input$in_PHEV_include,
                                      preset_IC_Fuel_Economy = input$in_preset_IC_Fuel_Economy,
                                      costperkg = costperkg(),
                                      costperkg_x = costperkg_x(),
                                      costperkg_y = costperkg_y(),
                                      test_points = test_points(),
                                      num_incentivized = num_incentivized(),
                                      test_budget_points = test_budget_points(),
                                      num_x = num_x(),
                                      num_y = num_y(),
                                      CO2_saved = CO2_saved(),
                                      test_budget_points_w_CO2 = test_budget_points_w_CO2(),
                                      CO2_saved_x = CO2_saved_x(),
                                      CO2_saved_y = CO2_saved_y(),
                                      num_incentivized_distrib = num_incentivized_distrib(),
                                      test_budget_points_distrib = test_budget_points_distrib(),
                                      CO2_saved_distrib = CO2_saved_distrib(),
                                      test_budget_points_w_CO2_distrib = test_budget_points_w_CO2_distrib(),
                                      mode_scale_colors = mode_scale_colors,
                                      mode_scale_fill = mode_scale_fill,
                                      in_preset_Car_Trips_Daily_Avg = input$in_preset_Car_Trips_Daily_Avg,
                                      flag_preset_Car_Trips_Daily_Avg = states$flag_preset_Car_Trips_Daily_Avg,
                                      in_preset_elec_gen_emissions = input$in_preset_elec_gen_emissions,
                                      flag_preset_elec_gen_emissions = states$flag_preset_elec_gen_emissions,
                                      in_preset_IC_Fuel_Economy = input$in_preset_IC_Fuel_Economy,
                                      flag_in_preset_IC_Fuel_Economy = states$flag_in_preset_IC_Fuel_Economy,
                                      in_preset_EBike = input$in_preset_EBike,
                                      flag_in_preset_EBike = states$flag_in_preset_EBike,
                                      in_preset_BEV = input$in_preset_BEV,
                                      flag_in_preset_BEV = states$flag_in_preset_BEV,
                                      in_preset_PHEV = input$in_preset_PHEV,
                                      flag_in_preset_PHEV = states$flag_in_preset_PHEV,
                                      in_preset_FCEV = input$in_preset_FCEV,
                                      flag_in_preset_FCEV = states$flag_in_preset_FCEV
                                      ),
                        envir = new.env(parent = globalenv())
                        )
    }
  )
}