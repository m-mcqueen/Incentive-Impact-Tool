#E-bike Incentive Impact Tool
#Server
#Mike McQueen

#Load Packages
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(units)
library(measurements)
library(stringr)



# Define server logic required calculate and draw plots
server <-  function(input, output, session) {
  
  #================================
  #Text input default value updaters
  #================================
  #Update Trips panel with preset
  observeEvent(input$apply_in_preset_Car_Trips_Daily_Avg, {
    updateNumericInput(session, inputId = "in_Car_Trips_Daily_Avg",
                       value = Trips %>%
                         filter(State == input$in_preset_Car_Trips_Daily_Avg) %>%
                         pull(Car_Trips_Daily_Avg)
    )
    updateNumericInput(session, inputId = "in_Car_Trip_Avg_Length",
                       value = drop_units(Trips %>%
                                            filter(State == input$in_preset_Car_Trips_Daily_Avg) %>%
                                            pull(Car_Trip_Avg_Length)
                       )
    )
  })
  #Update Power Generation panel with preset
  observeEvent(input$apply_in_preset_elec_gen_emissions, {
    updateNumericInput(session, inputId = "in_elec_gen_emissions",
                       value = drop_units(Electricity %>%
                                            filter(State == input$in_preset_elec_gen_emissions) %>%
                                            pull(CO2)
                       )
    )
  })
  
  #Update IC panel with preset
  observeEvent(input$apply_in_preset_IC_Fuel_Economy, {
    make_and_model = str_split(input$in_preset_IC_Fuel_Economy, " ")
    updateNumericInput(session, inputId = "in_IC_Fuel_Economy",
                       value = drop_units(IC %>%
                                            filter(Make == make_and_model[[1]][1],
                                                   Model == make_and_model[[1]][2]) %>%
                                            pull(Fuel_Economy)
                       )
    )
  })
  #Update E-Bike panel with preset
  observeEvent(input$apply_in_preset_EBike, {
    make_and_model = str_split(input$in_preset_EBike, " ")
    updateNumericInput(session, inputId = "in_EBike_Battery_Storage",
                       value = as.numeric(EBike %>%
                                            filter(Make == make_and_model[[1]][1],
                                                   Model == make_and_model[[1]][2]) %>%
                                            pull(Battery_Storage)
                       )
    )
    updateNumericInput(session, inputId = "in_EBike_Range",
                       value = as.numeric(EBike %>%
                                            filter(Make == make_and_model[[1]][1],
                                                   Model == make_and_model[[1]][2]) %>%
                                            pull(Range)
                       )
    )
  })
  #Update BEV panel with preset
  observeEvent(input$apply_in_preset_BEV, {
    updateNumericInput(session, inputId = "in_BEV_econ",
                       value = round(as.numeric(mix %>% 
                                                  filter(mix_type == "BEV",
                                                         mix_name == input$in_preset_BEV) %>% 
                                                  pull(epa_econ_wm)), 2))
  })
  #Update PHEV panel with preset
  observeEvent(input$apply_in_preset_PHEV, {
    updateNumericInput(session, inputId = "in_PHEV_elec_econ",
                       value = round(as.numeric(mix %>% 
                                                  filter(mix_type == "PHEV",
                                                         mix_name == input$in_preset_PHEV) %>% 
                                                  pull(epa_elec_econ_wm))))
    updateNumericInput(session, inputId = "in_PHEV_range_elec",
                       value = round(as.numeric(mix %>% 
                                                  filter(mix_type == "PHEV",
                                                         mix_name == input$in_preset_PHEV) %>% 
                                                  pull(range_elec_wm))))
    updateNumericInput(session, inputId = "in_PHEV_ic_econ",
                       value = round(as.numeric(mix %>%
                                                  filter(mix_type == "PHEV",
                                                         mix_name == input$in_preset_PHEV) %>% 
                                                  pull(epa_ic_econ_wm))))
  })
  #Update FCEV panel with preset
  observeEvent(input$apply_in_preset_FCEV, {
    updateNumericInput(session, inputId = "in_FCEV_econ",
                       value = round(as.numeric(mix %>% 
                                                  filter(mix_type == "FCEV",
                                                         mix_name == input$in_preset_FCEV) %>% 
                                                  pull(epa_h2_econ_wm)), 2))
  })
  #Update plots that use budget allotments
  observeEvent(input$in_update_budget_per, {
    update
  })
  
  #================================
  #Calcs
  #================================
  
  #IC
  mileage_day <- reactive({
    Car_Trips_Daily_Avg <- input$in_Car_Trips_Daily_Avg
    Car_Trip_Avg_Length <- set_units(input$in_Car_Trip_Avg_Length, "mi")
    Car_Trips_Daily_Avg * Car_Trip_Avg_Length #calculate total mileage/day
  })
  IC_Fuel_Economy <- reactive({set_units(input$in_IC_Fuel_Economy, "mi / gal")})
  IC_emissions_year <- reactive({
    IC_emissions_day <- set_units(drop_units(set_units(mileage_day() / IC_Fuel_Economy(), "kiloCO2_g")), "kg") #calculate emissions and convert to kg
    IC_emissions_day * 365 #multiply to get year total
  })
  
  #BEV
  Elec_gen_emissions <- reactive({set_units(input$in_elec_gen_emissions, "pounds / megawatthour")})
  BEV_emissions_year <- reactive({
    BEV_Fuel_Economy <- set_units(input$in_BEV_econ, "kilowatthour / hectomi")
    BEV_emissions_day <- set_units(Elec_gen_emissions() * mileage_day() * BEV_Fuel_Economy, "kg") #calculate emissions and convert to kg
    BEV_emissions_day * 365 #multiply to get year total
  })
  BEV_CO2_saved <- reactive({drop_units(IC_emissions_year()) - drop_units(BEV_emissions_year())})
  BEV_incentive <- reactive({round(input$in_BEV_incentive, -1)})
  
  #PHEV
  PHEV_emissions_year <- reactive({
    PHEV_elec_econ <- set_units(input$in_PHEV_elec_econ, "kilowatthour / hectomi")
    PHEV_range_elec <- set_units(input$in_PHEV_range_elec, "mi")
    PHEV_ic_econ <- set_units(input$in_PHEV_ic_econ, "mi / gal")
    if(PHEV_range_elec > mileage_day()) { 
      elec_dist <- mileage_day() #if the elec range is greater than the mileage_day, set elec_dist to mileage_day
    } else {
      elec_dist <- PHEV_range_elec #if the total distance was larger than the range, set the elec distance to the elec range
    }
    PHEV_elec_emissions_day <- set_units(Elec_gen_emissions() * elec_dist * PHEV_elec_econ, "kg") #calculate emissions and convert to kg
    PHEV_ic_emissions_day <- set_units(
      if (mileage_day() > PHEV_range_elec) {
        drop_units(set_units((mileage_day() - PHEV_range_elec) / PHEV_ic_econ, "kiloCO2_g"))
      } else {0}
      , "kg")
    PHEV_total_emissions_day <- PHEV_elec_emissions_day + PHEV_ic_emissions_day
    PHEV_total_emissions_day * 365
  })
  PHEV_CO2_saved <- reactive({drop_units(IC_emissions_year()) - drop_units(PHEV_emissions_year())})
  PHEV_incentive <- reactive({round(input$in_PHEV_incentive, -1)})
  
  #FCEV
  FCEV_emissions_year <- reactive({
    FCEV_H2_econ <- set_units(input$in_FCEV_econ, "mi / kg") #get H2 fuel economy
    FCEV_renew_energy_ratio <- input$in_renew_energy_ratio #get ratio of energy required for electrolysis that is 100% renewable (zero emissions)
    FCEV_H2_required_day <- mileage_day() / FCEV_H2_econ #get required H2 per day
    FCEV_elec_required_day <- set_units(set_units(drop_units(FCEV_H2_required_day), "H2_kg"), "kiloWatthour") #get electricity required for 1 day of average driving defined on trips tab
    FCEV_nonrenew_elec_required_day <- FCEV_elec_required_day * (1 - FCEV_renew_energy_ratio) #get non-renewable electricity total that is required
    FCEV_emissions_day <- set_units(FCEV_nonrenew_elec_required_day * Elec_gen_emissions(), "kg") #calculate emissions from non-renewable electricity and convert to kg based on generation profile selected
    FCEV_emissions_day * 365 #multiply to get year total
  })
  FCEV_CO2_saved <- reactive({drop_units(IC_emissions_year()) - drop_units(FCEV_emissions_year())})
  FCEV_incentive <- reactive({round(input$in_FCEV_incentive, -1)})
  
  #E-Bike
  VMT_r <- reactive({input$in_EBike_VMT_r}) #Ratio of car mileage replaced by e-bike
  EBike_emissions_year <- reactive({
    EBike_Battery_Storage <- set_units(input$in_EBike_Battery_Storage, "Watthour")
    EBike_Range <- set_units(input$in_EBike_Range, "mi")
    EBike_Fuel_Economy <- EBike_Range / EBike_Battery_Storage #calculate "fuel economy" based on range and battery storage
    EBike_emissions_day <- set_units(Elec_gen_emissions() * (mileage_day() * VMT_r()) / EBike_Fuel_Economy, "kg") #calculate emissions (taking into account portion of miles travelled by e-bike) and convert to kg
    EBike_emissions_day * 365 #multiply to get year total
  })
  EBike_CO2_saved <- reactive({
    IC_remaining_emissions_day <- set_units(drop_units(set_units(mileage_day() * (1 - VMT_r()) / IC_Fuel_Economy(), "kiloCO2_g")), "kg") #Get the remaining emissions from the remaining IC mileage. Change to kg
    IC_remaining_emissions_year <- IC_remaining_emissions_day * 365 #multiply to get year total
    drop_units(IC_emissions_year()) - (drop_units(EBike_emissions_year()) + drop_units(IC_remaining_emissions_year))
  })
  EBike_incentive <- reactive({round(input$in_EBike_incentive, -1)})
  
  #Cost/kg CO2 saved
  costperkg <- reactive({
    incentive_range = seq(0, 5000, by = 1000) #Set the range of incentive dollar amounts
    
    BEV_CO2_saved_cost <- incentive_range / BEV_CO2_saved() #Calculate the cost per kg CO2 saved for BEV
    BEV_costperkg <- tibble(mode = "BEV", incentive = incentive_range, costperkg = BEV_CO2_saved_cost) #Put these values in a tibble for plotting
    
    EBike_CO2_saved_cost <- incentive_range / EBike_CO2_saved() #Calculate the cost per kg CO2 saved for EBike
    EBike_costperkg <- tibble(mode = "EBike", incentive = incentive_range, costperkg = EBike_CO2_saved_cost) #Put these values in a tibble for plotting
    
    PHEV_CO2_saved_cost <- incentive_range / PHEV_CO2_saved() #Calculate the cost per kg CO2 saved for PHEV
    PHEV_costperkg <- tibble(mode = "PHEV", incentive = incentive_range, costperkg = PHEV_CO2_saved_cost) #Put these values in a tibble for plotting
    
    FCEV_CO2_saved_cost <- incentive_range / FCEV_CO2_saved() #Calculate the cost per kg CO2 saved for FCEV
    FCEV_costperkg <- tibble(mode = "FCEV", incentive = incentive_range, costperkg = FCEV_CO2_saved_cost) #Put these values in a tibble for plotting
    
    tibble() %>% #combined table for plotting
      bind_rows(BEV_costperkg, EBike_costperkg, PHEV_costperkg, FCEV_costperkg) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #Pull specific points of interest
  test_points <- reactive({
    #Grab e-bike test point
    EBike_incentive_test_point <- costperkg() %>%
      filter(incentive == EBike_incentive(),
             mode == "EBike")
    #Grab BEV test point
    BEV_incentive_test_point <- costperkg() %>%
      filter(incentive == BEV_incentive(),
             mode == "BEV")
    
    #Grab PHEV test point
    PHEV_incentive_test_point <- costperkg() %>% 
      filter(incentive == PHEV_incentive(),
             mode == "PHEV")
    
    #Grab FCEV test point
    FCEV_incentive_test_point <- costperkg() %>% 
      filter(incentive == FCEV_incentive(),
             mode == "FCEV")
    
    EBike_incentive_test_point %>% #bind to single testpoints tibble
      bind_rows(BEV_incentive_test_point, PHEV_incentive_test_point, FCEV_incentive_test_point) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================
  
  #Total vehicles incentivized and total budget
  num_incentivized <- reactive({
    total_budget_range = seq(1e6, 5e6, by = 1e6)
    BEV_num_incentivized = tibble(budget = total_budget_range, mode = "BEV", num = total_budget_range / BEV_incentive())
    EBike_num_incentivized = tibble(budget = total_budget_range, mode = "EBike", num = total_budget_range / EBike_incentive())
    PHEV_num_incentivized = tibble(budget = total_budget_range, mode = "PHEV", num = total_budget_range / PHEV_incentive())
    FCEV_num_incentivized = tibble(budget = total_budget_range, mode = "FCEV", num = total_budget_range / FCEV_incentive())
    tibble() %>% 
      bind_rows(BEV_num_incentivized, EBike_num_incentivized, PHEV_num_incentivized, FCEV_num_incentivized) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #Pull specific points of interest
  test_budget_points <- reactive({
    test_budget <- input$in_test_budget
    BEV_budget_test_point <- tibble(mode = "BEV",
                                    budget = test_budget,
                                    num = num_incentivized() %>%
                                      filter(mode == "BEV" & budget == test_budget) %>%
                                      pull(num))
    EBike_budget_test_point <- tibble(mode = "EBike",
                                      budget = test_budget,
                                      num = num_incentivized() %>%
                                        filter(mode == "EBike" & budget == test_budget) %>%
                                        pull(num))
    PHEV_budget_test_point <- tibble(mode = "PHEV",
                                     budget = test_budget,
                                     num = num_incentivized() %>%
                                       filter(mode == "PHEV" & budget == test_budget) %>%
                                       pull(num))
    FCEV_budget_test_point <- tibble(mode = "FCEV",
                                     budget = test_budget,
                                     num = num_incentivized() %>% 
                                       filter(mode == "FCEV" & budget == test_budget) %>% 
                                       pull(num))
    EBike_budget_test_point %>% 
      bind_rows(BEV_budget_test_point, PHEV_budget_test_point, FCEV_budget_test_point) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================
  
  #Total CO2 avoided per year and total budget
  CO2_avoided <- reactive({
    num_incentivized() %>% 
      mutate(CO2_avoided = case_when(mode == "BEV" ~ num * BEV_CO2_saved(),
                                     mode == "EBike" ~ num * EBike_CO2_saved(),
                                     mode == "PHEV" ~ num * PHEV_CO2_saved(),
                                     mode == "FCEV" ~ num * FCEV_CO2_saved(),
                                     TRUE ~ 0)) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #Pull specific points of interest
  test_budget_points_w_CO2 <- reactive({
    test_budget_points() %>% 
      left_join(CO2_avoided() %>% select(mode, budget, CO2_avoided), by = c("mode", "budget")) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================
  
  #Budget Distribution Specific Total vehicles incentivized and total budget
  num_incentivized_distrib <- reactive({
    total_budget_range = seq(1e6, 5e6, by = 1e6)
    BEV_num_incentivized = tibble(budget = total_budget_range, mode = "BEV", num = total_budget_range * (input$in_BEV_per_budget * .01) / BEV_incentive())
    EBike_num_incentivized = tibble(budget = total_budget_range, mode = "EBike", num = total_budget_range * (input$in_EBike_per_budget * .01) / EBike_incentive())
    PHEV_num_incentivized = tibble(budget = total_budget_range, mode = "PHEV", num = total_budget_range * (input$in_PHEV_per_budget * .01) / PHEV_incentive())
    FCEV_num_incentivized = tibble(budget = total_budget_range, mode = "FCEV", num = total_budget_range * (input$in_FCEV_per_budget * .01) / FCEV_incentive())
    tibble() %>% 
      bind_rows(BEV_num_incentivized, EBike_num_incentivized, PHEV_num_incentivized, FCEV_num_incentivized) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #Pull specific points of interest
  test_budget_points_distrib <- reactive({
    test_budget <- input$in_test_budget
    BEV_budget_test_point <- tibble(mode = "BEV",
                                    budget = test_budget,
                                    num = num_incentivized_distrib() %>%
                                      filter(mode == "BEV" & budget == test_budget) %>%
                                      pull(num))
    EBike_budget_test_point <- tibble(mode = "EBike",
                                      budget = test_budget,
                                      num = num_incentivized_distrib() %>%
                                        filter(mode == "EBike" & budget == test_budget) %>%
                                        pull(num))
    PHEV_budget_test_point <- tibble(mode = "PHEV",
                                     budget = test_budget,
                                     num = num_incentivized_distrib() %>%
                                       filter(mode == "PHEV" & budget == test_budget) %>%
                                       pull(num))
    FCEV_budget_test_point <- tibble(mode = "FCEV",
                                     budget = test_budget,
                                     num = num_incentivized_distrib() %>% 
                                       filter(mode == "FCEV" & budget == test_budget) %>% 
                                       pull(num))
    EBike_budget_test_point %>% 
      bind_rows(BEV_budget_test_point, PHEV_budget_test_point, FCEV_budget_test_point) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================
  
  #Budget Distribution Specific Total CO2 avoided per year and total budget
  CO2_avoided_distrib <- reactive({
    num_incentivized_distrib() %>% 
      mutate(CO2_avoided = case_when(mode == "BEV" ~ num * BEV_CO2_saved(),
                                     mode == "EBike" ~ num * EBike_CO2_saved(),
                                     mode == "PHEV" ~ num * PHEV_CO2_saved(),
                                     mode == "FCEV" ~ num * FCEV_CO2_saved(),
                                     TRUE ~ 0)) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #Pull specific points of interest
  test_budget_points_w_CO2_distrib <- reactive({
    test_budget_points() %>% 
      left_join(CO2_avoided_distrib() %>% select(mode, budget, CO2_avoided), by = c("mode", "budget")) %>% 
      exclude_items(input$in_EBike_include, input$in_PHEV_include, input$in_BEV_include, input$in_FCEV_include) #Exclude items that are not selected in GUI
  })
  
  #================================
  #Plots
  #================================
  #Cost per kg CO2 avoided by mode
  output$g1 <- renderPlot({
    ggplot(costperkg(), aes(incentive, costperkg, color=mode)) +
      coord_cartesian(xlim = c(min(costperkg()$incentive), max(costperkg()$incentive)), ylim = c(0, 4)) +
      geom_line(size = 1.5) + #plot mode lines
      geom_point(data = test_points(), aes(incentive, costperkg, color = mode), size = 5) + #plot specific test points
      geom_segment(data = test_points(), aes(x = 0, y = costperkg, xend = incentive, yend = costperkg, color = mode), linetype = "dashed", size = 1.5) +
      geom_segment(data = test_points(), aes(x = incentive, y = 0, xend = incentive, yend = costperkg, color = mode), linetype = "dashed", size = 1.5)
  })
  #Number incentivized
  output$g2 <- renderPlot({
    ggplot(num_incentivized(), aes(budget, num, color = mode)) +
      geom_line(size = 1.5) + #plot mode lines
      coord_cartesian(ylim = c(0, 10000)) +
      geom_point(data = test_budget_points(), aes(budget, num, color = mode), size = 5) + #plot specific test points
      geom_segment(data = test_budget_points(), aes(x = min(num_incentivized()$budget), y = num, xend = budget, yend = num, color = mode), linetype = "dashed", size = 1.5) +
      geom_segment(data = test_budget_points(), aes(x = budget, y = 0, xend = budget, yend = num, color = mode), linetype = "dashed", size = 1.5)
  })
  #CO2 avoided
  output$g3 <- renderPlot({
    ggplot(CO2_avoided(), aes(budget, CO2_avoided, color = mode)) +
      geom_line(size = 1.5) +
      coord_cartesian(ylim = c(0, 1e7)) +
      geom_point(data = test_budget_points_w_CO2(), aes(budget, CO2_avoided), size = 5) +
      geom_segment(data = test_budget_points_w_CO2(), aes(x = min(num_incentivized()$budget), y = CO2_avoided, xend = budget, yend = CO2_avoided, color = mode), linetype = "dashed", size = 1.5) +
      geom_segment(data = test_budget_points_w_CO2(), aes(x = budget, y = 0, xend = budget, yend = CO2_avoided, color = mode), linetype = "dashed", size = 1.5)
  })
  #Budget distribution specific number incentivized
  output$g4 <- renderPlot({
    #Don't update this plot until the update button is pressed
    input$in_update_budget_per
    
    isolate(ggplot(num_incentivized_distrib(), aes(budget, num, fill = mode)) +
      geom_area() + 
      coord_cartesian(ylim = c(0, 10000)) +
      geom_point(data = test_budget_points_distrib(),
                 aes(budget, sum(num)),
                 size = 5) + #plot specific test points
      geom_segment(data = test_budget_points_distrib(), #horizontal line
                   aes(x = min(num_incentivized()$budget),
                       y = sum(num), xend = budget,
                       yend = sum(num)),
                   linetype = "dashed",
                   size = 1.5) +
      geom_segment(data = test_budget_points_distrib(), #vertical line
                   aes(x = budget, y = 0,
                       xend = budget,
                       yend = sum(num)),
                   linetype = "dashed",
                   size = 1.5)
      )
  })
  #Budget distribution specific CO2 avoided
  output$g5 <- renderPlot({
    #Don't update this plot until the update button is pressed
    input$in_update_budget_per
    
    isolate(ggplot(CO2_avoided_distrib(), aes(budget, CO2_avoided, fill = mode)) +
      geom_area() +
      coord_cartesian(ylim = c(0, 1e7)) +
      geom_point(data = test_budget_points_w_CO2_distrib(),
                 aes(budget,sum(CO2_avoided)),
                 size = 5) +
      geom_segment(data = test_budget_points_w_CO2_distrib(), #horizontal line
                   aes(x = min(num_incentivized()$budget),
                       y = sum(CO2_avoided), xend = budget,
                       yend = sum(CO2_avoided)),
                   linetype = "dashed", size = 1.5) +
      geom_segment(data = test_budget_points_w_CO2_distrib(), #vertical line
                   aes(x = budget,
                       y = 0,
                       xend = budget,
                       yend = sum(CO2_avoided)),
                   linetype = "dashed", size = 1.5)
    )
  })
  #Plot to show percentage of budget used
  output$g_budget_total <- renderPlot({
    total <-  sum(input$in_BEV_per_budget, input$in_EBike_per_budget, input$in_PHEV_per_budget, input$in_FCEV_per_budget)
    #Make it red if the total is more than 100
    color <- if (total > 100) {
      "red"
    } else if (total == 100) {
      "green"
      } else ("blue")
    #Build plot
    barplot(total, ylim = c(0,120), main = paste(total, "% Used", sep = ""), col = color)
  })
}