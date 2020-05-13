#E-bike Incentive Impact Tool
#Mike McQueen

#Load Packages
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(units)
library(measurements)
library(stringr)

#================================
#Load data tables
#================================
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

#================================
#Clean up tables to combine units with scalar values
#================================
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
    group_by(mix_type, mix_name) %>% #group by vehicle type (BEV, PHEV, etc.) and mix name (ex: OR_Feb_20)
    summarize(epa_econ_wm = weighted.mean(epa_econ, count, na.rm = T),
              epa_elec_econ_wm = weighted.mean(epa_elec_econ, count, na.rm = T),
              range_elec_wm = weighted.mean(range_elec, count, na.rm = T),
              epa_ic_econ_wm = weighted.mean(epa_ic_econ, count, na.rm = T),
              epa_h2_econ_wm = weighted.mean(epa_h2_econ, count, na.rm = T)) #Summarize the weighted average fuel economies and ranges by type and by mix

#Remove unecessary tables
rm(BEV_raw, PHEV_raw, FCEV_raw, EBike_raw, Electricity_raw, IC_raw, Incentives_raw, Trips_raw, mix_raw)

#================================
#Constants
#================================

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

#================================
#Exclude Items that are not selected
#================================
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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("E-Bike Incentive Cost and Impact"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        tabsetPanel(type = "tabs",
            tabPanel("Trips",
                        verticalLayout(
                        titlePanel("Trips"),
                        numericInput("in_Car_Trips_Daily_Avg",
                                     "Average Auto Trips per Day",
                                     value = 3.908,
                                     step = .1),
                        numericInput("in_Car_Trip_Avg_Length",
                                     "Average Auto Trip Length per Day (mi)",
                                     value = 9.86,
                                     step = .1),
                        selectInput("in_preset_Car_Trips_Daily_Avg",
                                    "Choose Preset:",
                                    Trips %>% pull(State),
                                    selected = "CA"),
                        actionButton("apply_in_preset_Car_Trips_Daily_Avg",
                                     "Apply Preset")
                        )
            ),
            tabPanel("Power",
                        verticalLayout(
                        titlePanel("Power Generation"),
                        numericInput("in_elec_gen_emissions",
                                     "State CO2 emissions rate for power generation (lb/MWh)",
                                     value = 420.4,
                                     step = 10),
                        selectInput("in_preset_elec_gen_emissions",
                                    "Choose Preset:",
                                    Electricity %>% pull(State),
                                    selected = "CA"),
                        actionButton("apply_in_preset_elec_gen_emissions",
                                     "Apply Preset")
                        )
            ),
            tabPanel("Incentives",
                verticalLayout(
                    titlePanel("Incentives"),
                    numericInput("in_BEV_incentive",
                                 "BEV incentive ($)",
                                 value = 2000,
                                 step = 50),
                    numericInput("in_PHEV_incentive",
                                 "PHEV incentive ($)",
                                 value = 1000,
                                 step = 50),
                    numericInput("in_FCEV_incentive",
                                 "FCEV incentive ($)",
                                 value = 4500),
                    numericInput("in_EBike_incentive",
                                 "E-Bike incentive ($)",
                                 value = 500,
                                 step = 50),
                    numericInput("in_test_budget",
                                 "Total budget ($)",
                                 value = 3000000,
                                 step = 10000)
                )
            ),
            tabPanel("IC",
                verticalLayout(
                    titlePanel("IC"),
                    numericInput("in_IC_Fuel_Economy",
                                 "Average Auto Fuel Economy (mpg)",
                                 value = 25.2,
                                 step = 1),
                    selectInput("in_preset_IC_Fuel_Economy",
                                "Choose Preset:",
                                paste(IC %>% pull(Make), IC %>% pull(Model)),
                                selected = "CA_Average CA_Average"),
                    actionButton("apply_in_preset_IC_Fuel_Economy",
                                 "Apply Preset")
                )
            ),
            tabPanel("E-Bike",
                verticalLayout(
                    titlePanel("E-Bike"),
                    checkboxInput("in_EBike_include",
                                  "Include",
                                  T),
                    numericInput("in_EBike_Battery_Storage",
                                 "Battery Storage (Wh)",
                                 value = 422,
                                 step = 10),
                    numericInput("in_EBike_Range",
                                 "Average Range (mi)",
                                 value = 23,
                                 step = 1),
                    numericInput("in_EBike_VMT_r",
                                 "E-Bike VMT Replacement Ratio",
                                 value = .15,
                                 step = .05),
                    selectInput("in_preset_EBike",
                                "Choose Preset:",
                                paste(EBike %>% pull(Make), EBike %>% pull(Model)),
                                selected = "Average Standard"),
                    actionButton("apply_in_preset_EBike",
                                 "Apply Preset")
                )
            ),
            tabPanel("BEV", 
                verticalLayout(
                        titlePanel("BEV"),
                        checkboxInput("in_BEV_include",
                                      "Include",
                                      T),
                        numericInput("in_BEV_econ",
                                     "Avg EV Fuel Economy (kWh/100 mi)",
                                     value = 30.73,
                                     step = 1),
                        selectInput("in_preset_BEV",
                                    "Choose Preset:",
                                    mix %>% filter(mix_type == "BEV") %>% pull(mix_name),
                                    selected = "OR_Feb_20"),
                        actionButton("apply_in_preset_BEV",
                                     "Apply Preset")
                    )
                ),
            tabPanel("PHEV", 
                     verticalLayout(
                         titlePanel("PHEV"),
                         checkboxInput("in_PHEV_include",
                                       "Include",
                                       F),
                         numericInput("in_PHEV_elec_econ",
                                      "Avg E-Mode Fuel Economy (kWh/100 mi)",
                                      value = 35,
                                      step = 1),
                         numericInput("in_PHEV_range_elec",
                                      "Avg E-Mode Range (mi)",
                                      value = 30,
                                      step = 1),
                         numericInput("in_PHEV_ic_econ",
                                      "Avg IC Fuel Economy (mpg)",
                                      value = 41,
                                      step = 1),
                         selectInput("in_preset_PHEV",
                                     "Choose Preset:",
                                     mix %>% filter(mix_type == "PHEV") %>% pull(mix_name),
                                     selected = "OR_Feb_20"),
                         actionButton("apply_in_preset_PHEV",
                                      "Apply Preset")
                     )
            ),
            tabPanel("FCEV", 
                     verticalLayout(
                         titlePanel("FCEV"),
                         checkboxInput("in_FCEV_include",
                                       "Include",
                                       F),
                         numericInput("in_FCEV_econ",
                                      "Avg Fuel Economy (mi/kg H2)",
                                      value = 62,
                                      step = 1),
                         numericInput("in_renew_energy_ratio",
                                      "Ratio Electrolysis Renewable Energy",
                                      value = .10,
                                      step = .1),
                         selectInput("in_preset_FCEV",
                                     "Choose Preset:",
                                     mix %>% filter(mix_type == "FCEV") %>% pull(mix_name),
                                     selected = "FCEV_Equal_4"),
                         actionButton("apply_in_preset_FCEV",
                                      "Apply Preset")
                     )
            )
        )
            ),
        # Show a plot of the generated distribution
        mainPanel(
            splitLayout(
               #titlePanel("Cost per kg CO2 Avoided (1 year)"),
               plotOutput("g1"),
               #titlePanel("Number of Vehicles Incentivized"),
               plotOutput("g2"),
               #titlePanel("Total CO2 Avoided (1 year)"),
               plotOutput("g3")
            )
        )
    )
)
# Define server logic required calculate and draw plots
server = function(input, output, session) {
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
        incentive_range = seq(0, 2500, by = 10) #Set the range of incentive dollar amounts
        
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
        total_budget_range = seq(1000000, 5000000, by = 100)
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
    #Plots
    #================================
    output$g1 <- renderPlot({
        ggplot(costperkg(), aes(incentive, costperkg, color=mode)) +
            xlim(min(costperkg()$incentive), max(costperkg()$incentive)) +
            ylim(0, 4) +
            geom_line(size = 1.5) + #plot mode lines
            geom_point(data = test_points(), aes(incentive, costperkg, color = mode)) + #plot specific test points
            geom_segment(data = test_points(), aes(x = 0, y = costperkg, xend = incentive, yend = costperkg, color = mode), linetype = "dashed", size = 1.5) +
            geom_segment(data = test_points(), aes(x = incentive, y = 0, xend = incentive, yend = costperkg, color = mode), linetype = "dashed", size = 1.5)
    })
    output$g2 <- renderPlot({
        ggplot(num_incentivized(), aes(budget, num, color = mode)) +
            geom_line(size = 1.5) + #plot mode lines
            geom_point(data = test_budget_points(), aes(budget, num, color = mode)) + #plot specific test points
            geom_segment(data = test_budget_points(), aes(x = min(num_incentivized()$budget), y = num, xend = budget, yend = num, color = mode), linetype = "dashed", size = 1.5) +
            geom_segment(data = test_budget_points(), aes(x = budget, y = 0, xend = budget, yend = num, color = mode), linetype = "dashed", size = 1.5)
    })
    output$g3 <- renderPlot({
        ggplot(CO2_avoided(), aes(budget, CO2_avoided, color = mode)) +
            geom_line(size = 1.5) +
            ylim(0, 1e7) +
            geom_segment(data = test_budget_points_w_CO2(), aes(x = min(num_incentivized()$budget), y = CO2_avoided, xend = budget, yend = CO2_avoided, color = mode), linetype = "dashed", size = 1.5) +
            geom_segment(data = test_budget_points_w_CO2(), aes(x = budget, y = 0, xend = budget, yend = CO2_avoided, color = mode), linetype = "dashed", size = 1.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
