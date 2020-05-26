#E-Vehicle Incentive Impact Tool
#UI
#Mike McQueen

#Load Packages
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(units)
library(measurements)
library(stringr)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Electric Vehicle Incentive Cost and Impact"),
  
  #================================#
  #Sidebar####
  #================================#
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
                                          value = 1000,
                                          step = 50),
                             numericInput("in_test_budget",
                                          "Total budget ($)",
                                          value = 3000000,
                                          step = 10000),
                             splitLayout(cellWidths = c("75%", "25%"),
                               verticalLayout(
                                 sliderInput("in_BEV_per_budget",
                                            "BEV Budget Percentage Allotment",
                                            min = 0,
                                            max = 100,
                                            value = 0,
                                            step = 1),
                                 sliderInput("in_EBike_per_budget",
                                             "E-Bike Budget Percentage Allotment",
                                             min = 0,
                                             max = 100,
                                             value = 0,
                                             step = 1),
                                 sliderInput("in_PHEV_per_budget",
                                             "PHEV Budget Percentage Allotment",
                                             min = 0,
                                             max = 100,
                                             value = 0,
                                             step = 1),
                                 sliderInput("in_FCEV_per_budget",
                                             "FCEV Budget Percentage Allotment",
                                             min = 0,
                                             max = 100,
                                             value = 0,
                                             step = 1)
                               ),
                               plotOutput("g_budget_total")
                             )
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
                  ),
                  tabPanel("Report",
                           verticalLayout(
                             titlePanel("Report"),
                             downloadButton("report", "Generate report")
                           )
                           )
      )
    ),
    #================================#
    #Main Panel####
    #================================#
    # Show a plot of the generated distribution
    mainPanel(
      verticalLayout(
        verticalLayout(
          plotOutput("g1")
        ),
        splitLayout(
          verticalLayout(
            plotOutput("g2")
          ),
          verticalLayout(
            plotOutput("g3")
          )
        ),
        splitLayout(
          verticalLayout(
            plotOutput("g4")
          ),
          verticalLayout(
            plotOutput("g5")
          )
        )
      )
    )
  )
)
