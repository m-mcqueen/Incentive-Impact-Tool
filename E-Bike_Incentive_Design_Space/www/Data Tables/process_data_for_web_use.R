#Process Data for Web Use
#Save data as native R format for more efficient server operations
#Mike McQueen

#Load data to save as .RDS
#State Info
Electricity_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/State Info/Electricity.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
Incentives_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/State Info/Incentives.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
Trips_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/State Info/Trips.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#Vehicles
BEV_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/BEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
PHEV_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/PHEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
FCEV_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/FCEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
ICE_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/ICE.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
EBike_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/EBike.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
mix_raw <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/mix.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) #Defines a specific combo of vehicle types
mix_info <- read.csv("E-Bike_Incentive_Design_Space/www/Data Tables/Vehicles/mix_info.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#export as .RDS and remove from environment
save(Electricity_raw, Incentives_raw, Trips_raw, BEV_raw, PHEV_raw, FCEV_raw, ICE_raw, EBike_raw, mix_raw, mix_info, file = "E-Bike_Incentive_Design_Space/www/Data Tables/data.RData")
rm(Electricity_raw, Incentives_raw, Trips_raw, BEV_raw, PHEV_raw, FCEV_raw, ICE_raw, EBike_raw, mix_raw, mix_info)