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

#Process Data for Web Use
#Save data as native R format for more efficient server operations
#Mike McQueen

#Load data to save as .RDS
#State Info
Electricity_raw <- read.csv("www/Data Tables/State Info/Electricity.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
Incentives_raw <- read.csv("www/Data Tables/State Info/Incentives.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
Trips_raw <- read.csv("www/Data Tables/State Info/Trips.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#Vehicles
BEV_raw <- read.csv("www/Data Tables/Vehicles/BEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
PHEV_raw <- read.csv("www/Data Tables/Vehicles/PHEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
FCEV_raw <- read.csv("www/Data Tables/Vehicles/FCEV.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
ICE_raw <- read.csv("www/Data Tables/Vehicles/ICE.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
EBike_raw <- read.csv("www/Data Tables/Vehicles/EBike.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
mix_raw <- read.csv("www/Data Tables/Vehicles/mix.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) #Defines a specific combo of vehicle types
mix_info <- read.csv("www/Data Tables/Vehicles/mix_info.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#export as .RDS and remove from environment
save(Electricity_raw, Incentives_raw, Trips_raw, BEV_raw, PHEV_raw, FCEV_raw, ICE_raw, EBike_raw, mix_raw, mix_info, file = "www/Data Tables/data.RData")
rm(Electricity_raw, Incentives_raw, Trips_raw, BEV_raw, PHEV_raw, FCEV_raw, ICE_raw, EBike_raw, mix_raw, mix_info)