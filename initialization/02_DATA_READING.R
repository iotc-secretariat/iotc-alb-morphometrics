print("Read morphometric data...")

# Morphometric data sets ####

### Historical IOTC observer data ####

IOTC_HISTORICAL_DATASET_TABLE_RAW    = fread("../inputs/data/ALB_IOTC_HISTORICAL_DATASET.csv")
IOTC_HISTORICAL_DATASET_TABLE_RAW[FLEET_CODE == "KOR", `:=` (HG = RD, RD = NA)]  # RD seems to be HG
IOTC_HISTORICAL_DATASET_TABLE_RAW[, SOURCE := "IOTC"]
IOTC_HISTORICAL_DATASET_TABLE_RAW[AREA == "BIOT", AREA := "Chagos archipelago"]

IOTC_HISTORICAL_DATASET_TABLE = IOTC_HISTORICAL_DATASET_TABLE_RAW[, .(SOURCE = "IOTC-HISTORICAL", FISH_IDENTIFIER = paste("IOTC", seq(1, nrow(IOTC_HISTORICAL_DATASET_TABLE_RAW), 1), sep = "_"), CAPTURE_DATE_START = as.Date(DATESTART, format = "%d/%m/%Y"), CAPTURE_DATE_END = as.Date(DATEEND, format = "%d/%m/%Y"), SAMPLING_LOCATION = "Onboard", OCEAN = "Indian Ocean", FLEET_CODE, GEAR_CODE, FISHERY_GROUP_CODE = "LL", LONG = as.numeric(NA), LAT = as.numeric(NA), GRID = AREA, SPECIES_CODE, STATUS = "Fresh", SEX, FL, RD)]

IOTC_HISTORICAL_DATASET_TABLE[GRID == "Chagos archipelago", `:=` (LONG = 71.94157, LAT = -6.584289)]
IOTC_HISTORICAL_DATASET_TABLE[GRID != "Chagos archipelago", LONG := center_from_grid_code(GRID)[1], by = .(FISH_IDENTIFIER)]
IOTC_HISTORICAL_DATASET_TABLE[GRID != "Chagos archipelago", LAT := center_from_grid_code(GRID)[2], by = .(FISH_IDENTIFIER)]

### Ifremer ####
# No information on the origin of capture (use of sampling date for the capture date)

IFREMER_DATASET_TABLE_RAW = data.table(read.xlsx("../inputs/data/ALB_IFREMER_DATASET.xlsx"))[SPECIES_CODE == "ALB"]
IFREMER_DATASET_TABLE_RAW[!is.na(RD_SCIENTIFIC), RD := RD_SCIENTIFIC]
IFREMER_DATASET_TABLE_RAW[is.na(RD_SCIENTIFIC) & !is.na(RD_CANNERY), RD := RD_CANNERY]
IFREMER_DATASET_TABLE_RAW[, SAMPLING_DATE := convertToDate(SAMPLING_DATE)]

IFREMER_DATASET_TABLE = IFREMER_DATASET_TABLE_RAW[, .(SOURCE = "Ifremer", FISH_IDENTIFIER = paste("Ifremer", FISH_IDENTIFIER, sep = "_"), CAPTURE_DATE_START = SAMPLING_DATE, CAPTURE_DATE_END = SAMPLING_DATE, SAMPLING_LOCATION = "Factory", OCEAN = "Indian Ocean", FLEET_CODE = "EUREU", GEAR_CODE = "ELL" , FISHERY_GROUP_CODE = "LL", LONG = NA, LAT = NA, GRID = NA, SPECIES_CODE, STATUS = "Fresh", SEX, FL, RD)]

### IRD/University of Mauritius ###
IRD_DATASET_TABLE_RAW = data.table(read.xlsx("../inputs/data/ALB_MORPHOMETRIC_DATASET_IRD_UOM.csv"))
IRD_DATASET_TABLE_RAW[, fishing_date := convertToDate(fishing_date)]
IRD_DATASET_TABLE_RAW[, fishing_date_min := convertToDate(fishing_date_min)]
IRD_DATASET_TABLE_RAW[, fishing_date_max := convertToDate(fishing_date_max)]

IRD_DATASET_TABLE_RAW[, fish_sampling_date := convertToDate(fish_sampling_date)]
IRD_DATASET_TABLE_RAW[!is.na(fishing_date), `:=` (fishing_date_min = fishing_date, fishing_date_max = fishing_date)]

IRD_DATASET_TABLE = IRD_DATASET_TABLE_RAW[, .(SOURCE = "MUS University-IRD", FISH_IDENTIFIER = fish_identifier, CAPTURE_DATE_START = fishing_date_min, CAPTURE_DATE_END = fishing_date_max, SAMPLING_LOCATION = "Lab", OCEAN = "Indian Ocean", FLEET_CODE = NA, GEAR_CODE = fishing_gear, FISHERY_GROUP_CODE = fifelse(fishing_gear == "PS", "PS", fifelse(fishing_gear == "BB", "PL", "LL")), LONG = cal_longitude, LAT = cal_latitude, GRID = fifelse(!is.na(cal_longitude), convert_to_CWP_grid(lon = longitude_deg_dec, lat = cal_latitude), as.character(NA)),  SPECIES_CODE = "ALB", STATUS = "Fresh", SEX = sex, FL = fork_length, RD = whole_fishweight)]

### Regional Observer Scheme ####

ROS_DATASET_TABLE_RAW = data.table(read.xlsx("../inputs/data/ALB_IOTC_ROS_DATASET.xlsx"))

ROS_DATASET_TABLE = ROS_DATASET_TABLE_RAW[, .(SOURCE = "IOTC ROS", FISH_IDENTIFIER = paste("IOTC-ROS", seq(1, nrow(ROS_DATASET_TABLE_RAW), 1), sep = "_"), CAPTURE_DATE_START = as.Date(paste(YEAR, MONTH, 15, sep = "-")), CAPTURE_DATE_END = as.Date(paste(YEAR, MONTH, 15, sep = "-")), SAMPLING_LOCATION = "Onboard", OCEAN = "Indian Ocean", FLEET_CODE = FLAG, GEAR_CODE = "LL", FISHERY_GROUP_CODE = "LL", LONG = as.numeric(NA), LAT = as.numeric(NA), GRID, SPECIES_CODE = SPECIES, STATUS = "Fresh", SEX, FL, RD)]

ROS_DATASET_TABLE[, LONG := center_from_grid_code(GRID)[1], by = .(FISH_IDENTIFIER)] 
ROS_DATASET_TABLE[, LAT := center_from_grid_code(GRID)[2], by = .(FISH_IDENTIFIER)] 

# Merge the data sets
MORPHOMETRICS_TABLE = rbindlist(list(IOTC_HISTORICAL_DATASET_TABLE, IFREMER_DATASET_TABLE, IRD_DATASET_TABLE, ROS_DATASET_TABLE))[!is.na(FL) & !is.na(RD)]

# Fishery groups labels
MORPHOMETRICS_TABLE[FISHERY_GROUP_CODE == "LL", FISHERY_GROUP := "Longline"]
MORPHOMETRICS_TABLE[FISHERY_GROUP_CODE == "PL", FISHERY_GROUP := "Pole and line"]
MORPHOMETRICS_TABLE[FISHERY_GROUP_CODE == "PS", FISHERY_GROUP := "Purse seine"]
MORPHOMETRICS_TABLE[, FISHERY_GROUP_CODE := factor(FISHERY_GROUP_CODE, levels = c("LL", "PS", "PL"))]

# Sex harmonization
MORPHOMETRICS_TABLE[is.na(SEX), SEX := "U"]
MORPHOMETRICS_TABLE[SEX == "NULL", SEX := "U"]

# Remove a few obvious outliers
MORPHOMETRICS_TABLE = MORPHOMETRICS_TABLE[FL>50]
MORPHOMETRICS_TABLE = MORPHOMETRICS_TABLE[FISH_IDENTIFIER != "IOTC-ROS_7085"] 
MORPHOMETRICS_TABLE = MORPHOMETRICS_TABLE[FISH_IDENTIFIER != "IOTC-ROS_10591"]
MORPHOMETRICS_TABLE = MORPHOMETRICS_TABLE[FISH_IDENTIFIER != "IOTC-ROS_547"]
MORPHOMETRICS_TABLE = MORPHOMETRICS_TABLE[FISH_IDENTIFIER != "IOTC-ROS_5479"]
MORPHOMETRICS_TABLE = MORPHOMETRICS_TABLE[FISH_IDENTIFIER != "IOTC-ROS_10489"]

print("Morphometric data read!")
