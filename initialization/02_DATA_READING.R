print("Read morphometric data...")

# Morphometric data set
ALB_FL_RD = fread("../inputs/data/ALB_FORK_LENGTH_ROUND_WEIGHT_DATASET_CWP55.csv")
ALB_FL_RD[, log10FL := log(FL, 10)]
ALB_FL_RD[, log10RD := log(RD, 10)]
ALB_FL_RD[, YEAR := year(CAPTURE_DATE_START)]

# Stock assessment areas
ALB_SA_AREAS = st_read("../inputs/shapes/alb_sa/ALB_SA_AREAS.shp")

print("Morphometric data read!")
