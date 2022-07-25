print("Reading morphometric data...")

# Morphometric data set
unzip("../inputs/data/ALB_FORK_LENGTH_ROUND_WEIGHT_DATASET_CWP55.zip", exdir = "../inputs/data/")

ALB_FL_RD_RAW = fread("../inputs/data/ALB_FORK_LENGTH_ROUND_WEIGHT_DATASET_CWP55.csv")

# Filter fish > 130 cm
#ALB_FL_RD = ALB_FL_RD_RAW[FL<= quantile(ALB_FL_RD_RAW$FL, 0.9999)]
ALB_FL_RD = ALB_FL_RD_RAW[FL<= 130]

ALB_FL_RD[, log10FL := log(FL, 10)]
ALB_FL_RD[, log10RD := log(RD, 10)]
ALB_FL_RD[, logFL   := log(ALB_FL_RD$FL)]
ALB_FL_RD[, logRD   := log(ALB_FL_RD$RD)]
ALB_FL_RD[, YEAR := year(CAPTURE_DATE_START)]
ALB_FL_RD[, MONTH := month(CAPTURE_DATE_START)]

# Factorize fishery groups
ALB_FL_RD[, FISHERY_GROUP_CODE := factor(FISHERY_GROUP_CODE, levels = c("LL", "PS", "LI", "BB"))]
ALB_FL_RD[, FISHERY_GROUP := factor(FISHERY_GROUP, levels = c("Longline", "Purse seine", "Line", "Baitboat"))]

# Stock assessment areas
ALB_SA_AREAS = st_read("../inputs/shapes/alb_sa/ALB_SA_AREAS.shp")

# FIELDS DEFINITIONS

FIELD_DEFINITIONS = data.table(read.xlsx("../inputs/data/Metadata_Table.xlsx", sheet = "metadata"))[, .(Field, Definition)]

FIELD_DEFINITIONS_FT =
  FIELD_DEFINITIONS %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("all")) %>%
  flextable::fontsize(size = 9) %>%
  border_inner() %>%
  border_outer(border = fp_border(width = 2)) %>%
  autofit()

print("Morphometric data read!")
