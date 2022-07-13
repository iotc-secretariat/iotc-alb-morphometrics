print("Reading morphometric data...")

# Morphometric data set
ALB_FL_RD = fread("../inputs/data/ALB_FORK_LENGTH_ROUND_WEIGHT_DATASET_CWP55.csv")
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
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  flextable::fontsize(size = 9) %>%
  autofit()

print("Morphometric data read!")
