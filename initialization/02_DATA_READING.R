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

# Define fishery group colors (IOTC standard)
FG_COL = data.table(FISHERY_GROUP_CODE = c("LL", "PS", "LI", "BB"), 
                    FILL = c("#374BE5FF", "#FB4A6AFF", "#B3A2C7FF", "#FAC090FF"), 
                    #OUTLINE = c("#273AC0FF", "#D81C4AFF", "#8E7EA0FF", "#CC935FFF")
                    OUTLINE = c("#182FB5FF", "#C2013CFF", "#7F6B95FF", "#BF7A13FF")
                    )

# Define longline fleet colors (IOTC standards)

FLEET_COL = data.table(FLEET_CODE = c("EUFRA", "JPN", "TWN", "KOR"), 
                    FILL = c("#08519C", "#FF7F00", "#FDBB84", "#6A3D9A"), 
                    OUTLINE = c("#03498E", "#DC7534", "#DBA882", "#5D3B83")
)

  
print("Morphometric data read!")
