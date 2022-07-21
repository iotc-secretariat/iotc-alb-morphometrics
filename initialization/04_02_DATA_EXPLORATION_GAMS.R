
print("Initializing data exploration with GAMs...")

# GAM MODELS ####
ALB_FL_RD[, rf := factor(paste(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE))]
ALB_FL_RD[, yrf := factor(YEAR, ordered = TRUE)] # ordered factors behave differently in the GAM
ALB_FL_RD[, SEX := factor(SEX)]

mod1 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod2 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX, data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod3 <- gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod4 <- gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH), data = ALB_FL_RD[FLEET_CODE != "TWN"])

# with random effects
mod1re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod2re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod3re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod4re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

