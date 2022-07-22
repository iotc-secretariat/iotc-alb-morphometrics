print("Initializing data exploration with GAMs...")

ALB_FL_RD[, rf := factor(paste(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE))]
ALB_FL_RD[, yrf := factor(YEAR, ordered = TRUE)] # ordered factors behave differently in the GAM
ALB_FL_RD[, SEX := factor(SEX)]

# Subsample the data set with X fish by stratum when N>X
set.seed(42)

N_SUBSAMPLE = 10

ALB_FL_RD_SUBSAMPLED = ALB_FL_RD[, .SD[sample(.N, min(N_SUBSAMPLE, .N))], by = .(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE)]

# GAM MODELS ####

## NO RANDOM EFFECT ####

mod1 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED) # No sex effect

mod2 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX, data = ALB_FL_RD_SUBSAMPLED)  # Includes sex

mod3 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED)  # Includes sex and interaction with size

mod4 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED)  # Includes sex and interaction with size and long/lat

# Select best model based on AIC
AIC(mod1, mod2, mod3, mod4)

# Visualize "best model" outputs
MOD4 = getViz(mod4)

print(plot(MOD4, allTerms = T), pages = 1)
print(plot(MOD4, allTerms = T, select = 1), pages = 1)

par(mfcol = c(2, 2))

foo = 
  plot(sm(MOD4, 1)) +
  l_fitLine(alpha = 0.6, color = "red", size = 1.2) + 
  l_rug(alpha = 0.8) +
#  l_ciLine(mul = 5, colour = "blue", linetype = 2) +
  labs(y = "s(logFL)", title = "Females")

plot(sm(MOD2, 2)) + 
  l_fitRaster() + 
  l_fitContour() + 
  l_points(shape = 19, size = 3, alpha = 0.1)

plot.gam(mod4, select = 1, rug=T, residuals = T)
plot.gam(mod4, select = 2, scheme=2, too.far=.07, main = ""); maps::map(add=T, fill=T)
plot.gam(mod4, all.terms = TRUE, select = 3, rug=T, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 4, rug=T, residuals = T)

par(mfcol = c(4, 3))
plot.gam(mod4, all.terms = TRUE, select = 1, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 2, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 3, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 4, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 5, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4, all.terms = TRUE, select = 6, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4, all.terms = TRUE, select = 7, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4, all.terms = TRUE, select = 8, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4, all.terms = TRUE, select = 9, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 10, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 11, residuals = T)
plot.gam(mod4, all.terms = TRUE, select = 12, residuals = T)

print(plot(MOD4, allTerms = T), pages = 1)




# With random effects ####
mod1re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod2re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod3re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

mod4re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD[FLEET_CODE != "TWN"])

# Select best model based on AIC
AIC(mod1re, mod2re, mod3re, mod4re)









