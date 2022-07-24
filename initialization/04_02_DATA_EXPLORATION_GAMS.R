print("Initializing data exploration with GAMs...")

ALB_FL_RD[, rf := factor(paste(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE))]
ALB_FL_RD[, yrf := factor(YEAR, ordered = TRUE)] # ordered factors behave differently in the GAM
ALB_FL_RD[, SEX := factor(SEX)]
ALB_FL_RD[, FISHERY_CODE := factor(FISHERY_CODE)]

# Subsample the data set with X fish by stratum when N>X
set.seed(42)

N_SUBSAMPLE = 10

ALB_FL_RD_SUBSAMPLED = ALB_FL_RD[, .SD[sample(.N, min(N_SUBSAMPLE, .N))], by = .(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE)]

# GAM MODELS ####

## NO RANDOM EFFECT ####

mod0 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + FISHERY_CODE + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED) # Fishery effect

mod1 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED) # No sex effect

mod2 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX, data = ALB_FL_RD_SUBSAMPLED)  # Includes sex

mod3 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED)  # Includes sex and interaction with size

mod4 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH), data = ALB_FL_RD_SUBSAMPLED)  # Includes sex and interaction with size and long/lat

# Select best model based on AIC
AIC(mod0, mod1, mod2, mod3, mod4)

# Model diagnostics
appraise(mod4)

# Visualize "best model" outputs
MOD4 = getViz(mod4)

windows(); print(plot(MOD4, allTerms = T), pages = 1)
savePlot("../outputs/charts/GAMS/EFFECT_logFLFemales.png", type = "png")

print(plot(MOD4, allTerms = T, select = 1))

## PLOT.GAM OPTION 1

opar = par(no.readonly = TRUE)
par(mfrow=c(4, 3), mar = c(4,2.5,1,1), cex.axis = 1.1, cex.lab = 1.1, oma = c(0,2,0,0))

plot.gam(mod4, all.terms = TRUE, residuals = FALSE , xlab = "log(Fork length)", ylab = "", select = 1, shade = T, shade.col = alpha("red", 0.3), lwd=2, col = "red", xlim = c(4, 5), xaxs = 'i', ylim = c(-2, 2), rug = TRUE, las = 1, )
abline(v = seq(4, 5, 0.2), lty = 1, col = alpha("grey", 0.5), lwd = 0.3)
abline(h = seq(-2, 2, 1), lty = 1, col = alpha("grey", 0.5),lwd = 0.3)
#legend('topleft',legend='a',bty='n',cex=1.5)

# Y-label
#mtext(text = 'Selected effect on log(Round weight)', side = 2, line = 2.5, cex = 0.9)


## PLOT.GAM | OPTION 2
# Extract the smooth terms
sm_mod4 = gratia::smooth_estimates(mod4, dist = 0.1)
sm_mod4_1 = sm_mod4[sm_mod4$smooth == "s(logFL):SEXI", ]

sm_mod4_LongLatSexF = draw(mod4, residuals = FALSE)[[6]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Female")

sm_mod4_LongLatSexI = draw(mod4, residuals = FALSE)[[7]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Immature")

sm_mod4_LongLatSexM = draw(mod4, residuals = FALSE)[[8]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Male")

sm_mod4_LongLatSexU = draw(mod4, residuals = FALSE)[[9]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Unknown")

popo = (sm_mod4_LongLatSexM + sm_mod4_LongLatSexF) / (sm_mod4_LongLatSexI + sm_mod4_LongLatSexU)

ggsave("../outputs/charts/GAMS/test1.png", popo, width = 10, height = 8)

# plt <- plot.gam(mod4)[[5]]
# plt <- plt[[5]] # plot.gam returns a list of n elements, one per plot
# 
# sm <- smooth_estimates(m, smooth = "te(x,y)", dist = 0.1)
# ggplot(sm, aes(x = x, y = y)) +
#   geom_raster(aes(fill = est)) +
#   geom_point(data = df, alpha = 0.2) + # add a point layer for original data
#   scale_fill_viridis_c(option = "plasma")
# 

logFLFemales = 
  plot(sm(MOD4, 1)) +
  l_fitLine(alpha = 0.6, color = "red", size = 1.2) + 
  l_rug(alpha = 0.8) +
  l_ciPoly(level = 0.95, colour = NA, fill = alpha("red", 0.4)) +
  labs(y = "Selected effect on log(Round weight)", title = "Females") +
  theme(panel.grid.major = element_line(), panel.grid.minor = element_line(linetype = 2)) +
  scale_y_continuous(breaks = seq(-2, 2, 1), minor_breaks = seq(-2, 2, 0.5))

logFLMales = 
  plot(sm(MOD4, 3)) +
  l_fitLine(alpha = 0.6, color = "blue", size = 1.2) + 
  l_rug(alpha = 0.8) +
  l_ciPoly(level = 0.95, colour = NA, fill = alpha("blue", 0.4)) +
  labs(y = "Selected effect on log(Round weight)", title = "Males") + 
  theme(panel.grid.major = element_line(), panel.grid.minor = element_line(linetype = 2)) +
  scale_y_continuous(breaks = seq(-2, 2, 1), minor_breaks = seq(-2, 2, 0.5))

savePlot("../outputs/charts/GAMS/EFFECT_logFLFemales.png", type = "png")


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
mod1re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

mod2re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

mod3re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

mod4re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

# Select best model based on AIC
AIC(mod1re, mod2re, mod3re, mod4re)









