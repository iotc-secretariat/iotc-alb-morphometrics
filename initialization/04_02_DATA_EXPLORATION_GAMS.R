print("Initializing data exploration with GAMs...")

# Prepare data set for GAM analysis
ALB_FL_RD[, rf := factor(paste(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE))]
ALB_FL_RD[, yrf := factor(YEAR, ordered = TRUE)] # ordered factors behave differently in the GAM
ALB_FL_RD[, SEX := factor(SEX)]
#ALB_FL_RD[, MONTH := as.numeric(MONTH)]
ALB_FL_RD[, FISHERY_CODE := factor(FISHERY_CODE)]
ALB_FL_RD[, FLEET_CODE := factor(FLEET_CODE)]

# Subsample the data set with X fish by stratum when N>X
# set.seed(42)
# N_SUBSAMPLE = 10
# ALB_FL_RD_SUBSAMPLED = ALB_FL_RD[, .SD[sample(.N, min(N_SUBSAMPLE, .N))], by = .(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE)]

# GAM MODELS ####

## NO RANDOM EFFECT ####

GAM1 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD) # No sex effect

GAM2 = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX, data = ALB_FL_RD)  # Includes sex

GAM3 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH), data = ALB_FL_RD)  # Includes sex and interaction with size

GAM4 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH), data = ALB_FL_RD)  # Includes sex and interaction with size and long/lat

GAM5 = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH) + FISHERY_CODE + FLEET_CODE, data = ALB_FL_RD)  # Includes sex and interaction with size and long/lat

# Select best model based on AIC
AIC_TABLE_GAMS = as.data.table(AIC(GAM1, GAM2, GAM3, GAM4, GAM5), keep.rownames = TRUE)

AIC_TABLE_GAMS_FT = 
  flextable(AIC_TABLE_GAMS) %>% 
  set_header_labels(values = list(
    rn = "Model",
    df = "df",
    AIC = "AIC"
  )) %>%
  flextable::font(fontname = "calibri", part = c("all")) %>%  
  align(part = "header", align = "center") %>%
  border_inner() %>% 
  border_outer() %>%
  autofit()

# Model diagnostics
DIAGNOSTIC_GAM5 = appraise(GAM5)

ggsave("../outputs/charts/GAMS/DIAGNOSTIC_GAM5.png", DIAGNOSTIC_GAM5)

# Get predictions
ALB_FL_RD[, GAM_PREDICTIONS_FIT := predict.gam(GAM5, se.fit = TRUE)$fit]
ALB_FL_RD[, GAM_PREDICTIONS_UPPER := predict.gam(GAM5, se.fit = TRUE)$fit + 1.96 * predict.gam(GAM5, se.fit = TRUE)$se.fit]
ALB_FL_RD[, GAM_PREDICTIONS_LOWER := predict.gam(GAM5, se.fit = TRUE)$fit - 1.96 * predict.gam(GAM5, se.fit = TRUE)$se.fit]

# PLOT.GAM | MGCVIZ
GAM5VIZ = getViz(GAM5)

windows(); print(plot(GAM5VIZ, allTerms = T), pages = 1)
savePlot("../outputs/charts/GAMS/EFFECTS_GAM5.png", type = "png")

#print(plot(GAM5VIZ, allTerms = T, select = 1))
#savePlot("../outputs/charts/GAMS/EFFECTS_GAM5.png", type = "png")

## PLOT.GAM | GRATIA
# Extract the smooth terms
#sm_mod5 = gratia::smooth_estimates(mod5, dist = 0.1)
#sm_mod5_1 = sm_mod5[sm_mod5$smooth == "s(logFL):SEXI", ]

## FL|SEX
sm_mod5_FLSexF = draw(GAM5, residuals = FALSE)[[1]] & labs(x = "logFL", y = "Partial effect", title = "log10(Fork length)", subtitle = "By: Sex, Female")

sm_mod5_FLSexI = draw(GAM5, residuals = FALSE)[[2]] & labs(x = "logFL", y = "Partial effect", title = "log10(Fork length)", subtitle = "By: Sex, Indeterminate")

sm_mod5_FLSexM = draw(GAM5, residuals = FALSE)[[3]] & labs(x = "logFL", y = "Partial effect", title = "log10(Fork length)", subtitle = "By: Sex, Male")

sm_mod5_FLSexU = draw(GAM5, residuals = FALSE)[[4]] & labs(x = "logFL", y = "Partial effect", title = "log10(Fork length)", subtitle = "By: Sex, Unknown")

FL_SEX = (sm_mod5_FLSexF + sm_mod5_FLSexM) / (sm_mod5_FLSexI + sm_mod5_FLSexU)

ggsave("../outputs/charts/GAMS/MOD_FL_SEX_EFFECTS.png", FL_SEX, width = 10, height = 8)

# MONTH

sm_mod5_Month = draw(mod5, residuals = FALSE)[[5]] & labs(x = "logFL", y = "Partial effect", title = "Month", subtitle = "")

ggsave("../outputs/charts/GAMS/MOD_MONTH_EFFECT.png", sm_mod5_Month, width = 10, height = 8)

## LONG x LAT|SEX
sm_mod5_LongLatSexF = draw(mod5, residuals = FALSE)[[6]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Female")

sm_mod5_LongLatSexI = draw(mod5, residuals = FALSE)[[7]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Indeterminate")

sm_mod5_LongLatSexM = draw(mod5, residuals = FALSE)[[8]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Male")

sm_mod5_LongLatSexU = draw(mod5, residuals = FALSE)[[9]] & labs(x = "Longitude", y = "Latitude", title = "te(Longitude, Latitude)", subtitle = "By: Sex, Unknown")

LONG_LAT_SEX = (sm_mod5_LongLatSexF + sm_mod5_LongLatSexM) / (sm_mod5_LongLatSexI + sm_mod5_LongLatSexU)

ggsave("../outputs/charts/GAMS/MOD_LONG_LAT_SEX_EFFECTS.png", LONG_LAT_SEX, width = 10, height = 8)

## PLOT.GAM | OPTION 2

opar = par(no.readonly = TRUE)
par(mfrow=c(4, 3), mar = c(4,2.5,1,1), cex.axis = 1.1, cex.lab = 1.1, oma = c(0,2,0,0))

plot.gam(mod4, all.terms = TRUE, residuals = FALSE , xlab = "log(Fork length)", ylab = "", select = 1, shade = T, shade.col = alpha("red", 0.3), lwd=2, col = "red", xlim = c(4, 5), xaxs = 'i', ylim = c(-2, 2), rug = TRUE, las = 1, )
abline(v = seq(4, 5, 0.2), lty = 1, col = alpha("grey", 0.5), lwd = 0.3)
abline(h = seq(-2, 2, 1), lty = 1, col = alpha("grey", 0.5),lwd = 0.3)
#legend('topleft',legend='a',bty='n',cex=1.5)

# Y-label
#mtext(text = 'Selected effect on log(Round weight)', side = 2, line = 2.5, cex = 0.9)


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





# With random effects ####
mod1re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

mod2re = gam(logRD ~ s(logFL, k = 30) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + SEX + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

mod3re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6)) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

mod4re = gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6, 6), by = SEX) + yrf + s(MONTH) + s(rf, bs = "re"), data = ALB_FL_RD_SUBSAMPLED)

# Select best model based on AIC
AIC(mod1re, mod2re, mod3re, mod4re)









