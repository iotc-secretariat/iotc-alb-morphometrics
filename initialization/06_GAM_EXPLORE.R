library(maps)
library(mapdata)
library(lubridate)
library(mgcv)
library(mgcViz)
library(tidyverse)
library(randomForest)
library(maps)

ALB_FL_RD$logFL <- log(ALB_FL_RD$FL)
ALB_FL_RD$logRD <- log(ALB_FL_RD$RD)
ALB_FL_RD$month <- month(ALB_FL_RD$CAPTURE_DATE_START)

str(ALB_FL_RD)
table(ALB_FL_RD$FISHERY_GROUP)
table(ALB_FL_RD$FISHERY_GROUP, ALB_FL_RD$LAT_CENTROID, useNA = "always")
summary(ALB_FL_RD)
table(ALB_FL_RD$FLEET, ALB_FL_RD$FISHERY_GROUP)
table(ALB_FL_RD$FLEET, ALB_FL_RD$SAMPLING_PLATFORM)
table(ALB_FL_RD$FLEET, ALB_FL_RD$SAMPLING_STATUS)
table(ALB_FL_RD$SOURCE, ALB_FL_RD$SAMPLING_STATUS)
table(ALB_FL_RD$FLEET, ALB_FL_RD$SEX)
table(ALB_FL_RD$SAMPLING_PLATFORM, ALB_FL_RD$SAMPLING_STATUS)
table(ALB_FL_RD$PROJECT, ALB_FL_RD$SAMPLING_PLATFORM, ALB_FL_RD$SAMPLING_STATUS, useNA = "ifany")
table(ALB_FL_RD$PROJECT, ALB_FL_RD$FLEET, ALB_FL_RD$SAMPLING_PLATFORM, useNA = "ifany")

dim(ALB_FL_RD)
ALB_FL_RD[30442,]

latlev <- seq(-47.5, 2.5, 5)
lonlev <- seq(12.5, 117.5, 5)

windows(width=16, height=12); par(mfrow=c(2,2))
for(fg in unique(ALB_FL_RD$FISHERY_GROUP)) {
  dat <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP==fg,]
  aa <- (table(factor(dat$LON_CENTROID, levels=lonlev), factor(dat$LAT_CENTROID, levels=latlev)))
  a <- log(aa)
  a[is.infinite(a)] <- 0
  image(lonlev, latlev, (a), main = fg)
  contour(lonlev, latlev, (aa), levels = c(0,3,10,30,100,300,1000,3000,10000), main = fg, add=TRUE, labcex=.8)
  map(add=T, fill=TRUE)
}
savePlot("../outputs/charts/DESCRIPTION/sample_locations_by_fishery.png", type = "png")

windows(width=16, height=12); par(mfrow=c(3,2))
for(fg in unique(ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL",]$FLEET_CODE)) {
  dat <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL" & ALB_FL_RD$FLEET_CODE==fg,]
  aa <- (table(factor(dat$LON_CENTROID, levels=lonlev), factor(dat$LAT_CENTROID, levels=latlev)))
  a <- log(aa)
  a[is.infinite(a)] <- 0
  image(lonlev, latlev, (a), main = fg)
  contour(lonlev, latlev, (aa), levels = c(0,3,10,30,100,300,1000,3000,10000), main = fg, add=TRUE, labcex=.8)
  maps::map(add=T, fill=TRUE)
}
savePlot("../outputs/charts/DESCRIPTION/sample_locations_by_LL_fleet.png", type = "png")

windows(width=16, height=12); par(mfrow=c(2,2))
for(fg in unique(ALB_FL_RD$FISHERY_GROUP)) {
  dat <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP==fg,]
  plot(table(dat$YEAR), main = fg, xlim = range(ALB_FL_RD$YEAR), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_year.png", type = "png")

windows(width=16, height=12); par(mfrow=c(2,2))
for(fg in unique(ALB_FL_RD$FISHERY_GROUP)) {
  dat <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP==fg,]
  plot(table(dat$month), main = fg, xlim = range(ALB_FL_RD$month), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_month.png", type = "png")

windows(width=16, height=12); par(mfrow=c(2,2))
for(fg in unique(ALB_FL_RD$FISHERY_GROUP)) {
  dat <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP==fg,]
  hist(dat$FL, breaks = seq(10,150, 1), main = fg, xlim = c(50, 120), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_FL_by_fishery.png", type = "png")

windows(width=12, height=12); par(mfrow=c(3,2))
a <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL",]
for(fc in unique(a$FLEET_CODE)) {
  dat <- a[a$FLEET_CODE==fc,]
  hist(dat$FL, breaks = seq(10,150, 1), main = fc, xlim = c(50, 120), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_FL_by_fleet.png", type = "png")
# Problems with Japanese data - lots of 5s and 10s 

windows(width=12, height=12); par(mfrow=c(3,2))
a <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL" & ALB_FL_RD$FLEET_CODE == "JPN",]
table(a$SOURCE)
table(a$YEAR)
table(a$SEX)

windows(width=18, height=12); par(mfrow=c(3,3))
a <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL" & ALB_FL_RD$FLEET_CODE == "JPN",]
for(x in sort(unique(a$YEAR))) {
  dat <- a[a$YEAR==x,]
  hist(dat$FL, breaks = seq(10,150, 1), main = x, xlim = c(50, 120), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_FL_by_year_JPLL.png", type = "png")
# Rounding problems are worst in 2012, but occur to some extent in all years. 
# Rounding is probably associated with individual observers, so we need that level of data to resolve it. 

windows(width=18, height=12); par(mfrow=c(2,2))
a <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL" & ALB_FL_RD$FLEET_CODE == "JPN",]
for(x in sort(unique(a$SEX))) {
  dat <- a[a$SEX==x,]
  hist(dat$FL, breaks = seq(10,150, 1), main = x, xlim = c(50, 120), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_FL_by_sex_JPLL.png", type = "png")
# Rounding problems in all sexes. 

windows(width=16, height=12); par(mfrow=c(2,2))
for(fg in unique(ALB_FL_RD$FISHERY_GROUP)) {
  dat <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP==fg,]
  hist(dat$RD, breaks=seq(0, 100, 1), main = fg, xlim = c(0, 35), ylab = "# samples")
}
savePlot("../outputs/charts/DESCRIPTION/samples_by_RD_by_fishery.png", type = "png")

windows(width=16, height=12); 
par(mfrow=c(3,3), mar = c(4,2,3,1))
a <- ALB_FL_RD[ALB_FL_RD$FISHERY_GROUP_CODE=="LL" & ALB_FL_RD$FLEET_CODE == "JPN",]
for(ii in fives) {
  a3 <- a[a$FL == ii-2,]
  a4 <- a[a$FL == ii-1,]
  a5 <- a[a$FL == ii,]
  a6 <- a[a$FL == ii+1,]
  a7 <- a[a$FL == ii+2,]
  tmp3 <- table(round(a3$RD))
  tmp4 <- table(round(a4$RD))
  tmp5 <- table(round(a5$RD))
  tmp6 <- table(round(a6$RD))
  tmp7 <- table(round(a7$RD))
  plot(names(tmp5), tmp5/max(tmp5), xlim = c(5, 35), type = "b", ylim = c(0, 1), main = ii, xlab = "log10(RD)", ylab = "Relative numbers")
  points(names(tmp3), tmp3/max(tmp3), type = "b", col = 2, pch=2)
  points(names(tmp4), tmp4/max(tmp4), type = "b", col = 3, pch=3)
  points(names(tmp6), tmp6/max(tmp6), type = "b", col = 4, pch=4)
  points(names(tmp7), tmp7/max(tmp7), type = "b", col = 5, pch=5)
  m3=mean(a3$RD); s3=sd(a3$RD)
  m4=mean(a4$RD); s4=sd(a4$RD)
  m5=mean(a5$RD); s5=sd(a5$RD)
  m6=mean(a6$RD); s6=sd(a6$RD)
  m7=mean(a7$RD); s7=sd(a7$RD)
  if (ii==110) pos <- 9 else pos <- 32
  text(pos,1, paste("threes SD =", formatC(s3, digits=2)))
  text(pos,0.95, paste("fours SD =", formatC(s4, digits=2)))
  text(pos,0.9, paste("fives SD =", formatC(s5, digits=2)))
  text(pos,0.85, paste("sixes SD =", formatC(s6, digits=2)))
  text(pos,0.8, paste("sevens SD =", formatC(s7, digits=2)))

  text(pos,0.7, paste("threes mn =", formatC(m3, digits=3)))
  text(pos,0.65, paste("fours mn =", formatC(m4, digits=3)))
  text(pos,0.6, paste("fives mn =", formatC(m5, digits=3)))
  text(pos,0.55, paste("sixes mn =", formatC(m6, digits=3)))
  text(pos,0.5, paste("sevens mn =", formatC(m7, digits=3)))
}
legend("bottomleft", legend = c("threes","fours","fives", "sixes","sevens"), pch = c(2,3,1,4,5), col = c(2,3,1,4,5), lty=1)
savePlot("../outputs/charts/DESCRIPTION/check_for_rounding_bias.png", type = "png")

windows()
plot(jitter(a$logFL, amount=.001), jitter(a$log10RD, amount=0.01), pch=20, cex=0.3)
plot(jitter(a$FL, amount=.4), jitter(a$RD, amount=0.4), pch=20, cex=0.2, ylim = c(0, 35), xlim = c(50, 125))
table(round(a$logFL, 2))

dat <- filter(ALB_FL_RD, FLEET_CODE=="JPN" & FISHERY_GROUP_CODE=="LL" & logFL > 4.3 & logFL < 4.72)
fem <- dat[dat$YEAR >= 2012 & dat$SEX=="F" & dat$logFL < 4.53 & dat$LAT_CENTROID < -30 & dat$LAT_CENTROID > -40,]
mal <- dat[dat$YEAR >= 2012 & dat$SEX=="M" & dat$logFL < 4.7,]
unk <- dat[dat$YEAR >= 2012 & dat$SEX=="U" & dat$logFL < 4.5,]
malx5 <- mal[!mal$FL %in% fives,]
femx5 <- fem[!fem$FL %in% fives,]
table(malx5$FL)


table(dat$SEX, dat$YEAR)
table(round(mal$logFL, 2))
table(fem$LAT_CENTROID)
table(fem$LON_CENTROID)
table(mal$LAT_CENTROID, mal$LON_CENTROID)
table(unk$LAT_CENTROID, unk$LON_CENTROID)


# Random forest to identify important variables
dat <- filter(ALB_FL_RD, FISHERY_GROUP_CODE=="LL" & !is.na(LAT_CENTROID))
str(dat)
summary(dat)
mod0 <- gam(logRD ~ logFL, data = dat)
summary(mod0)
dat$resid <- resid(mod0)
for1 <- randomForest(resid ~ LON_CENTROID + LAT_CENTROID + YEAR + month + SEX + logFL + FLEET_CODE, data = dat, importance = TRUE)
print(for1)
round(importance(for1), 2)
windows()
varImpPlot(for1)
savePlot("../outputs/charts/DESCRIPTION/Importance_plot.png", type = "png")

library(rpart)
tree1 <- rpart(resid ~ LON_CENTROID + LAT_CENTROID + YEAR + month + SEX + logFL + FLEET_CODE, data = dat)
par(mfrow = c(1,1), xpd = NA) # otherwise on some devices the text is clipped
plot(tree1)
text(tree1, use.n = TRUE)


# GAM models
dat$rf<- factor(paste(dat$LAT_CENTROID, dat$LON_CENTROID, dat$YEAR, dat$MONTH, dat$FLEET_CODE))
dat$yrf <- factor(dat$YEAR, ordered = TRUE) # ordered factors behave differently in the GAM

dat$SEX <- as.factor(dat$SEX)
mod1 <- gam(logRD ~ s(logFL, k=30) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6)) + yrf + s(month) + s(rf, bs="re"), data = dat)
mod2 <- gam(logRD ~ s(logFL, k=30) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6)) + yrf + s(month) + SEX, data = dat)
mod3 <- gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6)) + yrf + s(month), data = dat)
mod4 <- gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6), by = SEX) + yrf + s(month), data = dat)

# with random effects
mod1re <- gam(logRD ~ s(logFL, k=30) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6)) + yrf + s(month) + s(rf, bs="re"), data = dat)
mod2re <- gam(logRD ~ s(logFL, k=30) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6)) + yrf + s(month) + SEX + s(rf, bs="re"), data = dat)
mod3re <- gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6)) + yrf + s(month) + s(rf, bs="re"), data = dat)
mod4re <- gam(logRD ~ SEX + s(logFL, by = SEX) + te(LON_CENTROID, LAT_CENTROID, k = c(6,6), by = SEX) + yrf + s(month) + s(rf, bs="re"), data = dat)

# single sex
modf1 <- gam(logRD ~ s(logFL, k=15) + s(month, k=6), data = fem)
modf1 <- gam(logRD ~ s(logFL, k=15) + s(month, k=6), data = fem)
modm1 <- gam(logRD ~ s(logFL, k=15) + s(month, k=6), data = mal)
modm1x5 <- gam(logRD ~ s(logFL, k=15) + s(month, k=6), data = malx5)
modf1x5 <- gam(logRD ~ s(logFL, k=15) + s(month, k=6), data = femx5)

AIC(mod1, mod2, mod3, mod4)
AIC(mod1re, mod2re, mod3re, mod4re)
AIC(modf1, modf2)

str(fem)
str(mal)
table(fem$LON_CENTROID, fem$LAT_CENTROID)
table(mal$LON_CENTROID, mal$LAT_CENTROID)
table(a$LON_CENTROID, a$LAT_CENTROID)

summary(mod1)
windows(); par(mfrow = c(2,2))
plot.gam(mod1, select = 1, rug=T, residuals = T)
plot.gam(mod1, select = 2, scheme=2, too.far=.07, main = ""); maps::map(add=T, fill=T)
plot.gam(mod1, all.terms = TRUE, select = 3, rug=T, residuals = T)
plot.gam(mod1, all.terms = TRUE, select = 4, rug=T, residuals = T)

summary(mod1)
windows(height = 12, width=10); par(mfrow = c(3,2))
plot.gam(mod1re, select = 1, rug=T, residuals = T)
plot.gam(mod1re, select = 2, scheme=2, too.far=.07, main = ""); maps::map(add=T, fill=T)
plot.gam(mod1re, all.terms = TRUE, select = 3, rug=T, residuals = T)
plot.gam(mod1re, all.terms = TRUE, select = 4, rug=T, residuals = T)
plot.gam(mod1re, all.terms = TRUE, select = 5, rug=T, residuals = T)

summary(mod1)
windows(height = 12, width=10); par(mfrow = c(3,2))
plot.gam(mod1re, pages = 1, all.terms = TRUE, scheme = 2)
plot.gam(mod2re, pages = 1, all.terms = TRUE, scheme = 2)
plot.gam(mod3re, pages = 1, all.terms = TRUE, scheme = 2)
plot.gam(mod4re, pages = 1, all.terms = TRUE, scheme = 2)

windows(height = 12, width=12); par(mfrow = c(4,3))
plot.gam(mod4re, all.terms = TRUE, select = 1, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 2, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 3, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 4, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 5, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4re, all.terms = TRUE, select = 6, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4re, all.terms = TRUE, select = 7, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4re, all.terms = TRUE, select = 8, scheme = 2, cex.main=0.9); maps::map(add=T, fill=T)
plot.gam(mod4re, all.terms = TRUE, select = 9, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 10, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 11, residuals = T)
plot.gam(mod4re, all.terms = TRUE, select = 12, residuals = T)
savePlot("../outputs/charts/DESCRIPTION/model_mod4re.png", type = "png")

summary(mod4r)
summary(mod2)
windows()
plot.gam(mod1, pages = 1, all.terms = TRUE, scheme = 2)
plot.gam(mod2, pages = 1, all.terms = TRUE, scheme = 2)
plot.gam(mod3, pages = 1, all.terms = TRUE, scheme = 2)
plot.gam(mod4, pages = 1, all.terms = TRUE, scheme = 2)
windows()
plot.gam(modf1, pages = 1, all.terms = TRUE, residuals = T)
windows()
plot.gam(modf1x5, pages = 1, all.terms = TRUE, residuals = T)
windows()
plot.gam(modm1, pages = 1, all.terms = TRUE, residuals = T)
windows()
plot.gam(modm1x5, pages = 1, all.terms = TRUE, residuals = T)

summary(mod4)
summary(mod1)
windows(); 
par(mfrow = c(3,4))
plot.gam(mod4, select = 1, rug=T, residuals = T)
plot.gam(mod4, select = 2, rug=T, residuals = T)
plot.gam(mod4, select = 3, rug=T, residuals = T)
plot.gam(mod4, select = 4, rug=T, residuals = T)
plot.gam(mod4, select = 5, scheme=2, too.far=.07); map(add=T, fill=T)
plot.gam(mod4, select = 6, scheme=2, too.far=.07); map(add=T, fill=T)
plot.gam(mod4, select = 7, scheme=2, too.far=.07); map(add=T, fill=T)
plot.gam(mod4, select = 8, scheme=2, too.far=.07); map(add=T, fill=T)
plot.gam(mod4, select = 9, rug=T, residuals = T)
plot.gam(mod4, select = 10, rug=T, residuals = T)

windows(); 
par(mfrow = c(2,2))
plot.gam(modf2, select = 1, rug=T, residuals = T)
plot.gam(modf2, select = 2, scheme=2, too.far=.1)


