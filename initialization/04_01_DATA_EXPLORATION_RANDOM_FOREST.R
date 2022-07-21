print("Inializing data exploration with random forests...")

# Filter the data set to reduce size
dat = ALB_FL_RD[!is.na(SEX) & FLEET_CODE != "TWN"]

# Simple GAM model to remove length effect
mod0 = gam(logRD ~ logFL, data = dat)
summary(mod0)

# Focus on residuals
dat[, resid := resid(mod0)]

# RANDOM FOREST MODEL ####
for1 = randomForest(resid ~ LON_CENTROID + LAT_CENTROID + YEAR + MONTH + SEX + logFL + FLEET_CODE, data = dat, importance = TRUE)

for1

round(importance(for1), 2)

varImpPlot(for1)

# REGRESSION TREE MODEL ####
tree1 = rpart(resid ~ LON_CENTROID + LAT_CENTROID + YEAR + MONTH + SEX + logFL + FLEET_CODE, data = dat)

par(mfrow = c(1, 1), xpd = NA) # otherwise on some devices the text is clipped
plot(tree1)
text(tree1, use.n = TRUE)

print("Data exploration with random forests initialized!")