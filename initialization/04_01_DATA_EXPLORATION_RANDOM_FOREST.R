print("Inializing data exploration with random forests...")

dat = ALB_FL_RD[!is.na(SEX) & FLEET_CODE != "TWN"]

mod0 = gam(logRD ~ logFL, data = dat)
summary(mod0)

dat[, resid := resid(mod0)]

for1 = randomForest(resid ~ LON_CENTROID + LAT_CENTROID + YEAR + MONTH + SEX + logFL + FLEET_CODE, data = dat, importance = TRUE)

print(for1)

round(importance(for1), 2)

varImpPlot(for1)

print("Data exploration with random forests initialized!")