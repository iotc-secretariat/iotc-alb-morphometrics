print("Initializing FL-RD model...")

# Prepare data set
ALB_FL_RD[, YR_QTR := as.factor(paste(YEAR, sprintf("%02d", CAPTURE_QUARTER), sep = "-"))]
ALB_FL_RD[, CAPTURE_QUARTER := as.factor(CAPTURE_QUARTER)]
ALB_FL_RD[, FISHERY_GROUP_CODE := as.factor(FISHERY_GROUP_CODE)]
ALB_FL_RD[, SA_AREA_CODE := as.factor(SA_AREA_CODE)]
ALB_FL_RD[, FISHING_GROUND_CODE := as.factor(FISHING_GROUND_CODE)]
ALB_FL_RD[, SEX := as.factor(SEX)]

# Model with fork length and several covariates
LM_ALB_FL_RD = lm(log10RD ~ log10FL + SA_AREA_CODE + CAPTURE_QUARTER + FISHERY_GROUP_CODE + SEX, data = ALB_FL_RD)

stepAIC(LM_ALB_FL_RD)
anova(LM_ALB_FL_RD)
summary(LM_ALB_FL_RD)

# Model with fork length and area only

LM_ALB_FL_RD = lm(log10RD ~ log10FL + SA_AREA_CODE, data = ALB_FL_RD)

BIAS_CORRECTION_FACTOR = exp(var(residuals(LM_ALB_FL_RD))*2.651)  # Smith 1993
a_FL_RD_IRALB01 = 10^coef(LM_ALB_FL_RD)[1] * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB02 = 10^(coef(LM_ALB_FL_RD)[1] + coef(LM_ALB_FL_RD)[3]) * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB03 = 10^(coef(LM_ALB_FL_RD)[1] + coef(LM_ALB_FL_RD)[4]) * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB04 = 10^(coef(LM_ALB_FL_RD)[1] + coef(LM_ALB_FL_RD)[5]) * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB05 = 10^(coef(LM_ALB_FL_RD)[1] + coef(LM_ALB_FL_RD)[6]) * BIAS_CORRECTION_FACTOR
b_FL_RD         = coef(LM_ALB_FL_RD)[2]

print("FL-RD model initialized!")