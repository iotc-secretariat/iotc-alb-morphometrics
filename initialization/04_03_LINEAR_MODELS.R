print("Initializing FL-RD model...")

# Prepare data set for linear model analysis
#ALB_FL_RD[, rf := factor(paste(LAT_CENTROID, LON_CENTROID, YEAR, MONTH, FLEET_CODE))]
ALB_FL_RD[, YEAR := factor(YEAR, ordered = TRUE)]
ALB_FL_RD[, SEX := factor(SEX)]
ALB_FL_RD[, MONTH := factor(MONTH)]
ALB_FL_RD[, FISHERY_CODE := factor(FISHERY_CODE)]
ALB_FL_RD[, FLEET_CODE := factor(FLEET_CODE)]
ALB_FL_RD[, YR_QTR := as.factor(paste(YEAR, sprintf("%02d", CAPTURE_QUARTER), sep = "-"))]
ALB_FL_RD[, CAPTURE_QUARTER := as.factor(CAPTURE_QUARTER)]
ALB_FL_RD[, SA_AREA_CODE := as.factor(SA_AREA_CODE)]

# Model with all covariates ####

## Full model
LM_ALB_FL_RD_FULL = lm(log10RD ~ log10FL + SA_AREA_CODE + log10FL:SA_AREA_CODE + SEX + YEAR + FISHERY_CODE + MONTH + log10FL:SEX + log10FL:SEX + FLEET_CODE, data = ALB_FL_RD)

stepAIC(LM_ALB_FL_RD_FULL)

# Final model
LM_ALB_FL_RD_ALL_FINAL = lm(log10RD ~ log10FL + SA_AREA_CODE + SEX + YEAR + MONTH + FLEET_CODE + log10FL:SA_AREA_CODE + log10FL:SEX, data = ALB_FL_RD)

anova(LM_ALB_FL_RD_ALL_FINAL)
ANOVA_TABLE_LM_ALB_FL_RD_ALL_FINAL =  anova_table(LM_ALB_FL_RD_ALL_FINAL)
  
summary(LM_ALB_FL_RD_ALL_FINAL)

ANOVA_TABLE_LM_ALB_FL_RD_FINAL_FT = 
  ANOVA_TABLE_LM_ALB_FL_RD_ALL_FINAL %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("all")) %>%
  align(part = "header", align = "center") %>%
  border_inner() %>%
  border_outer(border = fp_border(width = 2)) %>%
  autofit()

# Model with fork length only ####
LM_ALB_FL_RD = lm(log10RD ~ log10FL, data = ALB_FL_RD)

BIAS_CORRECTION_FACTOR = exp(var(residuals(LM_ALB_FL_RD))*2.651)  # Smith 1993
a_FL_RD = 10^coef(LM_ALB_FL_RD)[1] * BIAS_CORRECTION_FACTOR
b_FL_RD         = coef(LM_ALB_FL_RD)[2]

### Model predictions ####
PREDICTIONS_LM = data.table(FL = seq(50, 130, 0.1))
PREDICTIONS_LM[, log10FL := log(FL, 10)]
PREDICTIONS_LM[, OVERALL := round((10^predict.lm(LM_ALB_FL_RD, newdata = PREDICTIONS_LM)) * BIAS_CORRECTION_FACTOR, 2)]

### Comparison with current relationship ####
a_Penney = 0.0000137180
b_Penney = 3.0973

PREDICTIONS_LM[, PENNEY := round(a_Penney * FL^b_Penney, 2)][, log10FL := NULL]
PREDICTIONS_LM_MELTED = melt.data.table(PREDICTIONS_LM, id.vars = "FL", variable.name = "SOURCE", value.name = "RD")

ALB_FL_RD_CURVES =
ggplot(PREDICTIONS_LM_MELTED, aes(x = FL, y = RD, color = SOURCE)) +
  geom_line() +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/FITS/ALB_FL_RD_CURVES.png", ALB_FL_RD_CURVES, width = 8, height = 4.5)

# Model with fork length and area ####
LM_ALB_FL_RD_AREA = lm(log10RD ~ log10FL + SA_AREA_CODE, data = ALB_FL_RD)

anova(LM_ALB_FL_RD_AREA)
summary(LM_ALB_FL_RD_AREA)

BIAS_CORRECTION_FACTOR = exp(var(residuals(LM_ALB_FL_RD_AREA))*2.651)  # Smith 1993
a_FL_RD_IRALB01 = 10^coef(LM_ALB_FL_RD_AREA)[1] * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB02 = 10^(coef(LM_ALB_FL_RD_AREA)[1] + coef(LM_ALB_FL_RD_AREA)[3]) * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB03 = 10^(coef(LM_ALB_FL_RD_AREA)[1] + coef(LM_ALB_FL_RD_AREA)[4]) * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB04 = 10^(coef(LM_ALB_FL_RD_AREA)[1] + coef(LM_ALB_FL_RD_AREA)[5]) * BIAS_CORRECTION_FACTOR
a_FL_RD_IRALB05 = 10^(coef(LM_ALB_FL_RD_AREA)[1] + coef(LM_ALB_FL_RD_AREA)[6]) * BIAS_CORRECTION_FACTOR
b_FL_RD         = coef(LM_ALB_FL_RD_AREA)[2]

### Model predictions ####
PREDICTIONS_LM_AREA = data.table(expand.grid(FL = seq(50, 130, 0.1), SA_AREA_CODE = c("IRALB01", "IRALB02", "IRALB03", "IRALB04", "IRALB05")))
PREDICTIONS_LM_AREA[, log10FL := log(FL, 10)]
PREDICTIONS_LM_AREA[, RD := round((10^predict.lm(LM_ALB_FL_RD_AREA, newdata = PREDICTIONS_LM_AREA)) * BIAS_CORRECTION_FACTOR, 2)]

ALB_FL_RD_AREA_FACETED_FIT =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, col = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), shape = 21, size = .8) +
  theme_bw() +
  scale_color_manual(values = FG_COL$FILL) + 
  geom_line(data = PREDICTIONS_LM_AREA, aes(x = FL, y = RD), size = .4, col = "black") +
#  geom_line(data = PREDICTIONS_LM, aes(x = FL, y = PENNEY), size = .4, col = "red") +
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(limits = c(0, 45)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/FITS/ALB_FL_RD_AREA_FIT.png", ALB_FL_RD_AREA_FACETED_FIT, width = 10, height = 6)

ALB_FL_RD_AREA_CURVES =
  ggplot(data = PREDICTIONS_LM_AREA, aes(x = FL, y = RD, color = SA_AREA_CODE)) +
  geom_line() +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/FITS/ALB_FL_RD_AREA_CURVES.png", ALB_FL_RD_AREA_CURVES, width = 8, height = 4.5)

# Matrix of weights for between areas for comparison purpose
PREDICTION_TABLE_WEIGHT_AREA = dcast.data.table(PREDICTIONS_LM_AREA[, -c("log10FL")], FL ~ SA_AREA_CODE, value.var = "RD")[FL %in% seq(50, 130, 10)]

PREDICTION_TABLES_WEIGHT = merge(PREDICTIONS_LM, PREDICTION_TABLE_WEIGHT_AREA, by = "FL")


print("FL-RD model initialized!")