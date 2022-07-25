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
LM_ALB_FL_RD_ALL = lm(log10RD ~ log10FL + SA_AREA_CODE + log10FL:SA_AREA_CODE + SEX + YEAR + FISHERY_CODE + MONTH + log10FL:SEX + log10FL:SEX + FISHERY_CODE + FLEET_CODE, data = ALB_FL_RD)

stepAIC(LM_ALB_FL_RD_ALL)
anova(LM_ALB_FL_RD_ALL)
summary(LM_ALB_FL_RD_ALL)

ANOVA_TABLE_LM_ALB_FL_RD_ALL_FT = 
  anova_table(LM_ALB_FL_RD_ALL) %>%
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

# Model predictions
PREDICTION_DT = data.table(expand.grid(FL = seq(50, 130, 0.1), SA_AREA_CODE = c("IRALB01", "IRALB02", "IRALB03", "IRALB04", "IRALB05")))
PREDICTION_DT[, log10FL := log(FL, 10)]
PREDICTION_DT[, RD := (10^predict.lm(LM_ALB_FL_RD_AREA, newdata = PREDICTION_DT)) * BIAS_CORRECTION_FACTOR]

ALB_FL_RD_AREA_FACETED_FIT =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, col = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), shape = 21, size = .8) +
  theme_bw() +
  scale_color_manual(values = FG_COL$FILL) + 
  geom_line(data = PREDICTION_DT, aes(x = FL, y = RD), size = .4, col = "black") +
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(limits = c(0, 45)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/FITS/ALB_FL_RD_AREA_FIT.png", ALB_FL_RD_AREA_FACETED_FIT, width = 8, height = 6)


print("FL-RD model initialized!")