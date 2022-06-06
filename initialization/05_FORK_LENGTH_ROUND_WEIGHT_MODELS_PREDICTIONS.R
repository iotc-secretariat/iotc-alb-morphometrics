
# Model predictions
PREDICTION_DT = data.table(expand.grid(FL = seq(50, 150, 0.1), SA_AREA_CODE = c("IRALB01", "IRALB02", "IRALB03", "IRALB04", "IRALB05")))
PREDICTION_DT[, log10FL := log(FL, 10)]
PREDICTION_DT[, RD := (10^predict.lm(LM_ALB_FL_RD, newdata = PREDICTION_DT)) * BIAS_CORRECTION_FACTOR]

ALB_FL_RD_AREA_FACETED_FIT =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, col = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), shape = 21, size = .8) +
  theme_bw() +
  scale_color_manual(values = COLORS_FISHERY_GROUPS$FILL) + 
  geom_line(data = PREDICTION_DT, aes(x = FL, y = RD), size = .4, col = "black") +
  scale_x_continuous(limits = c(50, 150)) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/FITS/ALB_FL_RD_AREA_FIT.png", ALB_FL_RD_AREA_FACETED_FIT, width = 8, height = 6)




