# Table of FL-RD parameters for ALB ####
MORPHO_PARAMS_ALB = data.table(read.xlsx("../inputs/data/ALB_Morphometric_Parameters.xlsx"))

MORPHO_PARAMS_ALB_FT = 
  MORPHO_PARAMS_ALB[Target == "RD", -c("Source", "Target", "Type", "Area", "MinRD", "MaxRD", "Comment")] %>%
  flextable() %>%
  align(part = "header", align = "center") %>%
  flextable::font(part = "all", fontname = "Calibri") %>%
  flextable::fontsize(size = 9) %>%
  set_formatter(a = function(x) {formatC(x, format = "e", digits = 4)}) %>%
  border_inner() %>%
  border_outer(border = fp_border(width = 2)) %>%
  autofit()

# Curve comparison
FL_SEQ = seq(min(MORPHO_PARAMS_ALB$MinF), max(MORPHO_PARAMS_ALB$MaxFL), 0.1)

RD_PREDICTIONS_TABLE = data.table(FL = FL_SEQ)

RD_PREDICTIONS_TABLE[, `Penney 1994` := MORPHO_PARAMS_ALB[ReferenceCode == "Penney1994", a] * FL ^ MORPHO_PARAMS_ALB[ReferenceCode == "Penney1994", b]]

RD_PREDICTIONS_TABLE[, `Hsu (1999)` := MORPHO_PARAMS_ALB[ReferenceCode == "Hsu1999", a] * FL ^ MORPHO_PARAMS_ALB[ReferenceCode == "Hsu1999", b]]

RD_PREDICTIONS_TABLE[, `Kitakado et al. (2019)` := MORPHO_PARAMS_ALB[ReferenceCode == "Kitakado2019", a] * FL ^ MORPHO_PARAMS_ALB[ReferenceCode == "Kitakado2019", b]]

PUBLISHED_RD_PREDICTIONS = melt.data.table(RD_PREDICTIONS_TABLE, id.vars = "FL", variable.name = "SOURCE", value.name = "RD")

ALB_FL_RD_LINEPLOT = 
  ggplot(data = PUBLISHED_RD_PREDICTIONS) + aes(x = FL, y = RD, color = SOURCE) + 
  geom_line(linewidth = 0.8) + 
  labs(x = "Fork length (cm)", y = "Round weight (kg)") + 
  theme(legend.position = "bottom", legend.title = element_blank())
  
ggsave("../outputs/charts/DESCRIPTION/ALB_FL_RD_LINEPLOT.png", ALB_FL_RD_LINEPLOT, width = 8, height = 6)
  
  