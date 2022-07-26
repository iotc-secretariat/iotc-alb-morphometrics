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
