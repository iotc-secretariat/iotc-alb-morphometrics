# Annual number of samples by fishery group ####

NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT = 
ggplot(data = ALB_FL_RD, aes(x = YEAR, fill = FISHERY_GROUP, col = FISHERY_GROUP)) + 
  geom_bar() +
  labs(x = "", y = "Number of samples") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")
  
ggsave("../outputs/charts/DESCRIPTION/DESIGN/NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT.png", NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT, width = 10, height = 8)

# Monthly number of samples by fishery group ####

NUMBER_SAMPLES_FISHERY_GROUP_MONTH_BARPLOT = 
  ggplot(data = ALB_FL_RD, aes(x = MONTH, fill = FISHERY_GROUP, col = FISHERY_GROUP)) + 
  geom_bar() +
  labs(x = "", y = "Number of samples") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/DESIGN/NUMBER_SAMPLES_FISHERY_GROUP_MONTH_BARPLOT.png", NUMBER_SAMPLES_FISHERY_GROUP_MONTH_BARPLOT, width = 10, height = 8)

# Distribution of fork length ####

## By year and fleet ####

FL_DISTRIBUTION_YEAR_FISHERY_GROUP_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = FL, fill = FISHERY_GROUP, col = FISHERY_GROUP)) + 
  geom_histogram(bins = 50) +
  labs(x = "Fork length (cm)", y = "Frequency") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~YEAR, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1))

ggsave("../outputs/charts/DESCRIPTION/DESIGN/FL_DISTRIBUTION_YEAR_FISHERY_GROUP_HISTOGRAM.png", FL_DISTRIBUTION_YEAR_FISHERY_GROUP_HISTOGRAM, width = 16, height = 9)

## By year and fleet ####

FL_DISTRIBUTION_YEAR_FLEET_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = FL, fill = FLEET, col = FLEET)) + 
  geom_histogram(bins = 50, color = "black") +
  labs(x = "Fork length (cm)", y = "Frequency") + 
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~YEAR, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1))

ggsave("../outputs/charts/DESCRIPTION/DESIGN/FL_DISTRIBUTION_YEAR_FLEET_HISTOGRAM.png", FL_DISTRIBUTION_YEAR_FLEET_HISTOGRAM, width = 16, height = 9)

## By fishery group ####

FL_DISTRIBUTION_FISHERY_GROUP_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = FL, fill = FISHERY_GROUP, col = FISHERY_GROUP)) + 
  geom_histogram(bins = 50) +
  labs(x = "Fork length (cm)", y = "Frequency") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/DESIGN/FL_DISTRIBUTION_FISHERY_GROUP_HISTOGRAM.png", FL_DISTRIBUTION_FISHERY_GROUP_HISTOGRAM, width = 10, height = 8)

## By longline fleet ####

FL_DISTRIBUTION_LL_FLEET_HISTOGRAM = 
  ggplot(data = ALB_FL_RD[FLEET_CODE != "NEI" & FISHERY_GROUP_CODE == "LL"], aes(x = FL, fill = FLEET, col = FLEET)) + 
  geom_histogram(bins = 50) +
  labs(x = "Fork length (cm)", y = "Frequency") + 
  scale_fill_manual(values = FLEET_COL$FILL) +
  scale_color_manual(values = FLEET_COL$OUTLINE) +
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FLEET, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/DESIGN/FL_DISTRIBUTION_LL_FLEET_HISTOGRAM.png", FL_DISTRIBUTION_LL_FLEET_HISTOGRAM, width = 10, height = 8)

## By sex ####

FL_DISTRIBUTION_SEX_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = FL, fill = FLEET, col = FLEET)) + 
  geom_histogram(bins = 50, color = "black") +
  labs(x = "Fork length (cm)", y = "Frequency") + 
  scale_x_continuous(limits = c(50, 130)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~SEX)

ggsave("../outputs/charts/DESCRIPTION/DESIGN/FL_DISTRIBUTION_SEX_HISTOGRAM.png", FL_DISTRIBUTION_SEX_HISTOGRAM, width = 10, height = 8)

# Distribution of round weight ####

## By year and fleet ####

RD_DISTRIBUTION_YEAR_FISHERY_GROUP_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = RD, fill = FISHERY_GROUP, col = FISHERY_GROUP)) + 
  geom_histogram(bins = 30) +
  labs(x = "Round weight (kg)", y = "Frequency") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~YEAR, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1))

ggsave("../outputs/charts/DESCRIPTION/DESIGN/RD_DISTRIBUTION_YEAR_FISHERY_GROUP_HISTOGRAM.png", RD_DISTRIBUTION_YEAR_FISHERY_GROUP_HISTOGRAM, width = 16, height = 9)

## By year and fleet ####

RD_DISTRIBUTION_YEAR_FLEET_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = RD, fill = FLEET, col = FLEET)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(x = "Round weight (kg)", y = "Frequency") + 
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~YEAR, scales = "free_y") +
  guides(fill = guide_legend(nrow = 1))

ggsave("../outputs/charts/DESCRIPTION/DESIGN/RD_DISTRIBUTION_YEAR_FLEET_HISTOGRAM.png", RD_DISTRIBUTION_YEAR_FLEET_HISTOGRAM, width = 16, height = 9)

## By fishery group ####

RD_DISTRIBUTION_FISHERY_GROUP_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = RD, fill = FISHERY_GROUP, col = FISHERY_GROUP)) + 
  geom_histogram(bins = 30) +
  labs(x = "Round weight (kg)", y = "Frequency") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/DESIGN/RD_DISTRIBUTION_FISHERY_GROUP_HISTOGRAM.png", RD_DISTRIBUTION_FISHERY_GROUP_HISTOGRAM, width = 10, height = 8)

## By longline fleet ####

RD_DISTRIBUTION_LL_FLEET_HISTOGRAM = 
  ggplot(data = ALB_FL_RD[FLEET_CODE != "NEI" & FISHERY_GROUP_CODE == "LL"], aes(x = RD, fill = FLEET, col = FLEET)) + 
  geom_histogram(bins = 30) +
  labs(x = "Round weight (kg)", y = "Frequency") + 
  scale_fill_manual(values = FLEET_COL$FILL) +
  scale_color_manual(values = FLEET_COL$OUTLINE) +
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FLEET, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/DESIGN/RD_DISTRIBUTION_LL_FLEET_HISTOGRAM.png", RD_DISTRIBUTION_LL_FLEET_HISTOGRAM, width = 10, height = 8)

## By sex ####

RD_DISTRIBUTION_SEX_HISTOGRAM = 
  ggplot(data = ALB_FL_RD, aes(x = RD, fill = FLEET, col = FLEET)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(x = "Round weight (kg)", y = "Frequency") + 
  scale_x_continuous(limits = c(0, 35)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~SEX, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/DESIGN/RD_DISTRIBUTION_SEX_HISTOGRAM.png", RD_DISTRIBUTION_SEX_HISTOGRAM, width = 10, height = 8)

# SAMPLING DESIGN TABLES ####

## By source and year ####

SAMPLING_DESIGN_TABLE_SOURCE_YEAR = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(SOURCE, YEAR = as.character(YEAR))]

SAMPLING_DESIGN_TABLE_SOURCE_YEAR_FT =
  SAMPLING_DESIGN_TABLE_SOURCE_YEAR %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  align(part = "header", j = c("N", "FL", "RD"), align = "center") %>%
  align(part = "body", j = c("YEAR", "N", "FL", "RD"), align = "right") %>%
  flextable::fontsize(size = 9) %>%
  autofit()

## By source and fishery group ####

SAMPLING_DESIGN_TABLE_SOURCE_FISHERY_GROUP = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(SOURCE, FISHERY_GROUP)]

SAMPLING_DESIGN_TABLE_SOURCE_FISHERY_GROUP_FT =
  SAMPLING_DESIGN_TABLE_SOURCE_FISHERY_GROUP %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  align(part = "header", j = c("N", "FL", "RD"), align = "center") %>%
  align(part = "body", j = c("N", "FL", "RD"), align = "right") %>%
  flextable::fontsize(size = 9) %>%
  autofit()

## Bu source, fleet and fishery group ####

SAMPLING_DESIGN_TABLE_FG_SOURCE_FLEET = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(SOURCE, FISHERY_GROUP_CODE, FISHERY_GROUP, FLEET_CODE, FLEET)]

SAMPLING_DESIGN_TABLE_SOURCE_FLEET_FT =
  SAMPLING_DESIGN_TABLE_SOURCE_FLEET %>%
  flextable() %>%
  set_header_labels(values = list(
    SOURCE = "Source",
    FLEET_CODE = "Fleet code",
    FLEET = "Fleet", 
    N = "N", 
    FL = "FL",
    RD = "RD"
  )) %>%
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  align(part = "header", j = c("N", "FL", "RD"), align = "center") %>%
  align(part = "body", j = c("N", "FL", "RD"), align = "right") %>%
  border_inner() %>%
  border_outer(border = fp_border(width = 2)) %>%
  merge_v(j = "SOURCE") %>%
  flextable::fontsize(size = 9) %>%
  fix_border_issues() %>%
  autofit()

## By source and fleet ####

SAMPLING_DESIGN_TABLE_SOURCE_FLEET = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(SOURCE, FLEET_CODE, FLEET)]

SAMPLING_DESIGN_TABLE_SOURCE_FLEET_FT =
  SAMPLING_DESIGN_TABLE_SOURCE_FLEET %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  align(part = "header", j = c("N", "FL", "RD"), align = "center") %>%
  align(part = "body", j = c("N", "FL", "RD"), align = "right") %>%
  flextable::fontsize(size = 9) %>%
  autofit()

# By stock assessment area and quarter

SAMPLING_DESIGN_TABLE_AREA_QUARTER = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(`Assessment area` = SA_AREA_CODE, Quarter = CAPTURE_QUARTER)]

SAMPLING_DESIGN_TABLE_FT =
  SAMPLING_DESIGN_TABLE_AREA_QUARTER %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  align(part = "header", align = "center") %>%
  align(part = "body", j = 2:5, align = "right") %>%
  hline(i = c(4, 7, 11, 14)) %>%
  autofit()



