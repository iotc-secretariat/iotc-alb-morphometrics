print("Initializing data description...")

# SCATTERPLOTS ####

## SOURCE ####

COLORS_SOURCES = data.table(SOURCE = c("Ifremer", "UoM-IRD", "IOTC HISTORICAL", "IOTC ROS"), FILL = pal_simpsons(alpha = 0.6)(4), OUTLINE = darken(pal_simpsons(alpha = 0.6)(4), 0.2))

MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point() +
  scale_color_manual(values = COLORS_SOURCES$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE.png", MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE, width = 8, height = 6)

MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point() +
  scale_color_manual(values = COLORS_SOURCES$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SOURCE)

ggsave("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED, width = 8, height = 6)

## FISHERY GROUP ####

COLORS_FISHERY_GROUPS = data.table(FISHERY_GROUP_CODE = c("BB", "LI", "LL", "PS"), 
                                   FILL = c("#FAC090FF", "#B3A2C7FF", "#374BE5FF", "#FB4A6AFF"),
                                   OUTLINE = c("#CC935FFF", "#8E7EA0FF", "#273AC0FF", "#D81C4AFF")
                                   )
  
MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = COLORS_FISHERY_GROUPS$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP.png", MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP, width = 8, height = 6)

MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = COLORS_FISHERY_GROUPS$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~FISHERY_GROUP)

ggsave("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP_FACETED, width = 8, height = 6)

## SEX ####

MORPHOMETRIC_DATA_ALB_FL_RD_SEX =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SEX)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SEX.png", MORPHOMETRIC_DATA_ALB_FL_RD_SEX, width = 8, height = 6)

MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SEX)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SEX)

ggsave("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED, width = 8, height = 6)

ALB_FL_RD_AREA =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SA_AREA_CODE)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/ALB_FL_RD_AREA.png", ALB_FL_RD_AREA, width = 8, height = 6)

ALB_FL_RD_FISHERY_GROUP_AREA_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  scale_color_manual(values = COLORS_FISHERY_GROUPS$FILL) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/DESCRIPTION/ALB_FL_RD_FISHERY_GROUP_AREA_FACETED.png", ALB_FL_RD_FISHERY_GROUP_AREA_FACETED, width = 8, height = 6)

ALB_LOGFL_LOGRD_FISHERY_GROUP_AREA_FACETED =
  ggplot(ALB_FL_RD, aes(x = log10FL, y = log10RD, color = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), alpha = 0.4) +
  labs(x = "log-Fork length (cm)", y = "log-Round weight (kg)") +
  scale_color_manual(values = COLORS_FISHERY_GROUPS$FILL) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/DESCRIPTION/ALB_LOGFL_LOGRD_FISHERY_GROUP_AREA_FACETED.png", ALB_LOGFL_LOGRD_FISHERY_GROUP_AREA_FACETED, width = 8, height = 6)

# FIELDS DEFINITIONS

FIELD_DEFINITIONS = data.table(read.xlsx("../inputs/data/Metadata_Table.xlsx", sheet = "metadata"))[, .(Field, Definition)]

FIELD_DEFINITIONS_FT =
  FIELD_DEFINITIONS %>%
  flextable() %>%
  flextable::font(fontname = "calibri", part = c("head")) %>%
  flextable::font(fontname = "calibri", part = c("body")) %>%
  flextable::fontsize(size = 9) %>%
  autofit()

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

print("Data description initialized!")
