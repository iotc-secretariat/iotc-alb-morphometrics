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

# SAMPLES MAP ####

# World map
world_map = rnaturalearth::ne_countries(scale = "small", returnclass = c("sf"))

# Base map
BaseMap = 
  ggplot() +
  geom_sf(data = world_map, size = .2, fill = "darkgrey", col = NA) +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5))

# Map of fish samples

ALB_FL_RD_AGG_GEO = ALB_FL_RD[!is.na(FISHING_GROUND_CODE), .N, keyby = .(FISHING_GROUND_CODE, GEOM_WKT, LON_CENTROID, LAT_CENTROID)]
ALB_FL_RD_AGG_GEO[, `Number of samples` := findInterval(N, vec = c(1, 20, 100, 250, 500, 1000, 10000))]
ALB_FL_RD_AGG_GEO[, `Number of samples` := factor(`Number of samples`, labels = c("1-20", "21-100", "101-250", "251-500", "501-1,000", "1,001-10,000"))]
ALB_FL_RD_AGG_GEO_SF = st_as_sf(ALB_FL_RD_AGG_GEO, coords = c("LON_CENTROID", "LAT_CENTROID"), crs = st_crs(4326))

# Long/lat represent the centroids of the positions
SAMPLES_MAP = 
BaseMap +
  geom_sf(data = ALB_SA_AREAS, fill = NA) +
  scale_x_continuous(limits = c(20, 145)) +
  scale_y_continuous(limits = c(-60, 30)) +
  geom_sf(data = ALB_FL_RD_AGG_GEO_SF, aes(size = `Number of samples`), fill = "red", alpha = 0.3, shape = 21) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")

ggsave("../outputs/charts/DESCRIPTION/SAMPLES_MAP.png", SAMPLES_MAP, width = 12, height = 4.5/8*12)

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

# SAMPLING DESIGN TABLES ####

## By source and year ####

SAMPLING_DESIGN_TABLE_SOURCE_YEAR = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(SOURCE, YEAR = as.character(YEAR))]

SAMPLING_DESIGN_TABLE_SOURCE_YEAR_FT =
  SAMPLING_DESIGN_TABLE_SOURCE_YEAR %>%
  flextable() %>%
  align(part = "header", j = c("N", "FL", "RD"), align = "center") %>%
  align(part = "body", j = c("YEAR", "N", "FL", "RD"), align = "right") %>%
  autofit()

## By source and fishery group ####

SAMPLING_DESIGN_TABLE_SOURCE_FISHERY_GROUP = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(SOURCE, FISHERY_GROUP)]

SAMPLING_DESIGN_TABLE_SOURCE_FISHERY_GROUP_FT =
  SAMPLING_DESIGN_TABLE_SOURCE_FISHERY_GROUP %>%
  flextable() %>%
  align(part = "header", j = c("N", "FL", "RD"), align = "center") %>%
  align(part = "body", j = c("N", "FL", "RD"), align = "right") %>%
  autofit()

# By stock assessment area and quarter

SAMPLING_DESIGN_TABLE_AREA_QUARTER = ALB_FL_RD[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(`Assessment area` = SA_AREA_CODE, Quarter = CAPTURE_QUARTER)]

SAMPLING_DESIGN_TABLE_FT =
  SAMPLING_DESIGN_TABLE_AREA_QUARTER %>%
  flextable() %>%
  align(part = "header", align = "center") %>%
  align(part = "body", j = 2:5, align = "right") %>%
  hline(i = c(4, 7, 11, 14)) %>%
  autofit()