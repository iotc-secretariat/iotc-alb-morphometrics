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

## SPATIALIZE THE SAMPLES ####

MORPHOMETRICS_TABLE_GEO = MORPHOMETRICS_TABLE[!is.na(LONG), .N, keyby = .(LONG, LAT)]
MORPHOMETRICS_TABLE_GEO[, N_CLASS := findInterval(N, vec = c(1, 5, 20, 50, 100, 1000, 2500))]
MORPHOMETRICS_TABLE_GEO[, N_CLASS := factor(N_CLASS, labels = c("1-5", "6-20", "21-50", "51-100", "101-1,000", "1,001-2,500"))]
MORPHOMETRICS_TABLE_GEO_SP = st_as_sf(MORPHOMETRICS_TABLE_GEO, coords = c("LONG", "LAT"), crs = st_crs(4326))

## SA AREAS ####

ALB_SA_AREAS_DATA = fishing_grounds_data(c("IRALB01", "IRALB02", "IRALB03", "IRALB04"))
ALB_SA_AREAS      = st_make_valid(sf_for(ALB_SA_AREAS_DATA))

# Chagos archipelago
# CHAGOS = st_make_valid(sf_for(fishing_grounds_data('IRGBTEZ')))

# SAMPLES MAP ####

# World map
world_map = rnaturalearth::ne_countries(scale = "small", returnclass = c("sf"))

# Base map
BaseMap = 
  ggplot() +
  geom_sf(data = world_map, size = .2, fill = "darkgrey", col = NA) +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5))

# Map of fish samples
# Long/lat represent the centroids of the positions
SAMPLES_MAP = 
BaseMap +
  geom_sf(data = ALB_SA_AREAS, fill = NA) +
  scale_x_continuous(limits = c(20, 145)) +
  scale_y_continuous(limits = c(-60, 30)) +
  scale_color_manual(values = SPECIES_COL_SHAPE$FILL) +
  geom_sf(data = MORPHOMETRICS_TABLE_GEO_SP, aes(size = N_CLASS), fill = "red", alpha = 0.3, shape = 21) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Longitude", y = "Latitude")

ggsave("../outputs/charts/DESCRIPTION/SAMPLES_MAP.png", SAMPLES_MAP, width = 12, height = 4.5/8*12)

# SAMPLING DESIGN ####

# Assign area to each fish when spatial information is available

MORPHOMETRICS_TABLE_SF = st_as_sf(MORPHOMETRICS_TABLE[!is.na(LONG)], coords = c("LONG", "LAT"), crs = st_crs(4326))
MORPHOMETRICS_TABLE_SF = st_join(MORPHOMETRICS_TABLE_SF, ALB_SA_AREAS)
MORPHOMETRICS_TABLE_DT = as.data.table(MORPHOMETRICS_TABLE_SF)[, -c("geometry")] 
MORPHOMETRICS_TABLE_DT[, QUARTER := quarter(CAPTURE_DATE_START)]
MORPHOMETRICS_TABLE_DT[is.na(CODE), CODE := "OUTSIDE"]

SAMPLING_DESIGN_TABLE = MORPHOMETRICS_TABLE_DT[, .(N = length(unique(FISH_IDENTIFIER)), FL = paste(min(round(FL), na.rm = TRUE), max(round(FL), na.rm = TRUE), sep = "-"), RD = paste(min(round(RD, 1), na.rm = TRUE), max(round(RD, 1), na.rm = TRUE), sep = "-")), keyby = .(`Assessment area` = CODE, Quarter = QUARTER)]

#SAMPLING_DESIGN_TABLE[is.na(`Assessment area`), `Assessment area` := "Outside areas"]

SAMPLING_DESIGN_TABLE_FT =
  SAMPLING_DESIGN_TABLE[!is.na(`Assessment area`)] %>%
  flextable() %>%
  compose(part = "header", j = "FL", value = as_paragraph("L", as_sub("F"))) %>%
  compose(part = "header", j = "RD", value = as_paragraph("W", as_sub("R"))) %>%
  align(part = "header", align = "center") %>%
  align(part = "body", j = 2:5, align = "right") %>%
  autofit() %>%
  fix_border_issues()

MORPHOMETRIC_DATA_ALB_FL_RD_AREA_FACETED =
  ggplot(MORPHOMETRICS_TABLE_DT, aes(x = FL, y = RD, color = CODE)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~CODE)

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_AREA_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_AREA_FACETED, 8, 6)

MORPHOMETRIC_DATA_ALB_FL_RD_AREA =
  ggplot(MORPHOMETRICS_TABLE_DT, aes(x = FL, y = RD, color = CODE)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_AREA.png", MORPHOMETRIC_DATA_ALB_FL_RD_AREA, 8, 6)


