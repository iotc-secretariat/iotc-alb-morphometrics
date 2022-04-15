
# SCATTERPLOTS ####

## GEAR GROUP ####

COLORS_FISHERY_GROUPS = rbindlist(lapply(list("LL", "PS", "BB"), colors_for_fishery_group))

MORPHOMETRIC_DATA_ALB_FL_RD_GEAR_GROUP =
  ggplot(MORPHOMETRICS_TABLE, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = COLORS_FISHERY_GROUPS$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_GEAR_GROUP.png", MORPHOMETRIC_DATA_ALB_FL_RD_GEAR_GROUP, 8, 6)

MORPHOMETRIC_DATA_ALB_FL_RD_GEAR_GROUP_FACETED =
  ggplot(MORPHOMETRICS_TABLE, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SOURCE)

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_GEAR_GROUP_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_GEAR_GROUP_FACETED, 8, 6)

## SOURCE ####

COLORS_SOURCES = data.table(RFMO = c("Ifremer", "MUS University-IRD", "IOTC-HISTORICAL", "IOTC ROS"), FILL = pal_simpsons(alpha = 0.6)(4), OUTLINE = darken(pal_simpsons(alpha = 0.6)(4), 0.2))

MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE =
  ggplot(MORPHOMETRICS_TABLE, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point() +
  scale_color_manual(values = COLORS_SOURCES$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE.png", MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE, 8, 6)

MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED =
  ggplot(MORPHOMETRICS_TABLE, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point() +
  scale_color_manual(values = COLORS_SOURCES$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SOURCE)

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED, 8, 6)

## SEX ####

MORPHOMETRIC_DATA_ALB_FL_RD_SEX =
  ggplot(MORPHOMETRICS_TABLE, aes(x = FL, y = RD, color = SEX)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SEX.png", MORPHOMETRIC_DATA_ALB_FL_RD_SEX, 8, 6)

MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED =
  ggplot(MORPHOMETRICS_TABLE, aes(x = FL, y = RD, color = SEX)) +
  geom_point(alpha = 0.4) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SEX)

save_plot("../outputs/charts/DESCRIPTION/MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED, 8, 6)

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

# SAMPLING_DESIGN_TABLE = TUNA_SAMPLES[, .(N = length(unique(organism_identifier)), LD = paste(min(round(first_dorsal_length), na.rm = TRUE), max(round(first_dorsal_length), na.rm = TRUE), sep = "-"), LF = paste(min(round(fork_length), na.rm = TRUE), max(round(fork_length), na.rm = TRUE), sep = "-"), WR = paste(min(round(whole_weight_kg, 1), na.rm = TRUE), max(round(whole_weight_kg, 1), na.rm = TRUE), sep = "-")), keyby = .(Ocean = ocean, `Species code` = species_code_fao, `Species name` = species_english_name)]
# 
# SAMPLING_DESIGN_TABLE_FT =
#   SAMPLING_DESIGN_TABLE %>% 
#   flextable() %>%
#   merge_at(i = 1:3, j = "Ocean", part = "body") %>%
#   merge_at(i = 4:6, j = "Ocean", part = "body") %>%
#   hline(i = 3, border = fp_border(width = 1)) %>%
#   compose(part = "header", j = "LD", value = as_paragraph("L", as_sub("D"))) %>%
#   compose(part = "header", j = "LF", value = as_paragraph("L", as_sub("F"))) %>%
#   compose(part = "header", j = "WR", value = as_paragraph("W", as_sub("R"))) %>%
#   align(part = "header", j = 4:7, align = "center") %>%
#   align(part = "body", j = 4:7, align = "right") %>%
#   autofit() %>%
#   fix_border_issues()


