print("Initializing data description...")

# SCATTERPLOTS ####

## GENERAL DENSITY PLOT ####

MORPHOMETRIC_DATA_ALB_FL_RD_DENSITY = 
  ggplot(ALB_FL_RD, aes(x = FL, y = RD)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")  +
  # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  # scale_fill_distiller(palette=4, direction=1) +
  # scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_DENSITY.png", MORPHOMETRIC_DATA_ALB_FL_RD_DENSITY, width = 8, height = 6)

## SOURCE ####

MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point(size = 0.9) +
  scale_color_manual(values = COLORS_SOURCES$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE.png", MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE, width = 8, height = 6)

MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SOURCE)) +
  geom_point(size = 0.9) +
  scale_color_manual(values = COLORS_SOURCES$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SOURCE)

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_SOURCE_FACETED, width = 8, height = 6)

## FISHERY GROUP ####

MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(alpha = 0.4, size = 0.9) +
  scale_color_manual(values = FG_COL$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP.png", MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP, width = 8, height = 6)

MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(alpha = 0.4, size = 0.9) +
  scale_color_manual(values = FG_COL$FILL) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~FISHERY_GROUP)

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_FISHERY_GROUP_FACETED, width = 8, height = 6)

## SEX ####

MORPHOMETRIC_DATA_ALB_FL_RD_SEX =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SEX)) +
  geom_point(alpha = 0.4, size = 0.9) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_SEX.png", MORPHOMETRIC_DATA_ALB_FL_RD_SEX, width = 8, height = 6)

MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SEX)) +
  geom_point(alpha = 0.4, size = 0.9) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SEX)

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED.png", MORPHOMETRIC_DATA_ALB_FL_RD_SEX_FACETED, width = 8, height = 6)

## AREA ####

ALB_FL_RD_AREA =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = SA_AREA_CODE)) +
  geom_point(alpha = 0.4, size = 0.9) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/ALB_FL_RD_AREA.png", ALB_FL_RD_AREA, width = 8, height = 6)

ALB_FL_RD_FISHERY_GROUP_AREA_FACETED =
  ggplot(ALB_FL_RD, aes(x = FL, y = RD, color = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), alpha = 0.4, size = 0.9) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)") +
  scale_color_manual(values = FG_COL$FILL) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/ ALB_FL_RD_FISHERY_GROUP_AREA_FACETED.png", ALB_FL_RD_FISHERY_GROUP_AREA_FACETED, width = 8, height = 6)

ALB_LOGFL_LOGRD_FISHERY_GROUP_AREA_FACETED =
  ggplot(ALB_FL_RD, aes(x = log10FL, y = log10RD, color = FISHERY_GROUP)) +
  geom_point(aes(color = FISHERY_GROUP), alpha = 0.4, size = 0.9) +
  labs(x = "log-Fork length (cm)", y = "log-Round weight (kg)") +
  scale_color_manual(values = FG_COL$FILL) + 
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.background = element_rect(colour = "black", fill = "white"), strip.text = element_text(size = 12)) +
  facet_wrap(~SA_AREA_CODE)

ggsave("../outputs/charts/DESCRIPTION/SCATTERPLOTS/ALB_LOGFL_LOGRD_FISHERY_GROUP_AREA_FACETED.png", ALB_LOGFL_LOGRD_FISHERY_GROUP_AREA_FACETED, width = 8, height = 6)

print("Data description initialized!")
