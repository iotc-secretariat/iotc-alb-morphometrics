# World map ####
world_map = rnaturalearth::ne_countries(scale = "small", returnclass = c("sf"))

# Base map ####
BaseMap = 
  ggplot() +
  geom_sf(data = world_map, size = .2, fill = "darkgrey", col = NA) +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5))

# Map of samples ####
ALB_FL_RD_AGG_GEO = ALB_FL_RD[!is.na(FISHING_GROUND_CODE), .N, keyby = .(FISHING_GROUND_CODE, GEOM_WKT, LON_CENTROID, LAT_CENTROID)]
ALB_FL_RD_AGG_GEO[, `Number of samples` := findInterval(N, vec = c(1, 20, 100, 250, 500, 1000, 10000))]
ALB_FL_RD_AGG_GEO[, `Number of samples` := factor(`Number of samples`, labels = c("1-20", "21-100", "101-250", "251-500", "501-1,000", "1,001-10,000"))]
ALB_FL_RD_AGG_GEO_SF = st_as_sf(ALB_FL_RD_AGG_GEO, coords = c("LON_CENTROID", "LAT_CENTROID"), crs = st_crs(4326))

# Long/lat represent the centroids of the positions
SAMPLES_MAP_PIEPLOT = 
  BaseMap +
  geom_sf(data = ALB_SA_AREAS, fill = NA) +
  scale_x_continuous(limits = c(20, 145)) +
  scale_y_continuous(limits = c(-60, 30)) +
  geom_sf(data = ALB_FL_RD_AGG_GEO_SF, aes(size = `Number of samples`), fill = "red", alpha = 0.3, shape = 21) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")

ggsave("../outputs/charts/DESCRIPTION/SAMPLES_MAP_PIEPLOT.png", SAMPLES_MAP_PIEPLOT, width = 12, height = 4.5/8*12)

# Maps of sampling intensity ####

SAMPLES_BY_AREA_FISHERY_GROUP_FLEET = ALB_FL_RD[!is.na(GEOM_WKT), .N, keyby = .(FISHERY_GROUP_CODE, FISHERY_GROUP, FLEET_CODE, FLEET, GEOM_WKT, LON_CENTROID, LAT_CENTROID)]

SAMPLES_BY_AREA_FISHERY_GROUP_SF = st_as_sf(SAMPLES_BY_AREA_FISHERY_GROUP, wkt = "GEOM_WKT", crs = 4326)

map_sample_intensity = function(Dataset, FisheryGroupCode, FleetCode){

  if(FleetCode == "All") FC = sort(unique(Dataset$FLEET_CODE)) else FC = FleetCode
  if(FisheryGroupCode == "All") FG = sort(unique(Dataset$FISHERY_GROUP_CODE)) else FG = FisheryGroupCode
  
  DATA_SET_FILTERED = Dataset[Dataset$FISHERY_GROUP_CODE %in% FG & Dataset$FLEET_CODE %in% FC,]
  
  MAP_SAMPLES_INTENSITY =
  ggplot() + 
  geom_sf(data = ALB_SA_AREAS, fill = NA) + 
  scale_x_continuous(limits = c(20, 145)) + 
  scale_y_continuous(limits = c(-60, 30)) + 
  geom_sf(data = DATA_SET_FILTERED, aes(fill = N)) +
  scale_fill_gradientn(colours = rev(heat_hcl(10))) + 
  theme_bw() + 
  geom_sf(data = world_map, size = .2, fill = "darkgrey", col = NA) +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5)) + 
  theme(legend.position = c(0.2, 0.11), legend.direction = "horizontal")
  
  return(MAP_SAMPLES_INTENSITY)
}

## Maps by fishery group ####

MAP_SAMPLES_INTENSITY_BB = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "BB", "All") + labs(title = "Baitboat")
MAP_SAMPLES_INTENSITY_LI = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LI", "All") + labs(title = "Line")
MAP_SAMPLES_INTENSITY_PS = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "PS", "All") + labs(title = "Purse seine")
MAP_SAMPLES_INTENSITY_LL = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LL", "All") + labs(title = "Longline") + theme(legend.text=element_text(size = 8))

# Combine plot
MAP_SAMPLES_INTENSITY_FG  = (MAP_SAMPLES_INTENSITY_BB + MAP_SAMPLES_INTENSITY_LI) / (MAP_SAMPLES_INTENSITY_PS + MAP_SAMPLES_INTENSITY_LL)

ggsave("../outputs/charts/DESCRIPTION/MAP_SAMPLES_INTENSITY_FISHERY_GROUP.png", MAP_SAMPLES_INTENSITY_FG, width = 16, height = 12)

## Maps by fleet for longline fisheries ####

MAP_SAMPLES_INTENSITY_LL_EUFRA = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LL", "EUFRA") + labs(title = "EU,France")
MAP_SAMPLES_INTENSITY_LL_JPN = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LL", "JPN") + labs(title = "Japan")
MAP_SAMPLES_INTENSITY_LL_TWN = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LL", "TWN") + labs(title = "Taiwan,China")
MAP_SAMPLES_INTENSITY_LL_KOR = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LL", "KOR") + labs(title = "Republic of Korea")
MAP_SAMPLES_INTENSITY_LL_NEI = map_sample_intensity(SAMPLES_BY_AREA_FISHERY_GROUP_SF, "LL", "NEI") + labs(title = "Not elsewhere included")

MAP_SAMPLES_INTENSITY_LL_FLEETS = (MAP_SAMPLES_INTENSITY_LL_EUFRA + MAP_SAMPLES_INTENSITY_LL_JPN) / (MAP_SAMPLES_INTENSITY_LL_TWN + MAP_SAMPLES_INTENSITY_LL_KOR)

ggsave("../outputs/charts/DESCRIPTION/MAP_SAMPLES_INTENSITY_LL_FLEETS.png", MAP_SAMPLES_INTENSITY_LL_FLEETS, width = 16, height = 12)

# TEST: MAP WITH CONTOURS

# SAMPLES_MAP_CONTOURS = 
#   BaseMap +
#   geom_sf(data = ALB_SA_AREAS, fill = NA) +
#   scale_x_continuous(limits = c(20, 145)) +
#   scale_y_continuous(limits = c(-60, 30)) +
#   geom_sf(data = SAMPLES_BY_AREA_FISHERY_GROUP_SF[SAMPLES_BY_AREA_FISHERY_GROUP_SF$FISHERY_GROUP_CODE == "LL", ], aes(fill = N)) +
#   scale_fill_gradientn(colours = rev(heat_hcl(10))) +
# #  geom_contour(data = SAMPLES_BY_AREA_FISHERY_GROUP_SF[SAMPLES_BY_AREA_FISHERY_GROUP_SF$FISHERY_GROUP_CODE == "LL", ], aes(x = LON_CENTROID, y = LAT_CENTROID, z = N), color = "black", breaks = c(0, 3, 10, 30, 100, 300, 1000, 3000, 10000), show.legend = FALSE) +
# #  geom_text_contour() +
#   theme_bw() +
#   labs(x = "Longitude", y = "Latitude") +
#   theme(legend.position = c(0.2, 0.11), legend.direction = "horizontal")