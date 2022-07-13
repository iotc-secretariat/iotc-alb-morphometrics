print("Initalizing color palettes...")

# Define fishery group colors (IOTC standard)
FG_COL = data.table(FISHERY_GROUP_CODE = c("LL", "PS", "LI", "BB"), 
                    FILL = c("#374BE5FF", "#FB4A6AFF", "#B3A2C7FF", "#FAC090FF"), 
                    OUTLINE = c("#182FB5FF", "#C2013CFF", "#7F6B95FF", "#BF7A13FF")
)

# Define longline fleet colors (IOTC standards)

FLEET_COL = data.table(FLEET_CODE = c("CHN", "EUFRA", "JPN", "TWN", "KOR"), 
                       FILL = c("#E34A33", "#08519C", "#FF7F00", "#FDBB84", "#6A3D9A"), 
                       OUTLINE = c("#C04D40", "#03498E", "#DC7534", "#DBA882", "#5D3B83")
)

# Define data source colors
COLORS_SOURCES = data.table(SOURCE = c("OFDC", "Ifremer", "UoM-IRD", "IOTC HISTORICAL", "IOTC ROS"), FILL = pal_simpsons(alpha = 0.6)(5), OUTLINE = darken(pal_simpsons(alpha = 0.6)(5), 0.2))

print("Colors palettes initialized!")