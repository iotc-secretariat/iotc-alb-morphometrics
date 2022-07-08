# Annual number of samples by fishery group ####

NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT = 
ggplot(data = ALB_FL_RD, aes(x = YEAR, fill = FISHERY_GROUP_CODE, col = FISHERY_GROUP_CODE)) + 
  #geom_histogram(stat = "count") + 
  geom_bar() +
  labs(x = "", y = "Number of samples") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")
  
ggsave("../outputs/charts/DESCRIPTION/NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT.png", NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT, width = 10, height = 8)

# Monthly number of samples by fishery group ####

NUMBER_SAMPLES_FISHERY_GROUP_MONTH_BARPLOT = 
  ggplot(data = ALB_FL_RD, aes(x = MONTH, fill = FISHERY_GROUP_CODE, col = FISHERY_GROUP_CODE)) + 
  geom_bar() +
  labs(x = "", y = "Number of samples") + 
  scale_fill_manual(values = FG_COL$FILL) +
  scale_color_manual(values = FG_COL$OUTLINE) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white"), legend.position = "none") +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")

ggsave("../outputs/charts/DESCRIPTION/NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT.png", NUMBER_SAMPLES_FISHERY_GROUP_YEAR_BARPLOT, width = 10, height = 8)

