# Annual number of samples by fishery group ####

ggplot(data = ALB_FL_RD, aes(x = YEAR)) + 
  geom_histogram(bins = 30) + 
  labs(x = "", y = "Number of samples") + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.background = element_rect(fill = "white")) +
  facet_wrap(~FISHERY_GROUP, scales = "free_y")
  
