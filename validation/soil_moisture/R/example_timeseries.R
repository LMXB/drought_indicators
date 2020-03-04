library(ggplot2)
library(fame)
library(scales)

load("~/drought_indicators_data/preprocessed_soil_moisture/mesonet_soil_moisture_list.Rdata")

data = data.frame(x = as.POSIXct(mesonet_soil_moisture_list[[46]]$Date),
                  y = rowMeans(mesonet_soil_moisture_list[[46]][,2:6], na.rm = T))

diff = diff(data$y)

data = data[2:length(data$x),] %>%
  mutate(drv = diff) %>%
  mutate(drv_cond = as.factor(ifelse(drv > 0, "Wetting", "Drying")))

plot = ggplot(data = data, aes(x = x, y = y, color = drv_cond, group=1))+
  geom_line()+
  theme_bw(base_size = 14)+
  xlab(NULL)+
  ylab(expression(paste("Soil Moisture (m" ^3, "/m" ^3, ")")))+
  scale_x_datetime(labels = date_format("%Y"), date_breaks = "1 year")+
  scale_color_manual(breaks = c("Wetting", "Drying"),
                     values = c("red", "blue"))+
  labs(color=NULL)+
  theme(legend.position = c(0.912, 0.097))+
  theme(legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave("./validation/soil_moisture/plots/summary/wetting_drying_example.png",
       plot, width = 7, height = 4, units = "in", dpi = 400)

data = data.frame(x = mesonet_soil_moisture_list[[28]]$Date,
                  y = mesonet_soil_moisture_list[[28]]$soilwc00)
