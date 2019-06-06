library(ggplot2)
library(raster)
library(dplyr)
library(cowplot)

files = list.files("/home/zhoylman/Downloads/forecast_raster", full.names = T)
order = c(4,1,5,2,6,3)
rasters = lapply(files, raster)

plot_raster = function(raster, legend_limits, title){
  data.p = data.frame(rasterToPoints(raster))
  colnames(data.p) = c("x","y","z")
  
  color_ramp = c("#8b0000", "#ff0000","#ffffff","#0000ff", "#003366")
  
  data.p = data.p %>% mutate(z=replace(z, z > max(legend_limits), max(legend_limits)))
  data.p = data.p %>% mutate(z=replace(z, z < min(legend_limits), min(legend_limits)))
  
  map = ggplot(data = data.p, aes(x = x, y = y, fill = z)) + 
    geom_raster()+
    ggtitle(title)+
    theme_bw() +
    coord_equal() +
    scale_fill_gradientn(expression(), colours=color_ramp, 
                         breaks = c(legend_limits[1], 0, legend_limits[2]),
                         limits = legend_limits)+ 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 10, size = 18, margin = margin(t = 20, b = -32)),
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.position = c(.01, .01),
          legend.justification = c("left", "bottom"),
          legend.box.just = "left",
          legend.margin = margin(6, 6, 6, 6),
          legend.title=element_text(size=24),
          legend.text=element_text(size=18),
          panel.border = element_blank()
    )
  return(map)
}

maps = list()

names = c("30 Day SPI (Current)", "30 Day SPI (8 Day Forecasted Change)",
          "60 Day SPI (Current)", "60 Day SPI (8 Day Forecasted Change)",
          "90 Day SPI (Current)", "90 Day SPI (8 Day Forecasted Change)")

for(i in 1:6){
  maps[[i]] = plot_raster(rasters[[order[i]]], c(-3,3), names[i])
}

png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/forecasts.png"),
    width = 18, height = 14, units = "in", res = 200)
cowplot::plot_grid(maps[[1]], maps[[2]], maps[[3]], maps[[4]], maps[[5]], maps[[6]], labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2)
dev.off()
