library(ggplot2)
library(dplyr)

# load in misc functions to sort data and find best
source("./validation/soil_moisture/R/misc_functions.R")

# correlation matrix data
list.files("~/drought_indicators_data/correlation_matrix/",pattern = ".RData", full.names = T)%>%
  lapply(., load, .GlobalEnv)

# set up storage lists for best cors
best_times_list = list()
best_cor_list = list()
best_times_mesonet_list = list()
best_cor_mesonet_list = list()

for(i in 1:8){
  if(i < 5){
    best_times_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_spi), ncol = 6))
    colnames(best_times_list[[i]]) = c("2in","4in", "8in", "20in", "40in", "mean")
    
    best_cor_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_spi), ncol = 6))
    colnames(best_cor_list[[i]]) = c("2in","4in", "8in", "20in", "40in", "mean")
    
    best_times_mesonet_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_mesonet_spi), ncol = 6))
    colnames(best_times_mesonet_list[[i]]) = c("0in", "4in", "8in", "20in", "36in", "mean")
    
    best_cor_mesonet_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_mesonet_spi), ncol = 6))
    colnames(best_cor_mesonet_list[[i]]) = c("0in", "4in", "8in", "20in", "36in", "mean")
  }
  else{
    best_times_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_spi), ncol = 12))
    colnames(best_times_list[[i]]) = paste0(c(rep("wet_",6), rep("dry_",6)), c("2in","4in", "8in", "20in", "40in", "mean"))
    
    best_cor_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_spi), ncol = 12))
    colnames(best_cor_list[[i]]) = paste0(c(rep("wet_",6), rep("dry_",6)), c("2in","4in", "8in", "20in", "40in", "mean"))
    
    best_times_mesonet_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_mesonet_spi), ncol = 12))
    colnames(best_times_mesonet_list[[i]]) = paste0(c(rep("wet_",6), rep("dry_",6)), c("0in", "4in", "8in", "20in", "36in", "mean"))
    
    best_cor_mesonet_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_mesonet_spi), ncol = 12))
    colnames(best_cor_mesonet_list[[i]]) = paste0(c(rep("wet_",6), rep("dry_",6)), c("0in", "4in", "8in", "20in", "36in", "mean"))
  }
}

names(best_times_list) = c("spi","spei","eddi","sedi","spi_wet_dry","spei_wet_dry","eddi_wet_dry","sedi_wet_dry")
names(best_cor_list) = c("spi","spei","eddi","sedi","spi_wet_dry","spei_wet_dry","eddi_wet_dry","sedi_wet_dry")
names(best_times_mesonet_list) = c("spi","spei","eddi","sedi","spi_wet_dry","spei_wet_dry","eddi_wet_dry","sedi_wet_dry")
names(best_cor_mesonet_list) = c("spi","spei","eddi","sedi","spi_wet_dry","spei_wet_dry","eddi_wet_dry","sedi_wet_dry")

#copy empty list for summer time corelation matrix
best_times_list_summer = best_times_list[(c(1:4))]
best_cor_list_summer = best_cor_list[(c(1:4))]
best_times_mesonet_list_summer = best_times_mesonet_list[(c(1:4))]
best_cor_mesonet_list_summer = best_cor_mesonet_list[(c(1:4))]


# find best correlations and times for snotel
for(i in 1:length(correlation_matrix_spi)){
  tryCatch({
    best_times_list$spi[i,] = find_best(correlation_matrix_spi[[i]])
    best_times_list$spei[i,] = find_best(correlation_matrix_spei[[i]])
    best_times_list$eddi[i,] = find_best_neg(correlation_matrix_snotel_eddi[[i]])
    best_times_list$sedi[i,] = find_best_neg(correlation_matrix_snotel_sedi[[i]])
    
    best_cor_list$spi[i,] = find_best_cor(correlation_matrix_spi[[i]])
    best_cor_list$spei[i,] = find_best_cor(correlation_matrix_spei[[i]])
    best_cor_list$eddi[i,] = find_best_cor_neg(correlation_matrix_snotel_eddi[[i]])
    best_cor_list$sedi[i,] = find_best_cor_neg(correlation_matrix_snotel_sedi[[i]])
    
    #repreat for summer
    best_times_list_summer$spi[i,] = find_best(correlation_matrix_summer_snotel_spi[[i]])
    best_times_list_summer$spei[i,] = find_best(correlation_matrix_summer_snotel_spei[[i]])
    best_times_list_summer$eddi[i,] = find_best_neg(correlation_matrix_summer_snotel_eddi[[i]])
    best_times_list_summer$sedi[i,] = find_best_neg(correlation_matrix_summer_snotel_sedi[[i]])
    
    best_cor_list_summer$spi[i,] = find_best_cor(correlation_matrix_summer_snotel_spi[[i]])
    best_cor_list_summer$spei[i,] = find_best_cor(correlation_matrix_summer_snotel_spei[[i]])
    best_cor_list_summer$eddi[i,] = find_best_cor_neg(correlation_matrix_summer_snotel_eddi[[i]])
    best_cor_list_summer$sedi[i,] = find_best_cor_neg(correlation_matrix_summer_snotel_sedi[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

# find best correlations and times for mesonet
for(i in 1:length(correlation_matrix_mesonet_spi)){
  tryCatch({
    best_times_mesonet_list$spi[i,] = find_best_mesonet(correlation_matrix_mesonet_spi[[i]])
    best_times_mesonet_list$spei[i,] = find_best_mesonet(correlation_matrix_mesonet_spei[[i]])
    best_times_mesonet_list$eddi[i,] = find_best_mesonet_neg(correlation_matrix_mesonet_eddi[[i]])
    best_times_mesonet_list$sedi[i,] = find_best_mesonet_neg(correlation_matrix_mesonet_sedi[[i]])
    
    best_cor_mesonet_list$spi[i,] = find_best_mesonet_cor(correlation_matrix_mesonet_spi[[i]])
    best_cor_mesonet_list$spei[i,] = find_best_mesonet_cor(correlation_matrix_mesonet_spei[[i]])
    best_cor_mesonet_list$eddi[i,] = find_best_mesonet_cor_neg(correlation_matrix_mesonet_eddi[[i]])
    best_cor_mesonet_list$sedi[i,] = find_best_mesonet_cor_neg(correlation_matrix_mesonet_sedi[[i]])
    
    #repeat for summer
    best_times_mesonet_list_summer$spi[i,] = find_best_mesonet(correlation_matrix_summer_mesonet_spi[[i]])
    best_times_mesonet_list_summer$spei[i,] = find_best_mesonet(correlation_matrix_summer_mesonet_spei[[i]])
    best_times_mesonet_list_summer$eddi[i,] = find_best_mesonet_neg(correlation_matrix_summer_mesonet_eddi[[i]])
    best_times_mesonet_list_summer$sedi[i,] = find_best_mesonet_neg(correlation_matrix_summer_mesonet_sedi[[i]])
    
    best_cor_mesonet_list_summer$spi[i,] = find_best_mesonet_cor(correlation_matrix_summer_mesonet_spi[[i]])
    best_cor_mesonet_list_summer$spei[i,] = find_best_mesonet_cor(correlation_matrix_summer_mesonet_spei[[i]])
    best_cor_mesonet_list_summer$eddi[i,] = find_best_mesonet_cor_neg(correlation_matrix_summer_mesonet_eddi[[i]])
    best_cor_mesonet_list_summer$sedi[i,] = find_best_mesonet_cor_neg(correlation_matrix_summer_mesonet_sedi[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

## playing with wet dry data
for(i in 1:length(wet_dry_correlation_matrix_snotel_spi)){
  tryCatch({
    best_times_list$spi_wet_dry[i,] = find_best_wet_dry(wet_dry_correlation_matrix_snotel_spi[[i]])
    best_times_list$spei_wet_dry[i,] = find_best_wet_dry(wet_dry_correlation_matrix_snotel_spei[[i]])
    best_times_list$eddi_wet_dry[i,] = find_best_wet_dry_neg(wet_dry_correlation_matrix_snotel_eddi[[i]])
    best_times_list$sedi_wet_dry[i,] = find_best_wet_dry_neg(wet_dry_correlation_matrix_snotel_sedi[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  })
}

for(i in 1:length(wet_dry_correlation_matrix_mesonet_spi)){
  tryCatch({
    best_times_mesonet_list$spi_wet_dry[i,] = find_best_wet_dry_mesonet(wet_dry_correlation_matrix_mesonet_spi[[i]])
    best_times_mesonet_list$spei_wet_dry[i,] = find_best_wet_dry_mesonet(wet_dry_correlation_matrix_mesonet_spei[[i]])
    best_times_mesonet_list$eddi_wet_dry[i,] = find_best_wet_dry_mesonet_neg(wet_dry_correlation_matrix_mesonet_eddi[[i]])
    best_times_mesonet_list$sedi_wet_dry[i,] = find_best_wet_dry_mesonet_neg(wet_dry_correlation_matrix_mesonet_sedi[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  })
}

# aggregate based on generalized depths
best_times_combined = list()
best_cor_combined = list()
best_times_combined_summer = list()
best_cor_combined_summer = list()
best_times_combined_wet_dry = list()

for(i in 1:4){
  best_times_combined[[i]] = data.frame(shallow = c(best_times_list[[i]]$`2in`, best_times_list[[i]]$`4in`,
                                                    best_times_mesonet_list[[i]]$`0in`, best_times_mesonet_list[[i]]$`4in`),
                                        middle = c(best_times_list[[i]]$`8in`, best_times_list[[i]]$`20in`,
                                                   best_times_mesonet_list[[i]]$`8in`, best_times_mesonet_list[[i]]$`20in`),
                                        deep = c(best_times_list[[i]]$`40in`, best_times_mesonet_list[[i]]$`36in`),
                                        mean = c(best_times_list[[i]]$mean, best_times_mesonet_list[[i]]$mean))
  
  best_cor_combined[[i]] = data.frame(shallow = c(best_cor_list[[i]]$`2in`, best_cor_list[[i]]$`4in`,
                                                  best_cor_mesonet_list[[i]]$`0in`, best_cor_mesonet_list[[i]]$`4in`),
                                      middle = c(best_cor_list[[i]]$`8in`, best_cor_list[[i]]$`20in`,
                                                 best_cor_mesonet_list[[i]]$`8in`, best_cor_mesonet_list[[i]]$`20in`),
                                      deep = c(best_cor_list[[i]]$`40in`, best_cor_mesonet_list[[i]]$`36in`),
                                      mean = c(best_cor_list[[i]]$mean, best_cor_mesonet_list[[i]]$mean))
  
  # repeat for summer 
  
  best_times_combined_summer[[i]] = data.frame(shallow = c(best_times_list_summer[[i]]$`2in`, best_times_list_summer[[i]]$`4in`,
                                                    best_times_mesonet_list_summer[[i]]$`0in`, best_times_mesonet_list_summer[[i]]$`4in`),
                                        middle = c(best_times_list_summer[[i]]$`8in`, best_times_list_summer[[i]]$`20in`,
                                                   best_times_mesonet_list_summer[[i]]$`8in`, best_times_mesonet_list_summer[[i]]$`20in`),
                                        deep = c(best_times_list_summer[[i]]$`40in`, best_times_mesonet_list_summer[[i]]$`36in`),
                                        mean = c(best_times_list_summer[[i]]$mean, best_times_mesonet_list_summer[[i]]$mean))
  
  best_cor_combined_summer[[i]] = data.frame(shallow = c(best_cor_list_summer[[i]]$`2in`, best_cor_list_summer[[i]]$`4in`,
                                                  best_cor_mesonet_list_summer[[i]]$`0in`, best_cor_mesonet_list_summer[[i]]$`4in`),
                                      middle = c(best_cor_list_summer[[i]]$`8in`, best_cor_list_summer[[i]]$`20in`,
                                                 best_cor_mesonet_list_summer[[i]]$`8in`, best_cor_mesonet_list_summer[[i]]$`20in`),
                                      deep = c(best_cor_list_summer[[i]]$`40in`, best_cor_mesonet_list_summer[[i]]$`36in`),
                                      mean = c(best_cor_list_summer[[i]]$mean, best_cor_mesonet_list_summer[[i]]$mean))
}

for(i in 5:8){
  best_times_combined_wet_dry[[i]] = data.frame(
                                        #wet
                                        shallow_wet = c(best_times_list[[i]]$`wet_2in`, best_times_list[[i]]$`wet_4in`,
                                                    best_times_mesonet_list[[i]]$`wet_0in`, best_times_mesonet_list[[i]]$`wet_4in`),
                                        middle_wet = c(best_times_list[[i]]$`wet_8in`, best_times_list[[i]]$`wet_20in`,
                                                   best_times_mesonet_list[[i]]$`wet_8in`, best_times_mesonet_list[[i]]$`wet_20in`),
                                        deep_wet = c(best_times_list[[i]]$`wet_40in`, best_times_mesonet_list[[i]]$`wet_36in`),
                                        mean_wet = c(best_times_list[[i]]$wet_mean, best_times_mesonet_list[[i]]$wet_mean),
                                        #dry
                                        shallow_dry = c(best_times_list[[i]]$`dry_2in`, best_times_list[[i]]$`dry_4in`,
                                                        best_times_mesonet_list[[i]]$`dry_0in`, best_times_mesonet_list[[i]]$`dry_4in`),
                                        middle_dry = c(best_times_list[[i]]$`dry_8in`, best_times_list[[i]]$`dry_20in`,
                                                       best_times_mesonet_list[[i]]$`dry_8in`, best_times_mesonet_list[[i]]$`dry_20in`),
                                        deep_dry = c(best_times_list[[i]]$`dry_40in`, best_times_mesonet_list[[i]]$`dry_36in`),
                                        mean_dry = c(best_times_list[[i]]$dry_mean, best_times_mesonet_list[[i]]$dry_mean))
}

for(i in c(1,1,1,1)){
  best_times_combined_wet_dry[[i]] = NULL
}

# rename and organize
names(best_times_combined) = c("spi","spei","eddi","sedi")
names(best_cor_combined) = c("spi","spei","eddi","sedi")
names(best_times_combined_wet_dry) = c("spi","spei","eddi","sedi")
depths = c("2 - 4in","8 - 20in", "36 - 40in", "Mean")

# plot density graphs by depth
plot = list()
for(i in 1:4){
  best_cor_ggplot = c(round(median(best_cor_combined$spi[,i], na.rm = T),2),
                      round(median(best_cor_combined$spei[,i], na.rm = T),2),
                      round(median(best_cor_combined$eddi[,i], na.rm = T),2),
                      round(median(best_cor_combined$sedi[,i], na.rm = T),2))
  
  names = c("SPI: r = ","SPEI: r = ","EDDI: r = ","SEDI: r = ")
  
  names_short = c("SPI","SPEI","EDDI","SEDI")
  
  data = rbind(extract_density(best_times_combined$spi[,i], "SPI"),
               extract_density(best_times_combined$spei[,i], "SPEI"),
               extract_density(best_times_combined$eddi[,i], "EDDI"),
               extract_density(best_times_combined$sedi[,i], "SEDI"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)
  
  plot[[i]] = ggplot(data = data, aes(x = x, y=y, color = name))+
    geom_line()+
    ggtitle(paste0("Soil Moisture Depth = ", depths[i]))+
    xlab("Timescale (Days)")+
    ylab("Density")+
    theme_bw(base_size = 14)+
    xlim(0,600)+
    theme(legend.position = c(0.75, 0.75))+
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'),
          plot.title = element_text(hjust = 0.5))+
    ggrepel::geom_text_repel(data = best_times, aes(x = x, y=y, color = name, label = mround(x, 5)))+
    geom_point(data = best_times, aes(x = x, y=y, color = name))+
    scale_color_manual(breaks = names_short,
                       values = c("blue", "black", "orange", "red"),
                       labels = paste0(names, best_cor_ggplot),
                       name = "Drought Metric")
}
plot_grid = cowplot::plot_grid(plot[[4]],plot[[1]],plot[[2]],plot[[3]], nrow = 2)
ggsave("./validation/soil_moisture/plots/summary/correlation_density_unfrozen.png",
       plot_grid, width = 10, height = 9, units = "in", dpi = 400)


# plot density graphs by metric
depth_plot = list()
names_short = c("SPI","SPEI","EDDI","SEDI")
for(i in 1:length(names_short)){
  
  best_cor_ggplot = c(round(median(best_cor_combined[[i]]$mean, na.rm = T),2),
                      round(median(best_cor_combined[[i]]$shallow, na.rm = T),2),
                      round(median(best_cor_combined[[i]]$middle, na.rm = T),2),
                      round(median(best_cor_combined[[i]]$deep, na.rm = T),2))
  
  data = rbind(extract_density(best_times_combined[[i]]$mean, "Mean"),
               extract_density(best_times_combined[[i]]$shallow, "Shallow"),
               extract_density(best_times_combined[[i]]$middle, "Middle"),
               extract_density(best_times_combined[[i]]$deep, "Deep"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)
  
  depth_plot[[i]] = ggplot(data = data, aes(x = x, y = y, color = name))+
    geom_line()+
    ggtitle(names_short[i])+
    xlab("Timescale (Days)")+
    ylab("Density")+
    theme_bw(base_size = 14)+
    xlim(0,730)+
    theme(legend.position = c(0.75, 0.75))+
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'),
          plot.title = element_text(hjust = 0.5))+
    ggrepel::geom_text_repel(data = best_times, aes(x = x, y=y, color = name, label = mround(x, 5)))+
    geom_point(data = best_times, aes(x = x, y=y, color = name))+
    scale_color_manual(breaks = c("Mean", "Shallow", "Middle", "Deep"),
                       values = c("black", "forestgreen", "blue", "purple"),
                       labels = paste0(c("Mean: r = ", "Shallow: r = ", "Middle: r = ", "Deep: r = "),
                                       best_cor_ggplot),
                       name = "Probe Depth")
  
}

plot_grid_depth = cowplot::plot_grid(depth_plot[[1]],depth_plot[[2]],depth_plot[[3]],depth_plot[[4]], nrow = 2)
ggsave("./validation/soil_moisture/plots/summary/depth_density_unfrozen.png",
       plot_grid_depth, width = 10, height = 9, units = "in", dpi = 400)


### repeat for summer

# plot density graphs by metric
depth_plot_summer = list()
names_short = c("SPI","SPEI","EDDI","SEDI")
for(i in 1:length(names_short)){
  
  best_cor_ggplot = c(round(median(best_cor_combined_summer[[i]]$mean, na.rm = T),2),
                      round(median(best_cor_combined_summer[[i]]$shallow, na.rm = T),2),
                      round(median(best_cor_combined_summer[[i]]$middle, na.rm = T),2),
                      round(median(best_cor_combined_summer[[i]]$deep, na.rm = T),2))
  
  data = rbind(extract_density(best_times_combined_summer[[i]]$mean, "Mean"),
               extract_density(best_times_combined_summer[[i]]$shallow, "Shallow"),
               extract_density(best_times_combined_summer[[i]]$middle, "Middle"),
               extract_density(best_times_combined_summer[[i]]$deep, "Deep"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)
  
  depth_plot_summer[[i]] = ggplot(data = data, aes(x = x, y = y, color = name))+
    geom_line()+
    ggtitle(paste0(names_short[i], " (May - October)"))+
    xlab("Timescale (Days)")+
    ylab("Density")+
    theme_bw(base_size = 14)+
    xlim(0,730)+
    theme(legend.position = c(0.75, 0.75))+
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'),
          plot.title = element_text(hjust = 0.5))+
    ggrepel::geom_text_repel(data = best_times, aes(x = x, y=y, color = name, label = mround(x, 5)))+
    geom_point(data = best_times, aes(x = x, y=y, color = name))+
    scale_color_manual(breaks = c("Mean", "Shallow", "Middle", "Deep"),
                       values = c("black", "forestgreen", "blue", "purple"),
                       labels = paste0(c("Mean: r = ", "Shallow: r = ", "Middle: r = ", "Deep: r = "),
                                       best_cor_ggplot),
                       name = "Probe Depth")
  
}

plot_grid_depth = cowplot::plot_grid(depth_plot_summer[[1]],depth_plot_summer[[2]],depth_plot_summer[[3]],depth_plot_summer[[4]], nrow = 2)
ggsave("./validation/soil_moisture/plots/summary/depth_density_summer_unfrozen.png",
       plot_grid_depth, width = 10, height = 9, units = "in", dpi = 400)



################# wet dry plot ##########################################
wet_dry_plot = list()
# plot density graphs by metric

names_short = c("SPI","SPEI","EDDI","SEDI")
for(i in 1:length(names_short)){
  
  # best_cor_ggplot = c(round(median(best_cor_combined[[i]]$mean, na.rm = T),2),
  #                     round(median(best_cor_combined[[i]]$shallow, na.rm = T),2),
  #                     round(median(best_cor_combined[[i]]$middle, na.rm = T),2),
  #                     round(median(best_cor_combined[[i]]$deep, na.rm = T),2))
  
  data = rbind(extract_density_linetype(best_times_combined_wet_dry[[i]]$mean_wet, "Mean", "Wetting"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$shallow_wet, "Shallow", "Wetting"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$middle_wet, "Middle", "Wetting"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$deep_wet, "Deep", "Wetting"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$mean_dry, "Mean", "Drying"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$shallow_dry, "Shallow", "Drying"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$middle_dry, "Middle", "Drying"),
               extract_density_linetype(best_times_combined_wet_dry[[i]]$deep_dry, "Deep", "Drying"))
  
  best_density = data %>%
    group_by(name, linetype) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)
  
  wet_dry_plot[[i]] = ggplot(data = data, aes(x = x, y = y, color = name, linetype = linetype))+
    geom_line()+
    ggtitle(names_short[i])+
    xlab("Timescale (Days)")+
    ylab("Density")+
    theme_bw(base_size = 12)+
    xlim(0,600)+
    theme(legend.position = c(0.85, 0.65))+
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'),
          plot.title = element_text(hjust = 0.5))+
    ggrepel::geom_text_repel(data = best_times, aes(x = x, y=y, color = name, label = mround(x, 5)))+
    geom_point(data = best_times, aes(x = x, y=y, color = name))+
    scale_color_manual(breaks = c("Mean", "Shallow", "Middle", "Deep"),
                       values = c("black", "forestgreen", "blue", "purple"))+
    labs(color="Depth", linetype=NULL)
}

plot_grid_wet_dry = cowplot::plot_grid(wet_dry_plot[[1]],wet_dry_plot[[2]],wet_dry_plot[[3]],wet_dry_plot[[4]], nrow = 2)
ggsave("./validation/soil_moisture/plots/summary/wet_dry_depth_plot.png",
       plot_grid_wet_dry, width = 10, height = 9, units = "in", dpi = 400)

################# Monthly Post Processing and plots ########################
library(ggplot2)
library(scales)

index_names = c("SPI","SPEI","EDDI","SEDI")

monthly_data_snotel = list(monthly_correlation_matrix_snotel_spi, monthly_correlation_matrix_snotel_spei,
                           monthly_correlation_matrix_snotel_eddi, monthly_correlation_matrix_snotel_sedi)

monthly_data_mesonet = list(monthly_correlation_matrix_mesonet_spi, monthly_correlation_matrix_mesonet_spei,
                            monthly_correlation_matrix_mesonet_eddi, monthly_correlation_matrix_mesonet_sedi)

for(d in 1:length(monthly_data_snotel)){
#for(d in 1){
  data = rbind(extract_density(best_times_combined[[d]]$mean, "Mean"),
               extract_density(best_times_combined[[d]]$shallow, "Shallow"),
               extract_density(best_times_combined[[d]]$middle, "Middle"),
               extract_density(best_times_combined[[d]]$deep, "Deep"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)%>%
    mutate(x = mround(x,5))
  
  index = vector()
  
  for(i in 1:4){
    index[i] = which(best_times$x[i] == c(seq(5,730,5)))
  }
  
  #extract snotel
  for(i in 1:length(monthly_data_snotel[[d]])){
    mean_temp = monthly_data_snotel[[d]][[i]][[index[1]]]$mean_soil_moisture
    
    shallow_temp = rowMeans(data.frame(in_2 = monthly_data_snotel[[d]][[i]][[index[2]]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values,
                                       in_4 = monthly_data_snotel[[d]][[i]][[index[2]]]$Soil.Moisture.Percent..4in..pct..Start.of.Day.Values),
                            na.rm = TRUE)
    
    middle_temp = rowMeans(data.frame(in_8 = monthly_data_snotel[[d]][[i]][[index[3]]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values,
                                      in_20 = monthly_data_snotel[[d]][[i]][[index[3]]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values),
                           na.rm = TRUE)
    
    deep_temp = monthly_data_snotel[[d]][[i]][[index[4]]]$Soil.Moisture.Percent..40in..pct..Start.of.Day.Values
    
    if(i == 1){
      mean_full = data.frame(mean_temp)
      shallow_full = data.frame(shallow_temp)
      middle_full = data.frame(middle_temp)
      deep_full = data.frame(deep_temp)
    }
    else{
      mean_full = cbind(mean_full, mean_temp)
      shallow_full = cbind(shallow_full, shallow_temp)
      middle_full = cbind(middle_full, middle_temp)
      deep_full = cbind(deep_full, deep_temp)
    }
  }
  
  # extract mesonet
  for(i in 1:length(monthly_data_mesonet[[d]])){
    mean_temp = monthly_data_mesonet[[d]][[i]][[index[1]]]$mean_soil_moisture
    mean_full = cbind(mean_full, mean_temp)
    #shallow
    shallow_temp = rowMeans(data.frame(in_0 = monthly_data_mesonet[[d]][[i]][[index[2]]]$soilwc00,
                                       in_4 = monthly_data_mesonet[[d]][[i]][[index[2]]]$soilwc04),na.rm = TRUE)
    shallow_full = cbind(shallow_full, shallow_temp)
    #middle
    middle_temp = rowMeans(data.frame(in_8 = monthly_data_mesonet[[d]][[i]][[index[3]]]$soilwc08,
                                      in_20 = monthly_data_mesonet[[d]][[i]][[index[3]]]$soilwc20),na.rm = TRUE)
    middle_full = cbind(middle_full, middle_temp)
    #deep
    deep_temp = monthly_data_mesonet[[d]][[i]][[index[4]]]$soilwc36
    deep_full = cbind(deep_full, deep_temp)
    
  }
  
  
  summary = data.frame(median = apply(mean_full, 1, median, na.rm=TRUE),
                       upper = apply(mean_full, 1, quantile, 0.75, na.rm=TRUE),
                       lower = apply(mean_full, 1, quantile, 0.25, na.rm=TRUE),
                       time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                         format = "%Y-%m-%d %H:%M"))
  
  summary_shallow = data.frame(median = apply(shallow_full, 1, median, na.rm=TRUE),
                               upper = apply(shallow_full, 1, quantile, 0.75, na.rm=TRUE),
                               lower = apply(shallow_full, 1, quantile, 0.25, na.rm=TRUE),
                               time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                                 format = "%Y-%m-%d %H:%M"))  
  summary_middle = data.frame(median = apply(middle_full, 1, median, na.rm=TRUE),
                              upper = apply(middle_full, 1, quantile, 0.75, na.rm=TRUE),
                              lower = apply(middle_full, 1, quantile, 0.25, na.rm=TRUE),
                              time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                                format = "%Y-%m-%d %H:%M"))  
  summary_deep = data.frame(median = apply(deep_full, 1, median, na.rm=TRUE),
                            upper = apply(deep_full, 1, quantile, 0.75, na.rm=TRUE),
                            lower = apply(deep_full, 1, quantile, 0.25, na.rm=TRUE),
                            time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                              format = "%Y-%m-%d %H:%M"))  
  
  find_summer_stat = function(x){
    median = x %>%
      mutate(month = c(1:12)) %>%
      dplyr::filter(month > 4 & month < 11)%>%
      summarise(median(median))
    median = median$`median(median)`
    return(median)
  }
  
  summary_list = list(summary, summary_shallow, summary_middle, summary_deep)
  
  plot_monthly = function(data1, color1, depth, time_scale, name, summary){
    plot = ggplot() +
      geom_ribbon(data = data1, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = color1)+
      geom_line(data = data1, aes(x = time, y = median), color = color1)+
      theme_bw(base_size = 12)+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab("Correlation (r)")+
      ggtitle(paste0(name," [",time_scale," day] ~ Soil Moisture ", "[", depth, "]   "))+
      scale_x_datetime(labels = date_format("%b"),
                       date_breaks = "2 month")+
      theme(axis.title.x=element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_errorbarh(data = summary, aes(xmin = time[5], xmax = time[10],
                                     y = find_summer_stat(summary)*.7, height = 0.05, x = NULL))+
      annotate(geom = "text", y = find_summer_stat(summary)*.5, x = as.POSIXct("2018-07-15 00:00",
                                                                               format = "%Y-%m-%d %H:%M"),
               label = paste0("r = ", round(find_summer_stat(summary),2)))
    return(plot)
  }
  
  plot_monthly_eddi = function(data1, color1, depth, time_scale, name, summary){
    plot = ggplot() +
      geom_ribbon(data = data1, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = color1)+
      geom_line(data = data1, aes(x = time, y = median), color = color1)+
      theme_bw(base_size = 12)+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab("Correlation (r)")+
      ggtitle(paste0(name," [",time_scale," day] ~ Soil Moisture ", "[", depth, "]   "))+
      scale_x_datetime(labels = date_format("%b"),
                       date_breaks = "2 month")+
      theme(axis.title.x=element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_errorbarh(data = summary, aes(xmin = time[5], xmax = time[10],
                                         y = find_summer_stat(summary)+0.1, height = 0.03, x = NULL))+
      annotate(geom = "text", y = find_summer_stat(summary)+0.15, x = as.POSIXct("2018-07-15 00:00",
                                                                               format = "%Y-%m-%d %H:%M"),
               label = paste0("r = ", round(find_summer_stat(summary),2)))
    return(plot)
  }
  
  plots = list()
  
  datasets = list(summary, summary_shallow, summary_middle, summary_deep)
  colors = c("black", "forestgreen", "blue", "purple")
  depths = c("Mean", "Shallow", "Middle", "Deep")
  time_scale = c(seq(5,730,5)[index])
  
  for(i in 1:4){
    if(d == 3){
      plots[[i]] = plot_monthly_eddi(datasets[[i]], colors[i], depths[i], time_scale[i], index_names[d], summary_list[[i]])
    }
    else{
      plots[[i]] = plot_monthly(datasets[[i]], colors[i], depths[i], time_scale[i], index_names[d], summary_list[[i]])
    }
  }
  
  plot_grid_monthly = cowplot::plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]], nrow = 2)
  
  plot_final = cowplot::plot_grid(depth_plot[[d]], plot_grid_monthly , nrow = 1)
  
  ggsave(paste0("./validation/soil_moisture/plots/summary/plot_grid_monthly_",index_names[d],".png"),
         plot_final, width = 12, height = 5, units = "in", dpi = 400)
}


#repeate for summer

library(ggplot2)
library(scales)
library(matrixStats)

index_names = c("SPI","SPEI","EDDI","SEDI")

monthly_data_snotel = list(monthly_correlation_matrix_snotel_spi, monthly_correlation_matrix_snotel_spei,
                           monthly_correlation_matrix_snotel_eddi, monthly_correlation_matrix_snotel_sedi)

monthly_data_mesonet = list(monthly_correlation_matrix_mesonet_spi, monthly_correlation_matrix_mesonet_spei,
                            monthly_correlation_matrix_mesonet_eddi, monthly_correlation_matrix_mesonet_sedi)

for(d in 1:length(monthly_data_snotel)){
  #for(d in 1){
  data = rbind(extract_density(best_times_combined_summer[[d]]$mean, "Mean"),
               extract_density(best_times_combined_summer[[d]]$shallow, "Shallow"),
               extract_density(best_times_combined_summer[[d]]$middle, "Middle"),
               extract_density(best_times_combined_summer[[d]]$deep, "Deep"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)%>%
    mutate(x = mround(x,5))
  
  index = vector()
  
  for(i in 1:4){
    index[i] = which(best_times$x[i] == c(seq(5,730,5)))
  }
  
  #extract snotel
  for(i in 1:length(monthly_data_snotel[[d]])){
    mean_temp = monthly_data_snotel[[d]][[i]][[index[1]]]$mean_soil_moisture
    
    shallow_temp = tryCatch({rowMedians(as.matrix(data.frame(in_2 = monthly_data_snotel[[d]][[i]][[index[2]]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values,
                                       in_4 = monthly_data_snotel[[d]][[i]][[index[2]]]$Soil.Moisture.Percent..4in..pct..Start.of.Day.Values)), na.rm = TRUE)
    }, error=function(e) {
      return(rep(NA, 12))
    }) 
    
    middle_temp = tryCatch({
      rowMedians(as.matrix(data.frame(in_8 = monthly_data_snotel[[d]][[i]][[index[3]]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values,
                                      in_20 = monthly_data_snotel[[d]][[i]][[index[3]]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values)), na.rm = TRUE)
    }, error=function(e) {
      return(rep(NA, 12))
    }) 
    
    deep_temp = monthly_data_snotel[[d]][[i]][[index[4]]]$Soil.Moisture.Percent..40in..pct..Start.of.Day.Values
    
    if(i == 1){
      mean_full = data.frame(mean_temp)
      shallow_full = data.frame(shallow_temp)
      middle_full = data.frame(middle_temp)
      deep_full = data.frame(deep_temp)
    }
    else{
      mean_full = cbind(mean_full, mean_temp)
      shallow_full = cbind(shallow_full, shallow_temp)
      middle_full = cbind(middle_full, middle_temp)
      deep_full = cbind(deep_full, deep_temp)
    }
  }
  
  # extract mesonet
  for(i in 1:length(monthly_data_mesonet[[d]])){
    mean_temp = monthly_data_mesonet[[d]][[i]][[index[1]]]$mean_soil_moisture
    mean_full = cbind(mean_full, mean_temp)
    #shallow
    shallow_temp = tryCatch({rowMedians(as.matrix(data.frame(in_0 = monthly_data_mesonet[[d]][[i]][[index[2]]]$soilwc00,
                                       in_4 = monthly_data_mesonet[[d]][[i]][[index[2]]]$soilwc04)),na.rm = TRUE)
    }, error=function(e) {
      return(rep(NA, 12))
    }) 
    shallow_full = cbind(shallow_full, shallow_temp)
    #middle
    middle_temp = tryCatch({rowMedians(as.matrix(data.frame(in_8 = monthly_data_mesonet[[d]][[i]][[index[3]]]$soilwc08,
                                      in_20 = monthly_data_mesonet[[d]][[i]][[index[3]]]$soilwc20)),na.rm = TRUE)
    }, error=function(e) {
      return(rep(NA, 12))
    }) 
    middle_full = cbind(middle_full, middle_temp)
    #deep
    deep_temp = monthly_data_mesonet[[d]][[i]][[index[4]]]$soilwc36
    deep_full = cbind(deep_full, deep_temp)
    
  }
  
  
  summary = data.frame(median = apply(mean_full, 1, median, na.rm=TRUE),
                       upper = apply(mean_full, 1, quantile, 0.75, na.rm=TRUE),
                       lower = apply(mean_full, 1, quantile, 0.25, na.rm=TRUE),
                       time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                         format = "%Y-%m-%d %H:%M"))
  
  summary_shallow = data.frame(median = apply(shallow_full, 1, median, na.rm=TRUE),
                               upper = apply(shallow_full, 1, quantile, 0.75, na.rm=TRUE),
                               lower = apply(shallow_full, 1, quantile, 0.25, na.rm=TRUE),
                               time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                                 format = "%Y-%m-%d %H:%M"))  
  summary_middle = data.frame(median = apply(middle_full, 1, median, na.rm=TRUE),
                              upper = apply(middle_full, 1, quantile, 0.75, na.rm=TRUE),
                              lower = apply(middle_full, 1, quantile, 0.25, na.rm=TRUE),
                              time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                                format = "%Y-%m-%d %H:%M"))  
  summary_deep = data.frame(median = apply(deep_full, 1, median, na.rm=TRUE),
                            upper = apply(deep_full, 1, quantile, 0.75, na.rm=TRUE),
                            lower = apply(deep_full, 1, quantile, 0.25, na.rm=TRUE),
                            time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                              format = "%Y-%m-%d %H:%M"))  
  
  find_summer_stat = function(x){
    median = x %>%
      mutate(month = c(1:12)) %>%
      dplyr::filter(month > 4 & month < 11)%>%
      summarise(median(median))
    median = median$`median(median)`
    return(median)
  }
  
  summary_list = list(summary, summary_shallow, summary_middle, summary_deep)
  
  plot_monthly = function(data1, color1, depth, time_scale, name, summary){
    plot = ggplot() +
      geom_ribbon(data = data1, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = color1)+
      geom_line(data = data1, aes(x = time, y = median), color = color1)+
      theme_bw(base_size = 12)+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab("Correlation (r)")+
      ggtitle(paste0(name," [",time_scale," day] ~ Soil Moisture ", "[", depth, "]   "))+
      scale_x_datetime(labels = date_format("%b"),
                       date_breaks = "2 month")+
      theme(axis.title.x=element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_errorbarh(data = summary, aes(xmin = time[5], xmax = time[10],
                                         y = find_summer_stat(summary)*.7, height = 0.05, x = NULL))+
      annotate(geom = "text", y = find_summer_stat(summary)*.5, x = as.POSIXct("2018-07-15 00:00",
                                                                               format = "%Y-%m-%d %H:%M"),
               label = paste0("r = ", round(find_summer_stat(summary),2)))
    return(plot)
  }
  
  plot_monthly_eddi = function(data1, color1, depth, time_scale, name, summary){
    plot = ggplot() +
      geom_ribbon(data = data1, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = color1)+
      geom_line(data = data1, aes(x = time, y = median), color = color1)+
      theme_bw(base_size = 12)+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab("Correlation (r)")+
      ggtitle(paste0(name," [",time_scale," day] ~ Soil Moisture ", "[", depth, "]   "))+
      scale_x_datetime(labels = date_format("%b"),
                       date_breaks = "2 month")+
      theme(axis.title.x=element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_errorbarh(data = summary, aes(xmin = time[5], xmax = time[10],
                                         y = find_summer_stat(summary)+0.1, height = 0.03, x = NULL))+
      annotate(geom = "text", y = find_summer_stat(summary)+0.15, x = as.POSIXct("2018-07-15 00:00",
                                                                                 format = "%Y-%m-%d %H:%M"),
               label = paste0("r = ", round(find_summer_stat(summary),2)))
    return(plot)
  }
  
  plots = list()
  
  datasets = list(summary, summary_shallow, summary_middle, summary_deep)
  colors = c("black", "forestgreen", "blue", "purple")
  depths = c("Mean", "Shallow", "Middle", "Deep")
  time_scale = c(seq(5,730,5)[index])
  
  for(i in 1:4){
    if(d == 3){
      plots[[i]] = plot_monthly_eddi(datasets[[i]], colors[i], depths[i], time_scale[i], index_names[d], summary_list[[i]])
    }
    else{
      plots[[i]] = plot_monthly(datasets[[i]], colors[i], depths[i], time_scale[i], index_names[d], summary_list[[i]])
    }
  }
  
  plot_grid_monthly = cowplot::plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]], nrow = 2)
  
  plot_final = cowplot::plot_grid(depth_plot_summer[[d]], plot_grid_monthly , nrow = 1)
  
  ggsave(paste0("./validation/soil_moisture/plots/summary/plot_grid_monthly_summer_",index_names[d],".png"),
         plot_final, width = 12, height = 5, units = "in", dpi = 400)
}




################### site map #######################
library(dplyr)
#read in geospatial data
station_data = read.csv("~/drought_indicators_data/mesonet/station_data_clean.csv")
station_data$X = NULL
snotel = read.csv("./validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")
snotel_cropped = snotel %>%
  dplyr::select(site_name, latitude, longitude) %>%
  rename(station_key = site_name)

snotel_cropped$network = "NRCS"
master_list = rbind(snotel_cropped,station_data)

master_times = cbind(master_list, 
                     mean_time_spi = c(best_times_list$spi$mean, best_times_mesonet_list$spi$mean),
                     mean_time_spei = c(best_times_list$spei$mean, best_times_mesonet_list$spei$mean),
                     mean_time_eddi = c(best_times_list$eddi$mean, best_times_mesonet_list$eddi$mean),
                     mean_time_sedi = c(best_times_list$sedi$mean, best_times_mesonet_list$sedi$mean),
                     mean_cor_spi = c(best_cor_list$spi$mean, best_cor_mesonet_list$spi$mean),
                     mean_cor_spei = c(best_cor_list$spei$mean, best_cor_mesonet_list$spei$mean),
                     mean_cor_eddi = c(best_cor_list$eddi$mean, best_cor_mesonet_list$eddi$mean),
                     mean_cor_sedi = c(best_cor_list$sedi$mean, best_cor_mesonet_list$sedi$mean))

master_times_summer = cbind(master_list, 
                     mean_time_spi = c(best_times_list_summer$spi$mean, best_times_mesonet_list_summer$spi$mean),
                     mean_time_spei = c(best_times_list_summer$spei$mean, best_times_mesonet_list_summer$spei$mean),
                     mean_time_eddi = c(best_times_list_summer$eddi$mean, best_times_mesonet_list_summer$eddi$mean),
                     mean_time_sedi = c(best_times_list_summer$sedi$mean, best_times_mesonet_list_summer$sedi$mean),
                     mean_cor_spi = c(best_cor_list_summer$spi$mean, best_cor_mesonet_list_summer$spi$mean),
                     mean_cor_spei = c(best_cor_list_summer$spei$mean, best_cor_mesonet_list_summer$spei$mean),
                     mean_cor_eddi = c(best_cor_list_summer$eddi$mean, best_cor_mesonet_list_summer$eddi$mean),
                     mean_cor_sedi = c(best_cor_list_summer$sedi$mean, best_cor_mesonet_list_summer$sedi$mean))

#write.csv(master_times, "./validation/soil_moisture/summary_data/geospatial_with_best.csv")

states = sf::st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp") %>%
  dplyr::filter(STATE_NAME != "Alaska" & STATE_NAME != "Hawaii")

states_union = sf::st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp") %>%
  dplyr::filter(STATE_NAME != "Alaska" & STATE_NAME != "Hawaii") %>%
  st_union()

static_map = ggplot() + 
  geom_sf(data = states, fill = "transparent")+
  theme_bw(base_size = 16)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(data = master_times, aes(x = longitude, y = latitude), fill = "blue", 
             color = "black", shape = 21, alpha = 0.5, size = 2)+
  xlab("")+
  ylab("")

static_map

ggsave(paste0("./validation/soil_moisture/plots/summary/site_map.png"),
       static_map, width = 10, height = 5, units = "in", dpi = 400)


## example spei map 
spei_map = raster::raster("/home/zhoylman/drought_indicators_data/spei_20190901_60_day.tif") %>%
  rasterToPoints()%>%
  as.data.frame() %>%
  rename(spei = 3)

color_ramp = c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#00008b")

static_spei_map = ggplot() + 
  geom_sf(data = states_union, fill = "transparent", size = 1.5)+
  geom_tile(data = spei_map, aes(x, y, fill = spei)) + 
  theme_bw(base_size = 16)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_gradientn("SPEI", colours = color_ramp, limits = c(-3,3))+
  ggtitle(paste0("Standardized Precipitation Evapotranspiration Index\n 9-1-2019 (60 Day Timescale)"))+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))


static_spei_map

ggsave(paste0("./validation/soil_moisture/plots/summary/spei_map.png"),
       static_spei_map, width = 10, height = 5, units = "in", dpi = 420)



rbPal <- (colorRampPalette(c("darkblue", "blue", "lightblue", "yellow", "orange", "red", "darkred")))

index_names_lower = c("spi", "spei", "eddi", "sedi")

library(cowplot)

#best times saptial

for(i in 1:4){
  data_select = master_times %>%
    dplyr::select(longitude, latitude, paste0("mean_time_",index_names_lower[i]))%>%
    na.omit()
  
  static_map_metric = ggplot() + 
    geom_sf(data = states, fill = "transparent")+
    theme_bw(base_size = 16)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    geom_point(data = data_select , aes(x = longitude, y = latitude,
                                                       fill = get(paste0("mean_time_",index_names_lower[i]))), 
               color = "black", shape = 21, alpha = 1, size = 2)+
    xlab("")+
    ylab("")+
    ggtitle(paste0("Optimal Timescale (", index_names[i], ")"))+
    scale_fill_gradientn("",colours=(rbPal(100)), guide = F)
  
  #function to draw manual ramp
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  
  dummy_plot = ggplot(data = data_select) +
    geom_tile(aes(y=latitude, x=longitude, fill = get(paste0("mean_time_",index_names_lower[i]))), alpha = 1)+
    scale_fill_gradientn("Days",colours=(rbPal(100)))
  
  #draw ramp
  legend <- g_legend(dummy_plot)
  
  #plot final plot
  static_map_metric_inset = 
    ggdraw() +
    draw_plot(static_map_metric) +
    draw_plot(legend, x = .8, y = .2, width = .35, height = .35)
  
  
  ggsave(paste0("./validation/soil_moisture/plots/summary/spatial_",index_names_lower[i],"_map.png"),
         static_map_metric_inset, width = 10, height = 5, units = "in", dpi = 400)  
}

#best times saptial (Summer)

for(i in 1:4){
  data_select = master_times_summer %>%
    dplyr::select(longitude, latitude, paste0("mean_time_",index_names_lower[i]))%>%
    na.omit()
  
  static_map_metric = ggplot() + 
    geom_sf(data = states, fill = "transparent")+
    theme_bw(base_size = 16)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    geom_point(data = data_select , aes(x = longitude, y = latitude,
                                        fill = get(paste0("mean_time_",index_names_lower[i]))), 
               color = "black", shape = 21, alpha = 1, size = 2)+
    xlab("")+
    ylab("")+
    ggtitle(paste0("May - October Optimal Timescale (", index_names[i], ")"))+
    scale_fill_gradientn("",colours=(rbPal(100)), guide = F)
  
  #function to draw manual ramp
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  
  dummy_plot = ggplot(data = data_select) +
    geom_tile(aes(y=latitude, x=longitude, fill = get(paste0("mean_time_",index_names_lower[i]))), alpha = 1)+
    scale_fill_gradientn("Days",colours=(rbPal(100)))
  
  #draw ramp
  legend <- g_legend(dummy_plot)
  
  #plot final plot
  static_map_metric_inset = 
    ggdraw() +
    draw_plot(static_map_metric) +
    draw_plot(legend, x = .8, y = .2, width = .35, height = .35)
  
  
  ggsave(paste0("./validation/soil_moisture/plots/summary/spatial_",index_names_lower[i],"_map_summer.png"),
         static_map_metric_inset, width = 10, height = 5, units = "in", dpi = 400)  
}










## montly all timescales
library(ggplot2)
library(scales)
library(matrixStats)

index_names = c("SPI","SPEI","EDDI","SEDI")

monthly_data_snotel = list(monthly_correlation_matrix_snotel_spi, monthly_correlation_matrix_snotel_spei,
                           monthly_correlation_matrix_snotel_eddi, monthly_correlation_matrix_snotel_sedi)

monthly_data_mesonet = list(monthly_correlation_matrix_mesonet_spi, monthly_correlation_matrix_mesonet_spei,
                            monthly_correlation_matrix_mesonet_eddi, monthly_correlation_matrix_mesonet_sedi)


# d for metrcs
#for(d in 1:length(monthly_data_snotel)){

plot_monthy = function(d){
  timescale_line_plot = list()
    monthly_ls = list()
    # i for sites
    for(t in 1:146){
      for(i in 1:length(monthly_data_snotel[[d]])){
        # t for timescales
        mean_temp = monthly_data_snotel[[d]][[i]][[t]]$mean_soil_moisture
        if(i == 1){
          mean_full = data.frame(mean_temp)
        }
        else{
          mean_full = cbind(mean_full, mean_temp)
        }
      }
      for(i in 1:length(monthly_data_mesonet[[d]])){
        # t for timescales
        mean_temp_mesonet = monthly_data_mesonet[[d]][[i]][[t]]$mean_soil_moisture
        if(i == 1){
          mean_full_mesonet = data.frame(mean_temp_mesonet)
        }
        else{
          mean_full_mesonet = cbind(mean_full_mesonet, mean_temp)
        }
      }
      full = cbind(mean_full, mean_full_mesonet)
      summary = data.frame(rowMedians(as.matrix(full), na.rm = T))
      colnames(summary) = "median"
      monthly_ls[[t]] = summary
    }
    
    monthly_df = as.data.frame(lapply(monthly_ls, cbind))
    best_monthly_time = function(x, direction){
      if(direction == "max"){
        return(c(
        seq(5,730,5)[which(monthly_df[x,] == max(monthly_df[x,]))],
        max(monthly_df[x,])
        ))
      }
      if(direction == "min"){
        return(c(
          seq(5,730,5)[which(monthly_df[x,] == min(monthly_df[x,]))],
          min(monthly_df[x,])
        ))
      }
    }
    
    best_times = data.frame()
    if(d < 3){
      for(i in 1:12){best_times[i,1:2] = best_monthly_time(i, "max")}
    }
    if(d >= 3){
      for(i in 1:12){best_times[i,1:2] = best_monthly_time(i, "min")}
    }
    
    best_times$time = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                 format = "%Y-%m-%d %H:%M")
    
    #plot results
    colfunc <- colorRampPalette(c("darkblue", "blue", "lightblue", "yellow", "orange", "red", "darkred"))
    cols = colfunc(146)
    #y lim definitions
    if(d == 1){
      ylim_vals = c(-0.1,0.8)
      yjust = 0.05
    } 
    if(d == 2){
      ylim_vals = c(-0.1,0.8)
      yjust = 0.05
    } 
    if(d == 3){
      ylim_vals = c(-0.8,0.1)
      yjust = -0.05
    } 
    if(d == 4){
      ylim_vals = c(-0.8,0.1)
      yjust = -0.05
    } 
    
    timescale_line_plot = ggplot()+
      geom_line(data = NULL, aes(x = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                                format = "%Y-%m-%d %H:%M"), y = monthly_df[,1]), color = cols[1]) +
      theme_bw(base_size = 20) + 
      ylab("Correlation (r)")+
      xlab("Month")+
      ggtitle(index_names[d])+
      scale_x_datetime(labels = date_format("%b"),
                       date_breaks = "2 month")+
      ylim(ylim_vals)+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_text(data = best_times, aes(x = time, y = V2+yjust, label = V1))
    
    for(g in 2:length(monthly_ls)){
      temp_data = data.frame(x = as.POSIXct(paste0(as.Date(paste0(1:12,"-01-2018"), format("%m-%d-%Y"))," 00:00"),
                                            format = "%Y-%m-%d %H:%M"),
                             y = monthly_df[,g])
      timescale_line_plot = timescale_line_plot +
        geom_line(data = temp_data, aes(x = x, y = y), color = cols[g])
    }
    return(timescale_line_plot)
}

colfunc <- colorRampPalette(c("darkblue", "blue", "lightblue", "yellow", "orange", "red", "darkred"))


#function to draw manual ramp
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

dummy_plot = ggplot(data = data.frame(x = seq(1:146), y = seq(1:146), z = seq(5,730, 5))) +
  geom_tile(aes(y=y, x=x, fill = z), alpha = 1)+
  scale_fill_gradientn("Days",colours=(colfunc(100)))

#draw ramp
legend <- g_legend(dummy_plot) 


plot_grid_timescales = cowplot::plot_grid(plot_monthy(1),plot_monthy(2), NULL,
                                          plot_monthy(3),plot_monthy(4), NULL, 
                                          nrow = 2, rel_widths = c(1, 1, 0.1,
                                                                   1, 1, 0.1))
library(cowplot)
plot_grid_timescales_inset = 
  ggdraw() +
  draw_plot(plot_grid_timescales) +
  draw_plot(legend, x = .95, y = -0.05, width = .4, height = .4)

ggsave(paste0("./validation/soil_moisture/plots/summary/timescale_lines.png"),
       plot_grid_timescales_inset, width = 14, height = 10, units = "in", dpi = 300)

