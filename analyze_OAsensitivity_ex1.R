rm(list = ls())
# ROMS example:
# Explore ROMS outputs:

setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
require(raster)
library(gapminder)
library(ggplot2)
library(wesanderson)
library(magick)
require(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
require(data.table)
library(mapdata)
library(marmap)
library(tidyverse)
require(mapproj)
require(reshape2)
library(gganimate)
require(lubridate)
require(scales)
require(rnaturalearth)
require(rnaturalearthdata)
library(gridExtra)
require(sf)
source('aux_functions.R')
#load('BathyData.RData')
bathy1 = read.csv('main_files_hindcast/bathy1.csv')
bathy2 = read.csv('main_files_hindcast/bathy2.csv')

# Read bathymetry information ---------------------------------------------

ak = map_data('worldHires','USA:Alaska')
world = ne_countries(scale = "medium", returnclass = "sf")
ini_number = 8E+6 # check this number in initial data file csv. 
#surv_Ndays = 100
#surv_Len = 25

# Read output files -------------------------------------------------------
# Here I will read every results file and get the appropiate format to make every figure.
# This approach is better since files will be super large for the forcast period and 1 h time step

thLow1 = 0.025
thHigh1 = 0.975
thLow2 = 0.25
thHigh2 = 0.75
alphaLevel = 0.2
main_folder = 'save_sensitivity_oa' # read DisMELS outputs from
path_to_files = 'E:/DisMELS_save_outputs'
save_folder = 'output_data/sensitivity' # save lists created here to plot
n_releases = 5
min_depth = -1000
n_grids = 206
n_id = n_grids*n_releases

# Select folders to read:
scenarios = list.files(file.path(path_to_files, main_folder))
#scenarioLevels = c("GFDL_rcp85_all_exp", "GFDL_rcp45_all_exp")
#mainPal = 'Set1'
source_level_order = c("all", 'growth', 'preyab', 'preywgt', 'pca', 'meta', 'none')
source_level_label = c('All', 'Growth rate', 'Prey abundance', 'Prey weight', 'PCS', 'Metabolism', 'None')
oceanLevels = c('MIROC', 'CESM', 'GFDL')
scenario_order = c('rcp45', 'rcp85')
color_order = c('rcp45_MIROC', 'rcp45_GFDL', 'rcp85_MIROC', 'rcp85_CESM', 'rcp85_GFDL')
color_label = c('MIROC (RCP4.5)', 'GFDL (RCP4.5)', 'MIROC (RCP8.5)', 'CESM (RCP8.5)', 'GFDL (RCP8.5)')
mainCols = c(brewer.pal(9,"YlGnBu")[c(9,5)], brewer.pal(9,"YlOrRd")[c(9,7,5)])

plot_data_0a = list()
plot_data_0b = list()
plot_data_0c = list()
plot_data_0d = list()
plot_data_0e = list()
plot_data_2d = list()
plot_data_4d = list()
plot_data_5d = list()

indList = 1

for(k in seq_along(scenarios)) {

  mod_year = list.files(path = file.path(path_to_files, main_folder, scenarios[k]))
  
    for(j in seq_along(mod_year)) {
      
      # Read all results CSV:
      tmpData = read_data_in(eggInclude = FALSE, 
                             path = file.path(path_to_files, main_folder, scenarios[k], mod_year[j]))
      tmpData$ageYSLround = round(tmpData$ageFromYSL)
      tmpData$md_inx = lubridate::month(x = tmpData$time) + (lubridate::day(x = tmpData$time)/31)
      tmpData$day_month = paste0(lubridate::day(x = tmpData$time), '_', lubridate::month(x = tmpData$time))
      tmpData = tmpData[tmpData$md_inx < 9.5, ] # Max date: Sep 15th
      this_year = unique(tmpData$year)
      
      # Base initial points: (all fish) Just do it once
      if(k & j == 1) {
        base_locs = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
        base_locs$horizPos1 = ifelse(test = base_locs$horizPos1 > 0, yes = base_locs$horizPos1 - 360, 
                                     no = base_locs$horizPos1)
        base_locs$horizPos1 = base_locs$horizPos1 + 360
        base_locs = base_locs[order(base_locs$id), ]
        baseLocs = base_locs[,c('id', 'horizPos1', 'horizPos2', 'vertPos')]
        baseLocs$id_grid = rep(x = 1:n_grids, times = n_releases)
        baseLocs2 = aggregate(list(lon = baseLocs$horizPos1, lat = baseLocs$horizPos2, 
                                   depth = baseLocs$vertPos), 
                              list(id_grid = baseLocs$id_grid), unique)
      }
      
      # Section 0 ----------
      # Subset only fish passed to FDL
      fdl_in = unique(tmpData[tmpData$typeName == 'FDL', 'id'])
      dw_out = tmpData[which(tmpData$mortstarv >= 1000),]
      dw_in = base_locs$id[!(base_locs$id %in% unique(dw_out$id))]    
      
      starv_alive = data.frame(year = this_year, id = intersect(fdl_in$id, dw_in),
                               scenario = scenarios[k])
      starv_dead = data.frame(year = this_year, id = baseLocs$id[!(baseLocs$id %in% starv_alive$id)],
                              scenario = scenarios[k])
      starv_alive$id_grid = baseLocs$id_grid[match(starv_alive$id, baseLocs$id)]
      starv_dead$id_grid = baseLocs$id_grid[match(starv_dead$id, baseLocs$id)]
      
      plot_data_0a[[indList]] = starv_alive
      plot_data_0b[[indList]] = starv_dead
      tmpData2 = tmpData[tmpData$id %in% starv_alive$id, ] # exclude them
      tmpData3 = tmpData2 # future analysis
      
      # Subset only fish that end up within the EBS:
      end_ind = tmpData[tmpData[ , .I[which.max(time)], by = id]$V1]
      in_data = data.frame(year = this_year, id = end_ind$id[which(end_ind$horizPos1 < 0)],
                           scenario = scenarios[k])
      this_out = end_ind$id[which(end_ind$horizPos1 > 0)]
      if(length(this_out) > 0) {
        out_data = data.frame(year = this_year, id = end_ind$id[which(end_ind$horizPos1 > 0)],
                              scenario = scenarios[k])
      } else {
        out_data = data.frame(year = this_year, id = NA,
                              scenario = scenarios[k])
      }
      in_data$id_grid = baseLocs$id_grid[match(in_data$id, baseLocs$id)]
      out_data$id_grid = baseLocs$id_grid[match(out_data$id, baseLocs$id)]
      
      plot_data_0c[[indList]] = in_data
      tmpData2 = tmpData2[tmpData2$id %in% in_data$id, ] # exclude them
      
      # Number of fish alive by Oct 1st:
      alive_id = unique(tmpData2$id)
      alive_data = data.frame(year = this_year, id = alive_id,
                              scenario = scenarios[k])
      alive_data$id_grid = baseLocs$id_grid[match(alive_data$id, baseLocs$id)]
      
      # Number of fish dead by Sep 15th:
      dead_id = unique(c(out_data$id[!is.na(out_data$id)], starv_dead$id))
      dead_data = data.frame(year = this_year, id = dead_id,
                             scenario = scenarios[k])
      dead_data$id_grid = baseLocs$id_grid[match(dead_data$id, baseLocs$id)]
      
      plot_data_0d[[indList]] = alive_data
      plot_data_0e[[indList]] = dead_data
      
      # Find initial and final points (only alive id + out of EBS)
      init_points = tmpData3[tmpData3[ , .I[which.min(time)], by = id]$V1]
      end_points = tmpData3[tmpData3[ , .I[which.max(time)], by = id]$V1]
      

      # Section 2 ----------
      # Survival by Sep 15th 
      #tmp_data = tmpData2[tmpData2$ageYSLround == 100, ]
      tmp_data = tmpData2[tmpData2[ , .I[which.max(time)], by = id]$V1]
      survData = aggregate(x = list(psurv = tmp_data$psurvival), list(year = tmp_data$year, 
                                                                      id = tmp_data$id),
                           FUN = mean, na.rm = TRUE)
      survData$psurv = survData$psurv*1e-06
      surv_data = survData[,c('year', 'id', 'psurv')]
      # Prepare data to save:
      sel_var = 'psurv'
      toPlotData = surv_data
      toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
      toPlotData$scenario = scenarios[k]
      plot_data_2d[[indList]] = toPlotData

      # Section 4 ----------
      # Standard length
      sl_data = aggregate(x = list(SL = tmpData2$SL), list(year = tmpData2$year, id = tmpData2$id), 
                          FUN = max, na.rm=TRUE)
      # Prepare data to save:
      sel_var = 'SL'
      toPlotData = sl_data
      toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
      toPlotData$scenario = scenarios[k]
      plot_data_4d[[indList]] = toPlotData

      # Section 5 ----------
      # DW/DWmax : growth performance
      tmp_data = tmpData2[tmpData2[ , .I[which.max(time)], by = id]$V1]
      tmp_data$Gperf = tmp_data$DW/tmp_data$dwmax
      this_data = tmp_data[,c('year', 'id', 'Gperf')]
      # Prepare data to save:
      sel_var = 'Gperf'
      toPlotData = this_data
      toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
      toPlotData$scenario = scenarios[k]
      plot_data_5d[[indList]] = toPlotData
      
      # Get to next indicator:
      print(indList)
      indList = indList + 1
      
    }
    
}


# Save DF created:
save(plot_data_0a, file = file.path(save_folder, 'plot_data_0a.RData'))
save(plot_data_0b, file = file.path(save_folder, 'plot_data_0b.RData'))
save(plot_data_0c, file = file.path(save_folder, 'plot_data_0c.RData'))
save(plot_data_0d, file = file.path(save_folder, 'plot_data_0d.RData'))
save(plot_data_0e, file = file.path(save_folder, 'plot_data_0e.RData'))
save(plot_data_2d, file = file.path(save_folder, 'plot_data_2d.RData'))
save(plot_data_4d, file = file.path(save_folder, 'plot_data_4d.RData'))
save(plot_data_5d, file = file.path(save_folder, 'plot_data_5d.RData'))
#save(baseLocs, file = file.path(save_folder, 'baseLocs.RData'))


# Read saved files --------------------------------------------------------

load(file.path(save_folder, 'plot_data_0a.RData'))
load(file.path(save_folder, 'plot_data_0b.RData'))
load(file.path(save_folder, 'plot_data_0c.RData'))
load(file.path(save_folder, 'plot_data_0d.RData'))
load(file.path(save_folder, 'plot_data_0e.RData'))
load(file.path(save_folder, 'plot_data_2d.RData'))
load(file.path(save_folder, 'plot_data_4d.RData'))
load(file.path(save_folder, 'plot_data_5d.RData'))


# Analyze results: temporal series ---------------------------------------------------------
# -------------------------------------------------------------------------

# Biological variables --------------------------------------------------------

# For standard length:
plot_data_2 = bind_rows(plot_data_4d)
plot_data_2$variable = 'Standard length (mm)'
colnames(plot_data_2)[3] = 'var'
# For growth performance:
plot_data_3 = bind_rows(plot_data_5d)
plot_data_3$variable = 'Growth performance'
colnames(plot_data_3)[3] = 'var'

# Merge both datasets
plot_data = rbind(plot_data_2, plot_data_3)

# Create new variables:
plot_data$emission_scenario = sapply(X = stringr::str_split(string = plot_data$scenario, pattern = '_'), FUN = '[', 1)
plot_data$ocean_mod = sapply(X = stringr::str_split(string = plot_data$scenario, pattern = '_'), FUN = '[', 2)
plot_data$effect = sapply(X = stringr::str_split(string = plot_data$scenario, pattern = '_'), FUN = '[', 3)
plot_data$color_factor = paste0(plot_data$emission_scenario, '_', plot_data$ocean_mod)

# Order factors
plot_data$effect = factor(plot_data$effect, levels = source_level_order, 
                            labels = source_level_label)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$emission_scenario = factor(plot_data$emission_scenario, levels = scenario_order)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)
plot_data$variable = factor(plot_data$variable, levels = c('Standard length (mm)', 'Growth performance')) 

# Make plot:
ggplot(plot_data, aes(x = effect)) +
  geom_boxplot( aes(y = var, fill = color_factor, color = color_factor), 
                alpha = 0.2, outlier.size = 0.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL)) +
  facet_grid(variable ~ emission_scenario, scales = 'free_y') +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_blank(),
        legend.position="top",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols)  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

ggsave(filename = 'figures/fore_sensitivity_1.png', width = 190, height = 130, units = 'mm', dpi = 500) 

# Ind survived ------------------------------------------------------------

# % Starvation:
plot_data = bind_rows(plot_data_0a)
plot_data = aggregate(list(porcsurv = plot_data$id), list(year = plot_data$year,
                                                          scenario = plot_data$scenario),
                      FUN = function(x) length(unique(x))/n_id)
plot_data = plot_data %>% 
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(porcsurv = mean(x = porcsurv), .groups = 'drop')
plot_data$type = '% survived starvation'
colnames(plot_data)[3] = 'var'
my_data_a = plot_data

# % out of EBS
plot_data = bind_rows(plot_data_0c)
plot_data = aggregate(list(in_id = plot_data$id), list(year = plot_data$year,
                                                       scenario = plot_data$scenario),
                      FUN = function(x) length(unique(x))/n_id)
plot_data = plot_data %>% 
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(in_id = mean(x = in_id), .groups = 'drop')
plot_data$type = '% remained in the EBS'
colnames(plot_data)[3] = 'var'
my_data_b = plot_data

# Merge datasets:
my_plot_data = rbind(my_data_a, my_data_b)
my_plot_data$var = my_plot_data$var*100
my_plot_data$emission_scenario = sapply(X = stringr::str_split(string = my_plot_data$scenario, pattern = '_'), FUN = '[', 1)
my_plot_data$ocean_mod = sapply(X = stringr::str_split(string = my_plot_data$scenario, pattern = '_'), FUN = '[', 2)
my_plot_data$effect = sapply(X = stringr::str_split(string = my_plot_data$scenario, pattern = '_'), FUN = '[', 3)
my_plot_data$color_factor = paste0(my_plot_data$emission_scenario, '_', my_plot_data$ocean_mod)

# Create new factors:
my_plot_data$type = factor(my_plot_data$type, levels = c("% survived starvation","% remained in the EBS"))
my_plot_data$effect = factor(my_plot_data$effect, levels = source_level_order, 
                          labels = source_level_label)
my_plot_data$ocean_mod = factor(my_plot_data$ocean_mod, levels = oceanLevels)
my_plot_data$color_factor = factor(my_plot_data$color_factor, levels = color_order,
                                labels = color_label)
my_plot_data$emission_scenario = factor(my_plot_data$emission_scenario, levels = scenario_order)

# Make plot:
ggplot(my_plot_data, aes(x = effect)) +
  geom_boxplot( aes(y = var, fill = color_factor, color = color_factor), 
                alpha = 0.2, outlier.size = 0.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL)) +
  facet_grid(type ~ emission_scenario, scales = 'free_y') +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_blank(),
        legend.position="top",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols)  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

ggsave(filename = 'figures/fore_sensitivity_2.png', width = 190, height = 130, units = 'mm', dpi = 500) 

# ggsave(filename = 'figures/fore_sensitivity.png', device = 'png', width = 190, height = 130, units = 'mm', dpi = 500)
# 
# # KS test:
# fac1 = unique(plot_data$year)
# fac2 = unique(plot_data$variable)
# 
# plot_data$source_funtype = paste0(plot_data$source, '_', plot_data$funtype)
# fac3 = unique(plot_data$source_funtype)
# 
# out_data = NULL
# for(i in seq_along(fac1)) {
#   
#   for(j in seq_along(fac2)) {
#     
#     temp = plot_data[plot_data$year == fac1[i] & plot_data$variable == fac2[j], ]
#     
#     for(k in seq_along(fac3)) {
#       
#       if(k < 14) {
#       
#         if(k > 1) {
#           
#           temp_2 = temp[!(temp$source_funtype %in% fac3[1:(k-1)]), ]
#         
#         } else {
#           
#           temp_2 = temp
#           
#         }
#         
#         fac_temp = unique(temp_2$source_funtype)
#         
#         for(l in 1:(length(fac_temp)-1)) {
#           
#           ksres = ks.test(x = temp_2[temp_2$source_funtype == fac_temp[l], 'value'],
#                           y = temp_2[temp_2$source_funtype == fac_temp[l+1], 'value'])
#           temp_out = data.frame(regime = fac1[i], variable = fac2[j], 
#                                 comb1 = fac_temp[l], comb2 = fac_temp[l+1],
#                                 pvalue = ksres$p.value)
#           out_data = rbind(out_data, temp_out)
#           
#         }
#         
#       }
#       
#     }
#     
#   }
#   
# }
# 
# which(out_data$pvalue < 0.05)

