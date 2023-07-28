rm(list = ls())
# ROMS example:
# Explore ROMS outputs:

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
library(stringr)
require(sf)
source('aux_functions.R')
#load('BathyData.RData')
bathy1 = read.csv('main_files_forecast/bathy1.csv')
bathy2 = read.csv('main_files_forecast/bathy2.csv')

# Read bathymetry information ---------------------------------------------

ak = map_data('worldHires','USA:Alaska')
world = ne_countries(scale = "medium", returnclass = "sf")
#surv_Ndays = 100
#surv_Len = 25

# Read output files -------------------------------------------------------
# Here I will read every results file and get the appropiate format to make every figure.
# This approach is better since files will be super large for the forcast period and 1 h time step

# min_plot_year = 2030
# max_plot_year = 2090
# by_plot_year = 20
# x_labs_years = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)
# allYears = 2020:2100
# x_labs = rep('', times = length(allYears))
# x_labs[allYears %in% x_labs_years] = as.character(x_labs_years)
yearBreaks = c(2019, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
decadesLabels = c("2021-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069", 
                  "2070-2079", "2080-2089", "2090-2100")


thLow1 = 0.025
thHigh1 = 0.975
alphaLevel = 0.15
init_number = 1E+06
main_folder = 'E:/DisMELS_save_outputs/save_outputs' # read DisMELS outputs from
save_folder = 'output_data/forecast' # save lists created here to plot
n_releases = 5
min_depth = -1000
n_grids = 206
n_years = 80
n_id = n_grids*n_releases

# Select folders to read:
scenarios = list.files(path = main_folder)
oceanLevels = c('MIROC', 'CESM', 'GFDL')
scenario_order = c('rcp45', 'rcp85')
scenario_order_name = c('RCP4.5', 'RCP8.5')
color_order = c('rcp45_MIROC', 'rcp45_CESM', 'rcp45_GFDL', 'rcp85_MIROC', 'rcp85_CESM', 'rcp85_GFDL')
color_label = c('MIROC (RCP4.5)', 'CESM (RCP4.5)', 'GFDL (RCP4.5)', 'MIROC (RCP8.5)', 'CESM (RCP8.5)', 'GFDL (RCP8.5)')
mainCols = c(brewer.pal(9,"YlGnBu")[c(9,7,5)], brewer.pal(9,"YlOrRd")[c(9,7,5)])

plot_data_0a = list()
plot_data_0b = list()
plot_data_0c = list()
plot_data_0d = list()
plot_data_0e = list()
plot_data_1a = list()
plot_data_1b = list()
plot_data_1c = list()
plot_data_1d = list()
plot_data_2a = list()
plot_data_2b = list()
plot_data_2c = list()
plot_data_2d = list()
plot_data_2e = list()
plot_data_3a = list()
plot_data_3b = list()
plot_data_3c = list()
plot_data_4a = list()
plot_data_4b = list()
plot_data_4c = list()
plot_data_4d = list()
plot_data_4e = list()
plot_data_5a = list()
plot_data_5b = list()
plot_data_5c = list()
plot_data_5d = list()
plot_data_5e = list()
plot_data_6 = list()
plot_data_6b = list()
plot_data_6_5 = list()
plot_data_7 = list()
plot_data_8a = list()
plot_data_8b = list()
plot_data_8c = list()
plot_data_9a = list()
plot_data_9b = list()
plot_data_9c = list()
plot_data_10a = list()
plot_data_10b = list()
plot_data_11a = list()
plot_data_11b = list()
plot_data_12 = list()
plot_data_13 = list()
plot_data_14 = list()
plot_data_15 = list()
plot_data_16 = list()

indList = 1

for(k in seq_along(scenarios)) {

  mod_year = list.files(path = file.path(main_folder, scenarios[k]))
  
    for(j in seq_along(mod_year)) {
      
      # Read all results CSV:
      tmpData = read_data_in(eggInclude = FALSE, 
                             path = file.path(main_folder, scenarios[k], mod_year[j]))
      tmpData$ageYSLround = round(tmpData$ageFromYSL)
      tmpData$md_inx = lubridate::month(x = tmpData$time) + (lubridate::day(x = tmpData$time)/31)
      tmpData$day_month = paste0(lubridate::day(x = tmpData$time), '_', lubridate::month(x = tmpData$time))
      tmpData = tmpData[tmpData$md_inx < 9.5, ] # Max date: Sep 15th
      this_year = unique(tmpData$year)
      
      # Incorporate weighting factor (date) + rel_number:
      # tmpData$dm_init_date = paste0(lubridate::day(x = tmpData$startTime), '-', lubridate::month(x = tmpData$startTime))
      # tmpData$wgt_factor_time = NA
      # tmpData$wgt_factor_time[tmpData$dm_init_date == '1-3'] = 2
      # tmpData$wgt_factor_time[tmpData$dm_init_date == '8-3'] = 6
      # tmpData$wgt_factor_time[tmpData$dm_init_date == '15-3'] = 10
      # tmpData$wgt_factor_time[tmpData$dm_init_date == '22-3'] = 6
      # tmpData$wgt_factor_time[tmpData$dm_init_date == '29-3'] = 2

      # Get name for scenarios:
      scenario_name = str_split(string = scenarios[k], pattern = '_', simplify = TRUE)[1,2]
      ocean_mod_name = str_split(string = scenarios[k], pattern = '_', simplify = TRUE)[1,1]

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
								scenario = scenario_name,
								ocean_mod = ocean_mod_name)
		starv_dead = data.frame(year = this_year, id = baseLocs$id[!(baseLocs$id %in% starv_alive$id)],
								scenario = scenario_name,
								ocean_mod = ocean_mod_name)
		starv_alive$id_grid = baseLocs$id_grid[match(starv_alive$id, baseLocs$id)]
		starv_dead$id_grid = baseLocs$id_grid[match(starv_dead$id, baseLocs$id)]
		
		plot_data_0a[[indList]] = starv_alive
		plot_data_0b[[indList]] = starv_dead
		tmpData2 = tmpData[tmpData$id %in% starv_alive$id, ] # exclude them
		tmpData3 = tmpData2 # future analysis

		# Subset only fish that end up within the EBS:
		end_ind = tmpData[tmpData[ , .I[which.max(time)], by = id]$V1]
		in_data = data.frame(year = this_year, id = end_ind$id[which(end_ind$horizPos1 < 0)],
									scenario = scenario_name,
									ocean_mod = ocean_mod_name)
		this_out = end_ind$id[which(end_ind$horizPos1 > 0)]
		if(length(this_out) > 0) {
  		out_data = data.frame(year = this_year, id = end_ind$id[which(end_ind$horizPos1 > 0)],
  									scenario = scenario_name,
  									ocean_mod = ocean_mod_name)
		} else {
		  out_data = data.frame(year = this_year, id = NA,
		                        scenario = scenario_name,
		                        ocean_mod = ocean_mod_name)
		}
		in_data$id_grid = baseLocs$id_grid[match(in_data$id, baseLocs$id)]
		out_data$id_grid = baseLocs$id_grid[match(out_data$id, baseLocs$id)]
		
		plot_data_0c[[indList]] = in_data
		tmpData2 = tmpData2[tmpData2$id %in% in_data$id, ] # exclude them
		# tmp_dead_ebs = tmpData[!(tmpData$id %in% in_data$id), ]
	  
		# Number of fish alive by Sep 15th:
		alive_id = unique(tmpData2$id)
		alive_data = data.frame(year = this_year, id = alive_id,
								scenario = scenario_name,
								  ocean_mod = ocean_mod_name)
		alive_data$id_grid = baseLocs$id_grid[match(alive_data$id, baseLocs$id)]
		
		# Number of fish dead by Sep 15th:
		dead_id = unique(c(out_data$id[!is.na(out_data$id)], starv_dead$id))
		dead_data = data.frame(year = this_year, id = dead_id,
									scenario = scenario_name,
								  ocean_mod = ocean_mod_name)
		dead_data$id_grid = baseLocs$id_grid[match(dead_data$id, baseLocs$id)]
		
		plot_data_0d[[indList]] = alive_data
		plot_data_0e[[indList]] = dead_data
		# tmp_dead_all = tmpData[!(tmpData$id %in% alive_data$id), ]
	  
      # Find initial and final points (only alive id + out of EBS)
      init_points = tmpData3[tmpData3[ , .I[which.min(time)], by = id]$V1]
      end_points = tmpData3[tmpData3[ , .I[which.max(time)], by = id]$V1]
      
      # Section 1 ----------
      # Hatching success
    tmp_data = tmpData2[tmpData2[ , .I[which.min(time)], by = id]$V1]
    num_dataYSL = tmp_data[,c('year', 'id', 'number')]
    num_dataYSL$init_number = init_number
    num_dataYSL$hatsuc = num_dataYSL$number/num_dataYSL$init_number
    # Prepare data to save:
    sel_var = 'hatsuc'
    toPlotData = num_dataYSL
    toPlotData$scenario = scenario_name
    toPlotData$ocean_mod = ocean_mod_name
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_1d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', sel_var)]
    colnames(prevAnom)[3] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    prevAnom2$scenario = scenario_name
    prevAnom2$ocean_mod = ocean_mod_name
    plot_data_1a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_1b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
    plot_data_1c[[indList]] = fdata
 
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
    toPlotData$scenario = scenario_name
    toPlotData$ocean_mod = ocean_mod_name
    plot_data_2d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', sel_var)]
    colnames(prevAnom)[3] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
	  prevAnom2$scenario = scenario_name
    prevAnom2$ocean_mod = ocean_mod_name
    plot_data_2a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_2b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
    plot_data_2c[[indList]] = fdata
	
    # Section 4 ----------
    # Standard length
    sl_data = aggregate(x = list(SL = tmpData2$SL), list(year = tmpData2$year, id = tmpData2$id), 
                        FUN = max, na.rm=TRUE)
    # Prepare data to save:
    sel_var = 'SL'
    toPlotData = sl_data
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
	  toPlotData$scenario = scenario_name
    toPlotData$ocean_mod = ocean_mod_name
    plot_data_4d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', sel_var)]
    colnames(prevAnom)[3] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    prevAnom2$scenario = scenario_name
    prevAnom2$ocean_mod = ocean_mod_name
    plot_data_4a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_4b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
    plot_data_4c[[indList]] = fdata

    
    # Section 5 ----------
    # DW/DWmax : growth performance
    tmp_data = tmpData2[tmpData2[ , .I[which.max(time)], by = id]$V1]
    tmp_data$Gperf = tmp_data$DW/tmp_data$dwmax
    this_data = tmp_data[,c('year', 'id', 'Gperf')]
    # Prepare data to save:
    sel_var = 'Gperf'
    toPlotData = this_data
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    toPlotData$scenario = scenario_name
    toPlotData$ocean_mod = ocean_mod_name
    plot_data_5d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', sel_var)]
    colnames(prevAnom)[3] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    prevAnom2$scenario = scenario_name
    prevAnom2$ocean_mod = ocean_mod_name
    plot_data_5a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_5b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5)
    plot_data_5c[[indList]] = fdata
    

    # Section 6 ----------
    # Mortality: Sep 15th
    #myData = tmpData2[(tmpData2$ageYSLround <= 100), ]
    myData = tmpData2
    myData2 = myData[(myData$progYSA >= 1 | is.na(myData$progYSA)), ]
    #Calculate fish predator + invertebrate mortality
    myData3 = aggregate(list(mortfish = myData$mortfish, mortinv = myData$mortinv), 
                        list(year = myData$year, id = myData$id), FUN = median) # median?
    myData4 = myData3 %>% 
      gather('type', 'value', -c(year, id))
    #Find prop starvation:
    myData5 = aggregate(list(value = myData2$mortstarv), 
                        list(year = myData2$year, id = myData2$id), 
                        FUN = function(x) sum(x>0)/length(x))
    myData5$value = myData5$value*100 # in %
    myData5$type = 'mortstarv'
    # Merge both data:
    myData6 = bind_rows(myData4, myData5)
    myData6$scenario = scenario_name
    myData6$ocean_mod = ocean_mod_name
    plot_data_6b[[indList]] = myData6
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = myData6
    q50 = toPlotData %>%
      dplyr::group_by(type) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = 0.5))
    p2_5 = toPlotData %>%
      dplyr::group_by(type) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thLow1))
    p97_5 = toPlotData %>%
      dplyr::group_by(type) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thHigh1))
    fdata = data.frame(year = this_year, type = q50$type, q50 = q50$quant, 
                       q5 = p2_5$quant, q95 = p97_5$quant, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
    plot_data_6[[indList]] = fdata

    # Section 6.5 --------
    # Analyze stomach fullness state:
    stomachInfo = tmpData2 %>%
                dplyr::group_by(id) %>% 
                dplyr::summarise(quant = mean(x = stomachFullness))
    stomachInfo$id_grid = baseLocs$id_grid[match(stomachInfo$id, baseLocs$id)]
    # mean_val = mean( x = tmpData2$stomachFullness)
    # q50 = quantile( x = tmpData2$stomachFullness, probs = 0.5)
    # p2_5 = quantile( x = tmpData2$stomachFullness, probs = thLow1)
    # p97_5 = quantile( x = tmpData2$stomachFullness, probs = thHigh1)
    stomachInfo$year = this_year
    stomachInfo$scenario = scenario_name
    stomachInfo$ocean_mod = ocean_mod_name
    plot_data_6_5[[indList]] = stomachInfo  

	  
    # Section 8 ----------
    # Environmental variables (alive + dead)
    stageData = tmpData
    env_data = aggregate(x = list(temperature = stageData$temp, pCO2 = stageData$pCO2val), 
                           list(year = stageData$year, id = stageData$id), 
                           FUN = mean, na.rm=TRUE) # mean values
    int_data = gather(env_data, key = "variable", value = "value", temperature, pCO2)

    # Prepare data to save:
    sel_var = 'value'
    toPlotData = int_data
    toPlotData$state = 'dead'
    toPlotData$state[toPlotData$id %in% alive_data$id] = 'alive'
    toPlotData$scenario = scenario_name
    toPlotData$ocean_mod = ocean_mod_name
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_8a[[indList]] = toPlotData
    prevAnom = data.frame(toPlotData[,c('id_grid', 'variable', 'state', sel_var)])
    #prevAnom = prevAnom[prevAnom$state == 'alive', ] # only alive ID
    prevAnom = prevAnom %>%
      dplyr::group_by(id_grid, variable) %>% 
      dplyr::summarise(value = mean(value), .groups = 'drop')
    prevAnom$year = this_year
    prevAnom$scenario = scenario_name
    prevAnom$ocean_mod = ocean_mod_name
    plot_data_8b[[indList]] = prevAnom
    q50 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = 0.5))
    q2_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thLow1))
    q97_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thHigh1))
    fdata = data.frame(year = this_year, variable = q50$variable, q50 = q50$quant, 
                       q5 = q2_5$quant, q95 = q97_5$quant, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
    plot_data_8c[[indList]] = fdata

    
    # Section 8.5 ----------
    # Environmental variables (alive + dead): depth
    stageData = tmpData
    env_data = aggregate(x = list(value = stageData$vertPos), 
                         list(year = stageData$year, id = stageData$id), 
                         FUN = mean, na.rm=TRUE) # mean values
    env_data$variable = 'depth'
    env_data$state = 'dead'
    env_data$state[env_data$id %in% alive_data$id] = 'alive'
    env_data$scenario = scenario_name
    env_data$ocean_mod = ocean_mod_name
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = env_data
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_15[[indList]] = toPlotData
    
    # Section 8.5 ----------
    # Environmental variables (alive + dead): light
    stageData = tmpData
    env_data = aggregate(x = list(value = stageData$eb), 
                         list(year = stageData$year, id = stageData$id), 
                         FUN = mean, na.rm=TRUE) # mean values
    env_data$variable = 'light'
    env_data$state = 'dead'
    env_data$state[env_data$id %in% alive_data$id] = 'alive'
    env_data$scenario = scenario_name
    env_data$ocean_mod = ocean_mod_name
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = env_data
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_16[[indList]] = toPlotData 
      
    # Section 9 ----------
    # Prey density field
    prey_data = aggregate(x = list(copepods = stageData$copepod,
                                   neocalanus = stageData$neocalanus, 
                                   neocalanusShelf = stageData$neocalanusShelf, 
                                   euphausiids = stageData$euphausiid + stageData$euphausiidShelf), 
                          list(year = stageData$year, id = stageData$id), 
                          FUN = mean, na.rm=TRUE)
    int_data = gather(prey_data, key = "variable", value = "value",
                      copepods, neocalanus, neocalanusShelf, euphausiids)
    int_data$state = 'dead'
    int_data$state[int_data$id %in% alive_data$id] = 'alive'
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = int_data
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    toPlotData$scenario = scenario_name
    toPlotData$ocean_mod = ocean_mod_name
    plot_data_9a[[indList]] = toPlotData
    prevAnom = data.frame(toPlotData[,c('year', 'id_grid', 'variable', 'state', sel_var)])
    #prevAnom = prevAnom[prevAnom$state == 'alive', ] # only alive ID
    prevAnom = prevAnom %>%
      dplyr::group_by(id_grid, variable) %>% 
      dplyr::summarise(value = mean(value), .groups = 'drop')
    prevAnom$year = this_year
    prevAnom$scenario = scenario_name
    prevAnom$ocean_mod = ocean_mod_name
    plot_data_9b[[indList]] = prevAnom
    q50 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = 0.5))
    q2_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thLow1))
    q97_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thHigh1))
    fdata = data.frame(year = this_year, variable = q50$variable, q50 = q50$quant, 
                       q5 = q2_5$quant, q95 = q97_5$quant, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
    plot_data_9c[[indList]] = fdata
	  
      # Section 11 -----------
      # Calculate distance per id:
      mergePoints = rbind(init_points[,c('horizPos1', 'horizPos2', 'id')],
                          end_points[,c('horizPos1', 'horizPos2', 'id')])
      distMat = mergePoints %>%
        group_by(id)%>%
        group_map(~raster::pointDistance(.x[,c('horizPos1', 'horizPos2')], lonlat=TRUE)) %>%
        setNames(unique(sort(mergePoints$id)))
      distVals = unlist(distMat)
      distVals = distVals[distVals > 0 & !is.na(distVals)]
      distValsNm = (distVals/111000)*60 # units: nm
      preDist = data.frame(dist = distValsNm, year = unique(tmpData$year)[1])
      preDist$id = as.numeric(names(distMat))
      # Prepare data to save:
      sel_var = 'dist'
      toPlotData = preDist
      toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
      prevAnom = data.frame(toPlotData[,c('year', 'id_grid', sel_var)])
      colnames(prevAnom)[3] = 'value'
      prevAnom = prevAnom %>%
        group_by(id_grid) %>% 
        summarise(value = mean(value))
      prevAnom$year = this_year
      prevAnom$scenario = scenario_name
      prevAnom$ocean_mod = ocean_mod_name
      p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
      p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
      p97_5 = quantile(x = toPlotData[,sel_var], probs = thHigh1)
      fdata = data.frame(year = this_year, q50 = p50, 
                         q5 = p2_5, q95 = p97_5, scenario = scenario_name,
                         ocean_mod = ocean_mod_name)
      plot_data_11a[[indList]] = fdata
      plot_data_11b[[indList]] = prevAnom
      
      # Section 12 -----------
      # Calculate direction
      direcMat = mergePoints %>%
        group_by(id)%>%
        group_map(~geosphere::bearing(.x[,c('horizPos1', 'horizPos2')])) %>%
        setNames(unique(sort(mergePoints$id)))
      direcVals = unlist(direcMat)
      direcVals = direcVals[!is.na(direcVals)]
      direcValsNm = ifelse(test = direcVals < 0, yes = 360+direcVals, no = direcVals)
      preDirec = data.frame(direc = direcValsNm, year = unique(tmpData$year)[1])
      preDirec$id = as.numeric(names(direcMat))
      sel_var = 'direc'
      toPlotData = preDirec
      toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
      prevAnom = data.frame(toPlotData[,c('year', 'id_grid', sel_var)])
      colnames(prevAnom)[3] = 'value'
      prevAnom = prevAnom %>%
        group_by(id_grid) %>% 
        summarise(value = mean(value))
      prevAnom$year = this_year
      prevAnom$scenario = scenario_name
      prevAnom$ocean_mod = ocean_mod_name
      plot_data_12[[indList]] = prevAnom
      
      # Section 13 -----------
      # Calculate CG and Inertia:
      end_points$horizPos1 = ifelse(test = end_points$horizPos1 > 0, yes = end_points$horizPos1 - 360, 
                                     no = end_points$horizPos1)
      end_points$horizPos1 = end_points$horizPos1 + 360

      spatInfoEnd = cgi(x = end_points$horizPos1, y = end_points$horizPos2)
      plot_data_13[[indList]] = data.frame(CG_x_end = spatInfoEnd$xcg,
                                      CG_y_end = spatInfoEnd$ycg,
                                      I_end = spatInfoEnd$I,
                                      year = this_year,
                                      scenario = scenario_name,
                                      ocean_mod = ocean_mod_name)
      # Section 14 -----------
      # final locations to plot density
      endLocs_df = end_points[,c('id', 'horizPos1', 'horizPos2', 'year')] 
      endLocs_df$scenario = scenario_name
      endLocs_df$ocean_mod = ocean_mod_name
      plot_data_14[[indList]] = endLocs_df  
      
      # Get to next indicator:
      print(indList)
      indList = indList + 1
      
    }
    
}

# Info required to map:
# baseLocs3 = purrr::map_dfr(1:2, ~baseLocs2[,c('id', 'lon', 'lat')])
# baseLocs3$scenario = rep(x = scenarios, each = n_grids)

# Save DF created:
save(plot_data_0a, file = file.path(save_folder, 'plot_data_0a.RData'))
save(plot_data_0b, file = file.path(save_folder, 'plot_data_0b.RData'))
save(plot_data_0c, file = file.path(save_folder, 'plot_data_0c.RData'))
save(plot_data_0d, file = file.path(save_folder, 'plot_data_0d.RData'))
save(plot_data_0e, file = file.path(save_folder, 'plot_data_0e.RData'))
save(plot_data_1a, file = file.path(save_folder, 'plot_data_1a.RData'))
save(plot_data_1b, file = file.path(save_folder, 'plot_data_1b.RData'))
save(plot_data_1c, file = file.path(save_folder, 'plot_data_1c.RData'))
save(plot_data_1d, file = file.path(save_folder, 'plot_data_1d.RData'))
save(plot_data_2a, file = file.path(save_folder, 'plot_data_2a.RData'))
save(plot_data_2b, file = file.path(save_folder, 'plot_data_2b.RData'))
save(plot_data_2c, file = file.path(save_folder, 'plot_data_2c.RData'))
save(plot_data_2d, file = file.path(save_folder, 'plot_data_2d.RData'))
save(plot_data_2e, file = file.path(save_folder, 'plot_data_2e.RData'))
save(plot_data_4a, file = file.path(save_folder, 'plot_data_4a.RData'))
save(plot_data_4b, file = file.path(save_folder, 'plot_data_4b.RData'))
save(plot_data_4c, file = file.path(save_folder, 'plot_data_4c.RData'))
save(plot_data_4d, file = file.path(save_folder, 'plot_data_4d.RData'))
save(plot_data_4e, file = file.path(save_folder, 'plot_data_4e.RData'))
save(plot_data_5a, file = file.path(save_folder, 'plot_data_5a.RData'))
save(plot_data_5b, file = file.path(save_folder, 'plot_data_5b.RData'))
save(plot_data_5c, file = file.path(save_folder, 'plot_data_5c.RData'))
save(plot_data_5d, file = file.path(save_folder, 'plot_data_5d.RData'))
save(plot_data_5e, file = file.path(save_folder, 'plot_data_5e.RData'))
save(plot_data_6, file = file.path(save_folder, 'plot_data_6.RData'))
save(plot_data_6b, file = file.path(save_folder, 'plot_data_6b.RData'))
save(plot_data_6_5, file = file.path(save_folder, 'plot_data_6_5.RData'))
save(plot_data_8a, file = file.path(save_folder, 'plot_data_8a.RData'))
save(plot_data_8b, file = file.path(save_folder, 'plot_data_8b.RData'))
save(plot_data_8c, file = file.path(save_folder, 'plot_data_8c.RData'))
save(plot_data_9a, file = file.path(save_folder, 'plot_data_9a.RData'))
save(plot_data_9b, file = file.path(save_folder, 'plot_data_9b.RData'))
save(plot_data_9c, file = file.path(save_folder, 'plot_data_9c.RData'))
save(plot_data_10a, file = file.path(save_folder, 'plot_data_10a.RData'))
save(plot_data_10b, file = file.path(save_folder, 'plot_data_10b.RData'))
save(plot_data_11a, file = file.path(save_folder, 'plot_data_11a.RData'))
save(plot_data_11b, file = file.path(save_folder, 'plot_data_11b.RData'))
save(plot_data_12, file = file.path(save_folder, 'plot_data_12.RData'))
save(plot_data_13, file = file.path(save_folder, 'plot_data_13.RData'))
save(plot_data_14, file = file.path(save_folder, 'plot_data_14.RData'))
save(plot_data_15, file = file.path(save_folder, 'plot_data_15.RData'))
save(plot_data_16, file = file.path(save_folder, 'plot_data_16.RData'))
save(baseLocs, file = file.path(save_folder, 'baseLocs.RData'))
save(baseLocs2, file = file.path(save_folder, 'baseLocs2.RData'))


# Load data ---------------------------------------------------------------

load(file.path(save_folder, 'plot_data_0a.RData'))
load(file.path(save_folder, 'plot_data_0b.RData'))
load(file.path(save_folder, 'plot_data_0c.RData'))
load(file.path(save_folder, 'plot_data_0d.RData'))
load(file.path(save_folder, 'plot_data_0e.RData'))
load(file.path(save_folder, 'plot_data_1a.RData'))
load(file.path(save_folder, 'plot_data_1b.RData'))
load(file.path(save_folder, 'plot_data_1c.RData'))
load(file.path(save_folder, 'plot_data_1d.RData'))
load(file.path(save_folder, 'plot_data_2a.RData'))
load(file.path(save_folder, 'plot_data_2b.RData'))
load(file.path(save_folder, 'plot_data_2c.RData'))
load(file.path(save_folder, 'plot_data_2d.RData'))
load(file.path(save_folder, 'plot_data_2e.RData'))
load(file.path(save_folder, 'plot_data_4a.RData'))
load(file.path(save_folder, 'plot_data_4b.RData'))
load(file.path(save_folder, 'plot_data_4c.RData'))
load(file.path(save_folder, 'plot_data_4d.RData'))
load(file.path(save_folder, 'plot_data_4e.RData'))
load(file.path(save_folder, 'plot_data_5a.RData'))
load(file.path(save_folder, 'plot_data_5b.RData'))
load(file.path(save_folder, 'plot_data_5c.RData'))
load(file.path(save_folder, 'plot_data_5d.RData'))
load(file.path(save_folder, 'plot_data_5e.RData'))
load(file.path(save_folder, 'plot_data_6.RData'))
load(file.path(save_folder, 'plot_data_6b.RData'))
load(file.path(save_folder, 'plot_data_6_5.RData'))
load(file.path(save_folder, 'plot_data_8a.RData'))
load(file.path(save_folder, 'plot_data_8b.RData'))
load(file.path(save_folder, 'plot_data_8c.RData'))
load(file.path(save_folder, 'plot_data_9a.RData'))
load(file.path(save_folder, 'plot_data_9b.RData'))
load(file.path(save_folder, 'plot_data_9c.RData'))
load(file.path(save_folder, 'plot_data_10a.RData'))
load(file.path(save_folder, 'plot_data_10b.RData'))
load(file.path(save_folder, 'plot_data_11a.RData'))
load(file.path(save_folder, 'plot_data_11b.RData'))
load(file.path(save_folder, 'plot_data_12.RData'))
load(file.path(save_folder, 'plot_data_13.RData'))
load(file.path(save_folder, 'plot_data_14.RData'))
load(file.path(save_folder, 'plot_data_15.RData'))
load(file.path(save_folder, 'plot_data_16.RData'))
load(file.path(save_folder, 'baseLocs.RData'))
load(file.path(save_folder, 'baseLocs2.RData'))


# Analyze results: temporal series ---------------------------------------------------------
# -------------------------------------------------------------------------

# Hatching success + surv prob (Sep 15th) + SL (Sep 15th) ------------------------------

plot_data_1 = bind_rows(plot_data_1d)
plot_data_1$var_type = 'Hatch success'
colnames(plot_data_1)[5] = 'var'
plot_data_1$var = plot_data_1$var # delete after run models again

# plot_data_2 = bind_rows(plot_data_2d)
# plot_data_2$var_type = 'Survival probability'
# colnames(plot_data_2)[3] = 'var'

plot_data_3 = bind_rows(plot_data_4d)
plot_data_3$var_type = 'Standard length (mm)'
colnames(plot_data_3)[3] = 'var'

plot_data_4 = bind_rows(plot_data_5d)
plot_data_4$var_type = 'Growth performance'
colnames(plot_data_4)[3] = 'var'

plot_data = bind_rows(list(plot_data_1, plot_data_3, plot_data_4))
plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)
plot_data$var_type = factor(plot_data$var_type, 
                            levels = c('Hatch success', 'Standard length (mm)', 'Growth performance'))


ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = var, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(var_type ~ scenario, scales = 'free_y')

ggsave(filename = 'figures/fore_biovar.png', width = 190, height = 160, units = 'mm', dpi = 500) 

# plot_1 = ggplot(plot_data, aes(x = decade)) + 
#   geom_boxplot(aes(y = var, fill = new_factor, color = new_factor), 
#                alpha = alphaLevel, outlier.size = 0.6) +
#   theme_bw() +
#   xlab(NULL) +
#   ylab(NULL) +
#   scale_color_manual(values = mainCols) +
#   scale_fill_manual(values = mainCols) +
#   theme(legend.position = 'none', legend.background =element_blank(),
#         strip.background = element_blank()) +
#   guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL)) +
#   facet_grid(var_type ~ scenario, scales = 'free_y') 

# # Make biological variables plot ------------------------------------------
# png(filename = 'figures/fore_biovar.png', width = 190, height = 90, 
#     units = 'mm', res = 500)
# print(plot_1)
# dev.off()


# Plot 5: Mortality by category: -------------------------------------
# plot_data_4 = bind_rows(plot_data_6b)
# plot_data_4$type = factor(plot_data_4$type, levels = c("mortfish", "mortinv", "mortstarv"),
#                           labels = c('Fish predation', 'Invertebrate predation', '% empty stomach'))
# plot_data_4$scenario = factor(plot_data_4$scenario, levels = scenarioLevels, labels = c('RCP4.5', 'RCP8.5'))
# plot_data_4$ocean_mod = factor(plot_data_4$ocean_mod, levels = oceanLevels)
# plot_data_4$decade = cut(plot_data_4$year, breaks = yearBreaks, labels = decadesLabels)
# plot_data_4$new_factor = factor(paste0(plot_data_4$ocean_mod, '_', plot_data_4$scenario), levels = finalFacLevels)
# 
# plot_2 = ggplot(plot_data_4, aes(x = decade)) + 
#   geom_boxplot(aes(y = value, fill = new_factor, color = new_factor), alpha = alphaLevel, 
#                outlier.size = 0.6) +
#   theme_bw() +
#   xlab(NULL) +
#   ylab(NULL) +
#   scale_color_manual(values = mainCols) +
#   scale_fill_manual(values = mainCols) +
#   theme(legend.position = 'none', legend.background =element_blank(),
#         strip.background = element_blank()) +
#   guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL)) +
#   facet_grid(type ~ scenario, scales = 'free_y')

# Make mortality plot ------------------------------------------
# 
# png(filename = 'figures/fore_mort_types.png', width = 190, height = 220, 
#     units = 'mm', res = 500)
# print(plot_2)
# dev.off()
# 

# Plot 7: Dead individuals annual -------------------------------------

# Starvation:
plot_data = bind_rows(plot_data_0a)
plot_data = aggregate(list(porcsurv = plot_data$id), list(year = plot_data$year,
                                                          scenario = plot_data$scenario,
                                                          ocean_mod = plot_data$ocean_mod),
                      FUN = function(x) length(unique(x))/n_id)
plot_data = plot_data %>% 
              dplyr::group_by(year, scenario, ocean_mod) %>%
              dplyr::summarise(porcsurv = mean(x = porcsurv), .groups = 'drop')
plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$type = '% survived starvation'
colnames(plot_data)[4] = 'var'
my_data_a = plot_data

# Num ind out of EBS
plot_data = bind_rows(plot_data_0c)
plot_data = aggregate(list(in_id = plot_data$id), list(year = plot_data$year,
                                                       scenario = plot_data$scenario,
                                                       ocean_mod = plot_data$ocean_mod),
                      FUN = function(x) length(unique(x))/n_id)
plot_data = plot_data %>% 
  dplyr::group_by(year, scenario, ocean_mod) %>%
  dplyr::summarise(in_id = mean(x = in_id), .groups = 'drop')
plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$type = '% remained in the EBS'
colnames(plot_data)[4] = 'var'
my_data_b = plot_data

# Prop all individuals alive
# plot_data = bind_rows(plot_data_0d)
# plot_data = aggregate(list(alive_id = plot_data$id), list(year = plot_data$year,
#                                                           scenario = plot_data$scenario,
#                                                           ocean_mod = plot_data$ocean_mod),
#                       FUN = function(x) length(unique(x))/n_id)
# plot_data = plot_data %>% 
#   dplyr::group_by(year, scenario, ocean_mod) %>%
#   dplyr::summarise(alive_id  = mean(x = alive_id ), .groups = 'drop')
# plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
# plot_data$type = 'All alive'
# colnames(plot_data)[4] = 'var'
# my_data_c = plot_data

my_plot_data = rbind(my_data_a, my_data_b)
my_plot_data$var = my_plot_data$var*100
my_plot_data$type = factor(my_plot_data$type, levels = c("% survived starvation","% remained in the EBS"))

my_plot_data$decade = cut(my_plot_data$year, breaks = yearBreaks, labels = decadesLabels)
my_plot_data$ocean_mod = factor(my_plot_data$ocean_mod, levels = oceanLevels)
my_plot_data$color_factor = paste0(my_plot_data$scenario, '_', my_plot_data$ocean_mod)
my_plot_data$color_factor = factor(my_plot_data$color_factor, levels = color_order,
                                labels = color_label)


ggplot(my_plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = var, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(type ~ scenario, scales = 'free_y')

ggsave(filename = 'figures/fore_dead_ind.png', width = 190, height = 130, units = 'mm', dpi = 500) 

# Plot 11: Environmental variables -------------------------------------------------

# All (alive + dead):
plot_data = bind_rows(plot_data_8a)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$variable = factor(plot_data$variable, levels = c("temperature", 'pCO2'))
plot_data$variable2 = factor(plot_data$variable, labels = c('Temperature~(degree*C)', 'pCO[2]~(mu*atm)'))

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                   labels = color_label)

# Make plot:
ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(variable2 ~ scenario, scales = 'free_y', labeller = my_label_parsed)

ggsave(filename = 'figures/fore_envvar_all.png', width = 190, height = 130, units = 'mm', dpi = 500) 

# Only alive:
plot_data = bind_rows(plot_data_8a)
plot_data = plot_data %>% filter(state == 'alive')
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$variable = factor(plot_data$variable, levels = c("temperature", 'pCO2'))
plot_data$variable2 = factor(plot_data$variable, labels = c('Temperature~(degree*C)', 'pCO[2]~(mu*atm)'))

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

# Make plot:
ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(variable2 ~ scenario, scales = 'free_y', labeller = my_label_parsed)

ggsave(filename = 'figures/fore_envvar_alive.png', width = 190, height = 130, units = 'mm', dpi = 500) 

# Only Dead:
plot_data = bind_rows(plot_data_8a)
plot_data = plot_data %>% filter(state == 'dead')
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$variable = factor(plot_data$variable, levels = c("temperature", 'pCO2'))
plot_data$variable2 = factor(plot_data$variable, labels = c('Temperature~(degree*C)', 'pCO[2]~(mu*atm)'))

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

# Make plot:
ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(variable2 ~ scenario, scales = 'free_y', labeller = my_label_parsed)

ggsave(filename = 'figures/fore_envvar_dead.png', width = 190, height = 130, units = 'mm', dpi = 500) 

# -------------------------------------------------------------------------
# Depth:
plot_dataD = bind_rows(plot_data_15)
plot_data = plot_dataD %>% filter(state == 'dead')
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

plot_t_1 = ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Depth (m)') +
  ylim(c(-300, 0)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(. ~ scenario, scales = 'free_y', labeller = my_label_parsed)

# Light:
plot_dataL = bind_rows(plot_data_16)
plot_data = plot_dataL %>% filter(state == 'dead')
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

plot_t_2 = ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value*1E-15, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(expression(Ambient~irradiance~"("*mu*mol*"."*m^{-2}*"."*s^{-1}*")")) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(. ~ scenario, scales = 'free_y', labeller = my_label_parsed)


# Make plot:
jpeg(filename = 'figures/fore_depth_light.jpg', width = 200, height = 220, 
    units = 'mm', res = 500)
grid.arrange(plot_t_1, plot_t_2, ncol = 1)
dev.off()


# Plot 12: prey density data -------------------------------------------------------

# All:
plot_data = bind_rows(plot_data_9a)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "neocalanusShelf", 
                                                           "neocalanus", "copepods"))
plot_data$variable2 = factor(plot_data$variable, labels = c("Eup~(mg~C/m^3)",
                                                            "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)"))

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

# Make plot:
ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(variable2 ~ scenario, scales = 'free_y', labeller = my_label_parsed)

ggsave(filename = 'figures/fore_prey_all.png', width = 190, height = 190, units = 'mm', dpi = 500) 

# Alive:
plot_data = bind_rows(plot_data_9a)
plot_data = plot_data %>% filter(state == 'alive')
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "neocalanusShelf", 
                                                           "neocalanus", "copepods"))
plot_data$variable2 = factor(plot_data$variable, labels = c("Eup~(mg~C/m^3)",
                                                            "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)"))

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

# Make plot:
ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(variable2 ~ scenario, scales = 'free_y', labeller = my_label_parsed)

ggsave(filename = 'figures/fore_prey_alive.png', width = 190, height = 190, units = 'mm', dpi = 500) 

# Dead:
plot_data = bind_rows(plot_data_9a)
plot_data = plot_data %>% filter(state == 'dead')
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "neocalanusShelf", 
                                                           "neocalanus", "copepods"))
plot_data$variable2 = factor(plot_data$variable, labels = c("Eup~(mg~C/m^3)",
                                                            "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)"))

plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)
plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
plot_data$color_factor = paste0(plot_data$scenario, '_', plot_data$ocean_mod)
plot_data$color_factor = factor(plot_data$color_factor, levels = color_order,
                                labels = color_label)

# Make plot:
ggplot(plot_data, aes(x = decade)) +
  geom_boxplot(aes(y = value, fill = color_factor, color = color_factor),
               alpha = alphaLevel, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  guides(fill=guide_legend(title=NULL,nrow = 1), 
         color=guide_legend(title=NULL,nrow = 1),
         shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = 'top', legend.background =element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(variable2 ~ scenario, scales = 'free_y', labeller = my_label_parsed)

ggsave(filename = 'figures/fore_prey_dead.png', width = 190, height = 190, units = 'mm', dpi = 500) 


# Plot spatiotemporal trends -----------------------------------------------
save_maps = list()
indsave = 1

# Hatch success:
plot_data = bind_rows(plot_data_1a)
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  if(nrow(df) > n_years) slopeMod = coef(mod1)[2] # to have enough data points
  else slopeMod = NA
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

# Make plot
for(i in seq_along(scenario_order)) {

    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]

    this_title = paste0('Hatch success (', scenario_order_name[i], ')')
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = '/year', 
                                        mainTitle = this_title, colLimits = color_limits)
    indsave = indsave + 1

}

# SL
plot_data = bind_rows(plot_data_4a)
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  if(nrow(df) > n_years) slopeMod = coef(mod1)[2] # to have enough data points
  else slopeMod = NA
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

# Make plot
for(i in seq_along(scenario_order)) {

    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]

    this_title = paste0('Standard length (', scenario_order_name[i], ')')
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = 'mm/year', 
                                        mainTitle = this_title, colLimits = color_limits)
    indsave = indsave + 1

}

# Growth performance:
plot_data = bind_rows(plot_data_5a)
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  if(nrow(df) > n_years) slopeMod = coef(mod1)[2] # to have enough data points
  else slopeMod = NA
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

# Make plot
for(i in seq_along(scenario_order)) {
  
  tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
  
  this_title = paste0('Growth performance (', scenario_order_name[i], ')')
  save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = '/year', 
                                      mainTitle = this_title, colLimits = color_limits)
  indsave = indsave + 1
  
}

png(filename = 'figures/fore_biotrend.png', width = 190, height = 190, 
    units = 'mm', res = 500)
do.call("grid.arrange", c(save_maps, ncol = 2))
dev.off()


# Spatiotemporal trends env variables -------------------------------------
# --------------
save_maps = list()
indsave = 1
# Temperature
plot_data = bind_rows(plot_data_8a)
plot_data = plot_data[plot_data$variable == 'temperature', ]
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

# Make plot:
for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    
    this_title = paste0('Temperature (', scenario_order_name[i], ')')
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = expression(degree*C/year), 
                                        mainTitle = this_title, colLimits = color_limits)
    indsave = indsave + 1
  
}

# pCO2
plot_data = bind_rows(plot_data_8a)
plot_data = plot_data[plot_data$variable == 'pCO2', ]
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    
    this_title = paste0('pCO[2]~(' , scenario_order_name[i], ')')
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = expression(mu*atm/year), 
                                        mainTitle = this_title, colLimits = color_limits,
                                        parse = TRUE)
    indsave = indsave + 1
  
}

png(filename = 'figures/fore_envtrend.png', width = 190, height = 130, 
    units = 'mm', res = 500)
do.call("grid.arrange", c(save_maps, ncol = 2))
dev.off()


# Spatiotemporal trends for prey abundance --------------------------------
# -------------------------------
save_maps = list()
indsave = 1
# Copepods
plot_data = bind_rows(plot_data_9a)
plot_data = plot_data[plot_data$variable == 'copepods', ]
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    this_title = paste0('Cop (' , scenario_order_name[i], ')')
    
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = expression(mg~C.m^{-3}/year), 
                                        mainTitle = this_title, colLimits = color_limits)
    indsave = indsave + 1
  
}

# NCaO:
plot_data = bind_rows(plot_data_9a)
plot_data = plot_data[plot_data$variable == 'neocalanus', ]
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    this_title = paste0('NCaO (' , scenario_order_name[i], ')')
    
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = expression(mg~C.m^{-3}/year), 
                                        mainTitle = this_title, colLimits = color_limits)

    indsave = indsave + 1
  
}


# NCaS:
plot_data = bind_rows(plot_data_9a)
plot_data = plot_data[plot_data$variable == 'neocalanusShelf', ]
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    this_title = paste0('NCaS (' , scenario_order_name[i], ')')
    
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = expression(mg~C.m^{-3}/year), 
                                        mainTitle = this_title, colLimits = color_limits)
    indsave = indsave + 1
  
}

# Eup:
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data = plot_data[plot_data$variable == 'euphausiids', ]
mods = dlply(plot_data, c("id_grid", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id_grid, baseLocs$id_grid)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id_grid, baseLocs$id_grid)]
valMax = max(c(abs(min(plot_data$var, na.rm = TRUE)), max(plot_data$var, na.rm = TRUE)))
color_limits = c(-valMax, valMax)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    this_title = paste0('Eup (' , scenario_order_name[i], ')')
    
    save_maps[[indsave]] = plot_map_var(plot_data = tmp_data, legTitle = expression(mg~C.m^{-3}/year), 
                                        mainTitle = this_title, colLimits = color_limits)
    indsave = indsave + 1
    
}

# Make combined plot:
png(filename = 'figures/fore_preytrend.png', width = 190, height = 240, 
    units = 'mm', res = 500)
do.call("grid.arrange", c(save_maps, ncol = 2))
dev.off()


# -------------------------------------------------------------------------
# Map ind dead  -----------------------------------------------------------
save_maps = list()
indsave = 1
# Starvation:
plot_data = bind_rows(plot_data_0a)
plot_data = plot_data %>%
  dplyr::group_by(scenario, ocean_mod) %>%
  count(id)
plot_data$n_years = n_years
plot_data$n_years[plot_data$scenario == 'rcp45' & plot_data$ocean_mod == 'CESM'] = 59
plot_data$id_grid = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
#plot_data = aggregate(list(n = plot_data$n), list(id_grid = plot_data$id_grid), mean)
plot_data$var = plot_data$n/plot_data$n_years
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data = left_join(baseLocs2, plot_data, by = c('id_grid', 'lon', 'lat'))
plot_data$var[which(is.na(plot_data$var))] = 0
plot_data$var = plot_data$var*100
plot_data = plot_data %>%
              group_by(lon, lat, scenario) %>%
              summarise(var = mean(var)) 
color_limits = range(plot_data$var)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    this_title = paste0('% survived starvation (' , scenario_order_name[i], ')')
    
    save_maps[[indsave]] = plot_map_var2(plot_data = tmp_data, legTitle = '%', 
                                        mainTitle = this_title, limits = color_limits)
    indsave = indsave + 1
  
}

# Out of EBS:
plot_data = bind_rows(plot_data_0c)
plot_data = plot_data %>%
  dplyr::group_by(scenario, ocean_mod) %>%
  count(id)
plot_data$n_years = n_years
plot_data$n_years[plot_data$scenario == 'rcp45' & plot_data$ocean_mod == 'CESM'] = 59
plot_data$id_grid = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
#plot_data = aggregate(list(n = plot_data$n), list(id_grid = plot_data$id_grid), mean)
plot_data$var = plot_data$n/plot_data$n_years
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data = left_join(baseLocs2, plot_data, by = c('id_grid', 'lon', 'lat'))
plot_data$var[which(is.na(plot_data$var))] = 0
plot_data$var = plot_data$var*100
plot_data = plot_data %>%
  group_by(lon, lat, scenario) %>%
  summarise(var = mean(var)) 
color_limits = range(plot_data$var)

for(i in seq_along(scenario_order)) {
  
    tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]
    this_title = paste0('% remained in EBS (' , scenario_order_name[i], ')')
    
    save_maps[[indsave]] = plot_map_var2(plot_data = tmp_data, legTitle = '%', 
                                         mainTitle = this_title)
    indsave = indsave + 1
  
}

# Make plot:
png(filename = 'figures/fore_mapdead.png', width = 190, height = 130, 
    units = 'mm', res = 500)
do.call("grid.arrange", c(save_maps, ncol = 2))
dev.off()



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Information to map
shift_value_1 = 0
shift_value_2 = 360

map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
  dplyr::filter(region != "Antarctica")

country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x=long, y = lat, group = group),
                                fill = "gainsboro",
                                color = "gainsboro",
                                size = 0.15)


# Plot spatial indicators -------------------------------------------------
# Distance:

# dist_data = bind_rows(plot_data_11a, .id = "column_label")
# dist_data$q50 = dist_data$q50*1.852 # from nm to km
# dist_data$q5 = dist_data$q5*1.852 # from nm to km
# dist_data$q95 = dist_data$q95*1.852 # from nm to km
# plot_data = dist_data
# plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
# plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
# 
# sp1 = ggplot(plot_data, aes(x = year)) + 
#   #geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
#   #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
#   geom_line(aes(y = q50, colour = scenario, linetype = ocean_mod)) +
#   theme_bw() +
#   xlab('') +
#   ylab('Distance (km)') +
#   scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) +
#   coord_cartesian(ylim = c(0, 1000)) +  
#   scale_color_manual(values = mainCols) +
#   scale_fill_manual(values = mainCols) +
#   theme(legend.position = 'none')
# 
# # Inertia:
# plot_data = bind_rows(plot_data_13, .id = "column_label")
# plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
# plot_data$ocean_mod = factor(plot_data$ocean_mod, levels = oceanLevels)
# 
# # Plot:
# sp2 = ggplot(plot_data, aes(x = year, y = I_end, color = scenario)) + 
#   geom_line(aes(linetype = ocean_mod)) +
#   theme_bw() +
#   xlab('') +
#   ylab('Inertia') +
#   scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) +
#   scale_color_manual(values = mainCols) +
#   theme(legend.position = 'none') 
# 
# png(filename = 'figures/fore_spatialind.png', width = 95, height = 150, 
#     units = 'mm', res = 500)
# grid.arrange(sp1, sp2, nrow = 2)
# dev.off()

# Plot density of final locations -----------------------------------------
plot_data = bind_rows(plot_data_14)
plot_data$decade = cut(plot_data$year, breaks = yearBreaks, labels = decadesLabels)

for(i in seq_along(scenario_order)) {

      tmp_data = plot_data[plot_data$scenario == scenario_order[i], ]

      plot_map_2d_density(plot_data = tmp_data)
      ggsave(filename = paste0('figures/fore_endpoints_', scenario_order[i] ,'.png'),
             device = 'png', width = 190, height = 150, units = 'mm', dpi = 500)

}

