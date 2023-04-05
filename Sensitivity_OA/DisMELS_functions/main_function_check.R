rm(list = ls())

library(ggplot2)
setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')

main_folder = 'Sensitivity_OA'

# Call aux functions
source(file.path(main_folder, 'DisMELS_functions/zooplankton.R'))
source(file.path(main_folder, 'DisMELS_functions/reactive_distance.R'))
source(file.path(main_folder, 'DisMELS_functions/mortality.R'))
source(file.path(main_folder, 'DisMELS_functions/auxiliary_functions.R'))
source(file.path(main_folder, 'DisMELS_functions/BioenGrowth.R'))
source('aux_functions.R')

# Fixed variables
dt = 86400/6
dtday = dt/86400

# Read data training:
data_all = read_data_in(eggInclude = FALSE, path = 'save_outputs/CESM_rcp85_all_lin/Results_files_2021')
data_all = data_all[data_all$alive, ]
all_id = unique(data_all$id)
sel_id = sample(x = all_id, size = min(1, length(all_id)), replace = FALSE)
sel_id = 761

for(idx in seq_along(sel_id)) {

  data_id = data_all[data_all$id == sel_id[idx]]
  
  #Save output:
  save_output = list(nrow(data_id))
  save_output[[1]] = data.table::data.table(time = lubridate::ymd_hms(data_id$time)[1],
                                            stage = data_id$typeName[1],
                                            DW = data_id$DW[1], SL = data_id$SL[1], stmsta = data_id$stmsta[1],
                                            dwmax = data_id$dwmax[1], 
                                            stomachFullness = data_id$stomachFullness[1],
                                            avgRank = data_id$avgRank[1], psurvival = data_id$psurvival[1],
                                            mortfish = data_id$mortfish[1],
                                            mortinv = data_id$mortinv[1], mortstarv = data_id$mortstarv[1])

  
  # Initial values:
  
  for(t_step in 2:nrow(data_id)) {
  
    # Fix scale problem:
    old_dry_wgt = save_output[[t_step-1]]$DW*1E-03
    stmsta = save_output[[t_step-1]]$stmsta*1E-03
    dwmax = save_output[[t_step-1]]$dwmax *1E-03
    
    old_std_len = save_output[[t_step-1]]$SL # NO scale problem
    psurvival = save_output[[t_step-1]]$psurvival # NO scale problem
    
    Temp = data_id$temp[t_step]
    if(Temp <= 0) Temp = 0.01
    eb = data_id$eb[t_step]*1E-15
    depth = data_id$vertPos[t_step]
    ebtwozero = data_id$ebtwozero[t_step]
    euphausiids_tot = data_id$euphausiid[t_step] + data_id$euphausiidShelf[t_step]
    neocalanusShelf = data_id$neocalanusShelf[t_step]
    neocalanus = data_id$neocalanus[t_step]
    copepod = data_id$copepod[t_step]
    pCO2val = data_id$pCO2val[t_step]
    ageFromYSL = data_id$ageFromYSL[t_step]
    eps = data_id$eps[t_step]*1E-10
    this_time = lubridate::ymd_hms(data_id$time)[t_step]
    this_stage = data_id$typeName[t_step]
    this_ysa = data_id$progYSA[t_step]
    if(is.na(this_ysa)) this_ysa = 2
    
    # Growth component --------------------------------------------------------
    
    input_vec = c(Temp, old_dry_wgt, dt, dtday, old_std_len, eb, depth, stmsta, ebtwozero, euphausiids_tot, neocalanusShelf, neocalanus, copepod, pCO2val, ageFromYSL, eps, dwmax)
    bioEN_output = BioenGrowth(input_vec)
    
    grDW = bioEN_output[1]
    meta = bioEN_output[2]
    sum_ing = bioEN_output[3]
    assi = bioEN_output[4]
    stomachFullness = bioEN_output[5]
    avgRank = bioEN_output[6]
    avgSize = bioEN_output[7]
    metamax = bioEN_output[8]
    grDWmax = bioEN_output[9]
    costRateOfMetabolism = 0.5
    activityCost = 1*meta*costRateOfMetabolism
    activityCostmax = 1*metamax*costRateOfMetabolism
    
    # Update values:
    
    if(this_stage == 'YSL' & this_ysa < 1) {
      
      dwmax = dwmax + (grDWmax - activityCostmax)
      dry_wgt = dwmax
      stmsta = 0.3*0.06*dry_wgt
      std_len = getL_fromW(dry_wgt, old_std_len)
      avgRank = 0
      stomachFullness = 1

    } else {
    
      stmsta = max(0, min(0.06*old_dry_wgt, stmsta + sum_ing))
      gr_mg_fac = min(grDW + meta, stmsta*assi) - meta - activityCost
      dwmax = dwmax + (grDWmax - activityCostmax)
      dry_wgt = old_dry_wgt + gr_mg_fac
      stmsta = max(0, stmsta - ((dry_wgt - old_dry_wgt) + meta)/assi)
      std_len = getL_fromW(dry_wgt, old_std_len)

    }
    
    # Survival component ------------------------------------------------------
    
    mort_out = TotalMortality(old_std_len, eb, ebtwozero, dry_wgt, stomachFullness, dwmax)
    mortfish = mort_out[3]
    mortinv = mort_out[4]
    mortstarv = mort_out[5]
    if(mort_out[2] > 1000) {
      psurvival = 0
      alive = FALSE
      active = FALSE
    } else {
      psurvival = psurvival*exp(-dt*mort_out[1])
    }
  
    # Save at t_step
    save_output[[t_step]] = data.table::data.table(time = this_time, stage = this_stage,
                                                    DW = dry_wgt*1E+03, SL = std_len, stmsta = stmsta*1E+03,
                                                   dwmax = dwmax*1E+03, 
                                                   stomachFullness = stomachFullness, avgRank = avgRank,
                                                   psurvival = psurvival, mortfish = mortfish,
                                                   mortinv = mortinv, mortstarv = mortstarv)
  
  }
  
  
  data_id_out = dplyr::bind_rows(save_output)

  fig1 = ggplot(data_id_out, aes(x = time, y = DW*1E-3, color = stage)) +
          geom_line() +
          geom_line(data = data_id, aes(x = time, y = DW*1E-3, color = typeName), linetype = 2) +
          theme_bw() +
          xlab(NULL) +
          ylab('dry weight (mg)')+
          theme(legend.position = 'none')
  
  fig2 = ggplot(data_id_out, aes(x = time, y = stmsta*1E-3, color = stage)) +
          geom_line() +
          geom_line(data = data_id, aes(x = time, y = stmsta*1E-3, color = typeName), linetype = 2) +
          theme_bw() +
          xlab(NULL) +
          ylab('stomach state (mg)')+
          theme(legend.position = 'none')

  fig3 = ggplot(data_id_out, aes(x = time, y = stomachFullness, color = stage)) +
          geom_line() +
          geom_line(data = data_id, aes(x = time, y = stomachFullness, color = typeName), linetype = 2) +
          theme_bw() +
          xlab(NULL) +
          ylab('stomach fullness')+
          theme(legend.position = 'none')
  
  fig4 = ggplot(data_id_out, aes(x = time, y = psurvival, color = stage)) +
          geom_line() +
          geom_line(data = data_id, aes(x = time, y = psurvival, color = typeName), linetype = 2) +
          theme_bw() +
          xlab(NULL) +
          ylab('survival probability')+
          theme(legend.position = 'none')
  
  fig5 = ggplot(data_id_out, aes(x = time, y = avgRank, color = stage)) +
          geom_line() +
          geom_line(data = data_id, aes(x = time, y = avgRank, color = typeName), linetype = 2) +
          theme_bw() +
          xlab(NULL) +
          ylab('rank diet')+
          theme(legend.position = 'none')
  
  
  png(filename = paste0(main_folder, '/check_model_figs/id_', sel_id[idx], '.png'), width = 190, height = 100, units = 'mm', res = 300)
    gridExtra::grid.arrange(fig1, fig2, fig3, fig4, fig5, nrow = 2)
  dev.off()
  
}
