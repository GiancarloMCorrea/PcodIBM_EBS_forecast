rm(list = ls())
library(ggplot2)
library(dplyr)

main_folder = 'Sensitivity_OA'
# Call aux functions
source(file.path(main_folder, 'DisMELS_functions/zooplankton.R'))
source(file.path(main_folder, 'DisMELS_functions/reactive_distance.R'))
source(file.path(main_folder, 'DisMELS_functions/mortality.R'))
source(file.path(main_folder, 'DisMELS_functions/auxiliary_functions.R'))
source(file.path(main_folder, 'DisMELS_functions/BioenGrowth.R'))
source('aux_functions.R')
colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(42)
# Fixed variables
dt = 86400/6
dtday = dt/86400

# -------------------------------------------------------------------------

# Set scenario
scenario_name = 'LowCO2_HighLight'

# pco2:
fix_pco2 = 500 # High = 1500, Low = 500
# light:
fix_eb = 1 # High = 1, Limited = 1e-03
# Temperature and food:
factor_food = seq(from = 0.1, to = 1, by = 0.1)
fix_temp = seq(from = 1, to = 10, by = 1)

# impacts on Model components
growth_co2_1 = 0.1 # 0.1
growth_co2_2 = 0.15 # 0.15
meta_co2 = 0.1 # 0.1
preyabun_co2 = 0.1  # 0.1
preywgt_co2 = 0.1 # 0.1
pca_co2 = 0.1 # 0.1
  
# Data for simulation:
max_age = 165 # in days
#sel_id = 1 # number of fish
init_SL = 6
fix_depth = 5 # value does not matter
fix_eb2_zero = 0.1
fix_eupha = 5.5 
fix_NaoS = 1.5 
fix_NaoO = 1 
fix_Cop = 4 
fix_eps = 0.0001

# Environmental information:
pCO2val = fix_pco2
eb = fix_eb
depth = fix_depth
ebtwozero = fix_eb2_zero
eps = fix_eps

data_id_out = list()
idx = 1
for(tin in seq_along(fix_temp)) {
  
  for(fin in seq_along(factor_food)) {
  
    # Env information:
    Temp = fix_temp[tin]
    euphausiids_tot = fix_eupha*factor_food[fin]
    neocalanusShelf = fix_NaoS*factor_food[fin]
    neocalanus = fix_NaoO*factor_food[fin]
    copepod = fix_Cop*factor_food[fin]
    
    #Save output:
    save_output = list()
    init_dw = 1.976E-06*(init_SL^2.974)
    save_output[[1]] = data.table::data.table(stage = 1, # 1 = YSL, 2 = > YSL & < 17 mm, 3 = > 17 mm 
                                              age = 0,
                                              state = 1, # 1 = alive, 0 = dead
                                              progYSL = 0,
                                              progPNR = 0,
                                              SL = init_SL,
                                              DW = init_dw, 
                                              stmsta = init_dw*0.06*0.3,
                                              ingested = NA,
                                              dwmax = init_dw, 
                                              stomachFullness = 1,
                                              psurvival = 1,
                                              scenario = scenario_name,
                                              temperature = fix_temp[tin],
                                              food = factor_food[fin])
  
    # Loop over time steps:
    for(t_step in 2:((1/dtday)*max_age)) {
    
      # previous info:
      old_dry_wgt = save_output[[t_step-1]]$DW
      stmsta = save_output[[t_step-1]]$stmsta
      dwmax = save_output[[t_step-1]]$dwmax
      old_std_len = save_output[[t_step-1]]$SL
      psurvival = save_output[[t_step-1]]$psurvival
      
      this_stage = save_output[[t_step-1]]$stage # stage previous time step
      
      if(this_stage == 1) {
        this_ysa = save_output[[t_step-1]]$progYSL + dtday/calculateYSA(Temp)
        this_pnr = save_output[[t_step-1]]$progPNR + dtday/calculatePNR(Temp)
        this_state = 1
        if(this_pnr >= 1) this_state = ifelse(test = this_ysa < 1, yes = 0, no = 1) # dead by reaching PNR
      } else {
        this_ysa = NA
        this_pnr = NA
        this_state = 1
      }
      
      this_age = save_output[[t_step-1]]$age + dtday
  
      # Growth component --------------------------------------------------------
      
      input_vec = c(Temp, old_dry_wgt, dt, dtday, old_std_len, eb, depth, stmsta, 
                    ebtwozero, euphausiids_tot, neocalanusShelf, neocalanus, copepod, 
                    pCO2val, this_age, eps, dwmax)
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
      stmsta = max(0, min(0.06*old_dry_wgt, stmsta + sum_ing))
      gr_mg_fac = min(grDW + meta, stmsta*assi) - meta - activityCost
      dwmax = dwmax + (grDWmax - activityCostmax)
      dry_wgt = old_dry_wgt + gr_mg_fac
      stmsta = max(0, stmsta - ((dry_wgt - old_dry_wgt) + meta)/assi)
      std_len = getL_fromW(dry_wgt, old_std_len)
      
      if(this_stage == 1 & this_ysa < 1) {
        
          dwmax = dwmax + (grDWmax - activityCostmax)
          dry_wgt = dwmax
          stmsta = 0.3*0.06*dry_wgt
          std_len = getL_fromW(dry_wgt, old_std_len)
          stomachFullness = 1
  
      }
  
      
      if(this_stage == 1 & stomachFullness > 0.01 & this_ysa >= 1) this_stage = 2
      if(this_stage == 2 & std_len > 17) this_stage = 3
        
      # Survival component ------------------------------------------------------
      
      mort_out = TotalMortality(old_std_len, eb, ebtwozero, dry_wgt, stomachFullness, dwmax)
      mortfish = mort_out[3]
      mortinv = mort_out[4]
      mortstarv = mort_out[5]
      if(mort_out[2] > 1000) {
        psurvival = 0
        this_state = 0
      } else {
        psurvival = psurvival*exp(-dt*mort_out[1])
      }
    
      # Save at t_step
      save_output[[t_step]] = data.table::data.table(stage = this_stage, # 1 = YSL, 2 = > YSL & < 17 mm, 3 = > 17 mm 
                                                age = this_age,
                                                state = this_state, # 1 = alive, 0 = dead
                                                progYSL = this_ysa,
                                                progPNR = this_pnr,
                                                DW = dry_wgt, 
                                                SL = std_len, 
                                                stmsta = stmsta,
                                                ingested = sum_ing,
                                                dwmax = dwmax, 
                                                stomachFullness = stomachFullness,
                                                psurvival = psurvival,
                                                scenario = scenario_name,
                                                temperature = fix_temp[tin],
                                                food = factor_food[fin])
    
    }
  
  data_id_out[[idx]] = dplyr::bind_rows(save_output)
  print(idx)
  idx = idx + 1

  } # food
  
} # temperature

merged_data = dplyr::bind_rows(data_id_out)
write.csv(merged_data, paste0(main_folder, '/save_results/', scenario_name, '.csv'), row.names = FALSE)


