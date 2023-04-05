rm(list = ls())
setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')
library(xml2)
library(lubridate)
source('create_inputs_fun.R')
fixed_date = ymd_hms('1900-01-01 00:00:00')


# Parameters --------------------------------------------------------------
# Specify years to run in each core:
year_vec = 2100
scenario = 'GFDL_rcp45' # do NOT change for MIROC or CESM
scenario_true = 'MIROC_rcp45'
# DO NOT forget to change D or E disk below

# Forecast period ---------------------------------------------------------

# The next steps should be done just once. If any main files are changed,
# so run these lines again:

# Create 'Core' folders:

# dir.create(path = paste0('Forecast/', scenario,'/Core_1'))
# dir.create(path = paste0('Forecast/', scenario,'/Core_2'))
# dir.create(path = paste0('Forecast/', scenario,'/Core_3'))
# dir.create(path = 'Hindcast/Core_4')
# dir.create(path = 'Hindcast/Core_5')

n_cores = length(list.dirs(path = paste0('Forecast/', scenario), recursive = FALSE))

# Copy and paste standard files that I need:
# WARNING!!! DO NOT FORGET TO CHANGE THE CANONICAL INITIAL PATH IN ROMS WHEN 
# CHANGING RCP SCENARIO

for(j in 1:n_cores) {
  # Copy and paste ROMS file:
  file.copy(from = file.path('main_files_forecast', 'ROMS.properties'), 
            to = file.path(paste0('Forecast/', scenario, '/Core_', j)), recursive = TRUE)
  # Copy and paste LHS file:
  file.copy(from = file.path('main_files_forecast', 'LHS_Types.xml'), 
            to = file.path(paste0('Forecast/', scenario, '/Core_', j)), recursive = TRUE)
  # Copy and paste LHS parameters file:
  file.copy(from = file.path('main_files_forecast', 'LHS_Parameters.Epijuv1kmHSM.RandomMovement.xml'), 
            to = file.path(paste0('Forecast/', scenario, '/Core_', j)), recursive = TRUE)
}

# DO NOT FORGET TO UPDATE THE PCOD SH IN EACH CORE !!!!!!!!

# Specify when to release eggs
#ini_day_month = c('03-01', '03-08', '03-15', '03-22', '03-29') # month-day
ini_day_month = c('03-01', '03-08', '03-15', '03-22', '03-29') # month-day
ini_numbers = rep(x = 1E+6, times = length(ini_day_month))

# Now copy and paste Model.xml file:
for(i in seq_along(year_vec)) {
  
  sel_year = year_vec[i]
  
  # Create initAtt file csv:
  create_init_locations(ini_dates = paste0(sel_year, '-', ini_day_month), 
                        Path = paste0('Forecast/', scenario, '/Core_', i),
                        ini_numbers = ini_numbers)
  
  # Create folder in corresponding core:
  model_path = paste0('Forecast/', scenario, '/Core_', i, '/', 'Connectivity_files_', sel_year)
  dir.create(path = model_path)
  model_path = paste0('Forecast/', scenario, '/Core_', i, '/', 'Results_files_', sel_year)
  dir.create(path = model_path)
  
  # Read Model file XML:
  model_file = xml2::read_xml(x = file.path('main_files_forecast', "Model_Run_1.xml"))
  # Change Netcdf file name:
  nSkipFiles = floor(((sel_year - 2013 + 7)*52 + 8)/10) # 52 because 52 weeks per year. 10 because 10 time steps per nc file. 8 because 8 weeks from Jan to end Feb
  nDig = floor(log10(nSkipFiles)) + 1
  if(nDig == 2) ind_num = paste0('0', nSkipFiles)
  if(nDig == 3) ind_num = as.character(nSkipFiles)
  ncdfName = paste0("D:\\Bering10K\\", scenario_true,"\\Bering10k_avg_00", ind_num, ".nc")
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[4]
  xml_text(tmp_ind) = ncdfName
  # Change Conectivity folder name:
  conec_name = paste0('Connectivity_files_', sel_year,'\\ConnecFile')
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[1]
  xml_text(tmp_ind) = conec_name
  # Change Results folder name:
  result_name = paste0('Results_files_', sel_year,'\\ResultFile')
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[5]
  xml_text(tmp_ind) = result_name
  # Change Start time:
  ini_time = ymd_hms(paste0(sel_year, '-01-01 12:00:00'))
  time_seconds = as.numeric(ini_time - fixed_date)*86400
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[9]
  xml_text(tmp_ind) = as.character(time_seconds)
  
  # Print new Model file in corresponding folder:
  xml2::write_xml(x = model_file, file = 
                    file.path(paste0('Forecast/', scenario, '/Core_', i), 'Model_1.xml'))

}
