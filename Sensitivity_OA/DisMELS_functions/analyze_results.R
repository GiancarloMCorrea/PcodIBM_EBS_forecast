
# Analyze results ---------------------------------------------------------
rm(list = ls())
setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')
results_folder = 'Sensitivity_OA/save_results'
all_files = list.files(results_folder)

# For age death:
plot_data = list()
for(i in seq_along(all_files)) {
  
  my_data = read.csv(file.path(results_folder, all_files[i]))
  
  plot_data_1 = my_data %>%
    group_by(temperature, food, scenario) %>%
    summarise(var = age[which(state == 0)[1]])
  plot_data_1$type = 'age'

  plot_data[[i]] = plot_data_1

}

plot_data = dplyr::bind_rows(plot_data)
plot_data$scenario[which(plot_data$scenario == 'HighCO2_all')] = 'High CO2'
plot_data$scenario[which(plot_data$scenario == 'LowCO2')] = 'Low CO2'

p1 = ggplot(plot_data, aes(x = food, y = temperature, z = var)) +
  stat_contour(geom = 'polygon', aes(fill = ..level..)) +
  geom_tile(aes(fill = var)) +
  ylab('Temperature (C)') +
  xlab('Prey abundance factor') +
  guides(fill = guide_colorbar(title = 'Age')) +
  theme_bw() +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), na.value = 'gray80') +
  facet_wrap(. ~ factor(scenario))

# For SL:
plot_data = list()
for(i in seq_along(all_files)) {
  
  my_data = read.csv(file.path(results_folder, all_files[i]))
  
  plot_data_1 = my_data %>%
    group_by(temperature, food, scenario) %>%
    summarise(var = max(SL))
  plot_data_1$type = 'SL'
  
  plot_data[[i]] = plot_data_1
  
}

plot_data = dplyr::bind_rows(plot_data)
plot_data$scenario[which(plot_data$scenario == 'HighCO2_all')] = 'High CO2'
plot_data$scenario[which(plot_data$scenario == 'LowCO2')] = 'Low CO2'

p2 = ggplot(plot_data, aes(x = food, y = temperature, z = var)) +
  stat_contour(geom = 'polygon', aes(fill = ..level..)) +
  geom_tile(aes(fill = var)) +
  ylab('Temperature (C)') +
  xlab('Prey abundance factor') +
  guides(fill = guide_colorbar(title = 'Length')) +
  theme_bw() +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), na.value = 'gray80') +
  facet_wrap(. ~ factor(scenario))

png(filename = file.path('Sensitivity_OA', 'OA_impacts.png'), width = 190, height = 170, units = 'mm',
    res = 500)
gridExtra::grid.arrange(p1, p2)
dev.off()

