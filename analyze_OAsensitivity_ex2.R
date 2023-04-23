require(dplyr)
require(ggplot2)
require(data.table)

# Analyze results ---------------------------------------------------------
rm(list = ls())

results_folder = 'Sensitivity_OA/save_results'
all_files = list.files(results_folder)

# For age death:
data_age = list()
for(i in seq_along(all_files)) {
  
  my_data = data.table::fread(file.path(results_folder, all_files[i]))

  plot_data_1 = my_data %>%
    group_by(temperature, food, scenario) %>%
    summarise(var = age[which(state == 0)[1]]) # already take into account both sources of starvation
  plot_data_1$type = 'age'

  data_age[[i]] = plot_data_1

}

plot_data = dplyr::bind_rows(data_age)
plot_data$scenario = factor(plot_data$scenario, levels = c("HighCO2_HighLight", 'HighCO2_LowLight',
                                                           "LowCO2_HighLight", "LowCO2_LowLight"),
                            labels = c('High~pCO[2]~`/`~Light', 'High~pCO[2]~`/`~Light~Limited',
                                       'Low~pCO[2]~`/`~Light', 'Low~pCO[2]~`/`~Light~Limited'))

p1 = ggplot(plot_data, aes(x = food, y = temperature, z = var)) +
  stat_contour(geom = 'polygon', aes(fill = ..level..)) +
  geom_tile(aes(fill = var)) +
  ylab(expression(Temperature~(degree*C))) +
  xlab('Prey abundance factor') +
  guides(fill = guide_colorbar(title = 'Age\n(days)')) +
  scale_y_continuous(breaks=seq(from = 1, to = 10, by = 1), labels = seq(from = 1, to = 10, by = 1)) +
  scale_x_continuous(breaks=seq(from = 0.1, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1)) +
  theme_bw() +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), na.value = 'gray80') +
  facet_wrap(. ~ scenario, labeller = label_parsed, ncol = 2)

png(filename = file.path('figures', 'OA_impacts_age.png'), width = 190, height = 170, units = 'mm',
    res = 500)
print(p1)
dev.off()

# -------------------------------------------------------------------------

# For SL:
data_SL = list()
for(i in seq_along(all_files)) {
  
  my_data = read.csv(file.path(results_folder, all_files[i]))

  plot_data_1 = my_data %>%
    group_by(temperature, food, scenario) %>%
    summarise(var = max(SL))
  plot_data_1$type = 'SL'
  plot_data_1$var[which(!is.na(data_age[[i]]$var))] = NA
  
  data_SL[[i]] = plot_data_1
  
}

plot_data = dplyr::bind_rows(data_SL)
plot_data$scenario = factor(plot_data$scenario, levels = c("HighCO2_HighLight", 'HighCO2_LowLight',
                                                             "LowCO2_HighLight", "LowCO2_LowLight"),
                             labels = c('High~pCO[2]~`/`~Light', 'High~pCO[2]~`/`~Light~Limited',
                                        'Low~pCO[2]~`/`~Light', 'Low~pCO[2]~`/`~Light~Limited'))

p2 = ggplot(plot_data, aes(x = food, y = temperature, z = var)) +
  stat_contour(geom = 'polygon', aes(fill = ..level..)) +
  geom_tile(aes(fill = var)) +
  ylab(expression(Temperature~(degree*C))) +
  xlab('Prey abundance factor') +
  guides(fill = guide_colorbar(title = 'Length\n(mm)')) +
  scale_y_continuous(breaks=seq(from = 1, to = 10, by = 1), labels = seq(from = 1, to = 10, by = 1)) +
  scale_x_continuous(breaks=seq(from = 0.1, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1)) +
  theme_bw() +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), na.value = 'gray80') +
  facet_wrap(. ~ scenario, labeller = label_parsed)

png(filename = file.path('figures', 'OA_impacts_SL.png'), width = 190, height = 170, units = 'mm',
    res = 500)
print(p2)
dev.off()

