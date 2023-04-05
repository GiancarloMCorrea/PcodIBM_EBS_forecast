# ROMS example:
# Explore ROMS outputs:

#setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')
source('aux_functions.R')
require(ggplot2)

# figure impacts of climate change  ------------------------------------------------------------------
min_pco2 = 500
max_pco2 = 1500
#par_eq = 0.005
xVals = seq(from = min_pco2, to = max_pco2, by = 10)
#maxval = 665
# fac_fun = function(x) {
#   xa = (1/maxval)*(exp(x*par_eq) - 1)
#   return(xa)
# }

# out_df = data.frame(pco2 = xVals)
# out_df$fac = fac_fun(x = out_df$pco2)
# out_df$type = 'Resilient'

tmp2 = data.frame(pco2 = xVals)
tmp2$fac = -0.5 + (1/(max_pco2 - min_pco2))*xVals
#tmp2$type = 'Non-resilient'

tmp3 = data.frame(pco2 = seq(0,min_pco2, by = 100))
tmp3$fac = 0

tmp4 = data.frame(pco2 = seq(max_pco2, 2000, by = 100))
tmp4$fac = 1

# Make plot:

ggplot(data = tmp2, aes(x = pco2, y = fac)) +
  geom_line() +
  geom_line(data = tmp3, aes(x = pco2, y = fac)) +
  geom_line(data = tmp4, aes(x = pco2, y = fac)) +
  theme_bw() +
  scale_y_continuous(breaks = c(0,1), labels = c('0', expression(gamma))) +
  ylab('Increase/decrease in model component') +
  xlab(expression(pCO["2"]*' ('*mu*'atm)')) +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"))
  

ggsave(filename = 'figures/impacts_OA.jpg', device = jpeg, width = 95, 
       height = 90, units = 'mm', dpi = 500)


# -------------------------------------------------------------------------
# Sources of mortality:
m2mm = 1000
getr_fun = function(p_len) {
  
  prey_len = p_len 
  contrast = 0.3
  em = 5E4
  larvalShape = 0.2
  larvalWidth   = larvalShape*prey_len
  prey_area = 0.75*prey_len*larvalWidth 
  ke_larvae = 1
  beamAttCoeff = 3
  eb = 10
  
  visual = 0
  c = beamAttCoeff/m2mm
  c0 = contrast
  ap = prey_area
  vc = em
  ke = 1
  ier = 0
  
  r2 = abs(c0)*ap*vc*(eb/(ke+eb))
  r = sqrt(r2)
  # beamAttCoeff = 0.18*3
  
  eps = 0.0001
  iend = 200
  tol = r
  fr2 = log(abs(c0)*ap*vc)
  fr1 = log(((ke+eb)/eb)*r*r*exp(c*r))
  f1 = fr1 - fr2
  fder = c + 2/r
  
  tolf = 100*eps
  ier = 1
  
  for(i in 1:iend){
    
    if(f1 != 0) {
      
      if(fder != 0) {
        dx = f1/fder
        r = r-dx
        if(r < 0) {
          ier = 3
          break
        }
        tol = r
        
        fr2 = log(abs(c0)*ap*vc)
        fr1 = log(((ke+eb)/eb)*r*r*exp(c*r))
        f1 = fr1 - fr2
        fder = c + 2/r
        tol = eps
        as = abs(r)
        
        if(as-1 <= 0) {
          if(abs(dx)-tol <= 0){
            if(abs(f1)-tolf <= 0) {
              ier = 0
              break
            } 
          }
          
        } else {
          tol = tol*as
        }
        
      } else { 
        ier = 2
        break
      }
      
    } else { 
      
      ier = 0
      break
      
    }
    
  }
  
  return(r)
  
}

len_vec = seq(from = 5, to = 30, by = 0.1)
setMort = 1 # 0.001
visFieldShape = 0.5
fishSwimVel = 0.1
fishDens = 1e-4
aPred = 2.77*10^-6 #1.78e-5
bPred = -1.3 #-1.3
starvMort = 1e-6 
starved = 1
kval = 7e-6

fishMortality = numeric(length(len_vec))
invertebrateMortality = numeric(length(len_vec))
starvationMortality = numeric(length(len_vec))
for(k in seq_along(len_vec)) {

  Pe = 0.92/(1+exp((len_vec[k]-16.3)/4.13))

  larval_mm = len_vec[k]
  visual = getr_fun(larval_mm)

  #fishMortality[k] = setMort*(visFieldShape*pi*(visual^2)*fishDens)
  fishMortality[k] = kval*(Pe)*(visual/1000)^2 
  invertebrateMortality[k] = setMort*((aPred*(larval_mm)^bPred))
  starvationMortality[k] = starvMort*starved

}

data_plot = rbind(data.frame(len = len_vec, mort = fishMortality, type = 'Visual predators'),
                  data.frame(len = len_vec, mort = invertebrateMortality, type = 'Invertebrates')
                  #data.frame(len = len_vec, mort = starvationMortality, type = 'Starvation')
                  )
data_plot$mort = data_plot$mort*1e6

# Make Plot:
ggplot(data = data_plot, aes(x = len, y = mort, color = type)) +
  geom_line() +
  theme_bw() +
  xlab('Standard length (mm)') +
  ylab(expression(paste('Mortality (', s^{-1}, ', ', 10^{-6}, ')'))) +
  theme(legend.position = c(0.77, 0.85),
        legend.background = element_rect(fill='transparent')) +
  guides(color = guide_legend(title = 'Source of mortality'))

ggsave(filename = 'figures/source_mort.png', device = 'png', width = 100, height = 90, units = 'mm', dpi = 500)

# -------------------------------------------------------------------------
# Plot trajectories 3D two particles:

main_folder = 'E:/DisMELS_save_outputs/save_hindcast' # directory where the DisMELS outputs are
mod_year = list.files(path = file.path(main_folder))
j = 1
tmpData = read_data_in(eggInclude = FALSE, path = file.path(main_folder, mod_year[j]))
tmpData$ageYSLround = round(tmpData$ageFromYSL)
explore_plot_3D(data = tmpData)



