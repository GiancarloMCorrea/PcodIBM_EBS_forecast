BioenGrowth = function(vals) {
  
  temp = vals[1]
  mass = vals[2]
  dt = vals[3]
  dtday = vals[4]
  std_len = vals[5]
  eb = vals[6]
  depth = vals[7]
  stm_sta = vals[8] 
  attCoeff = vals[9]
  eup = vals[10]
  ncas = vals[11]
  ncao = vals[12]
  cop = vals[13]
  pCO2 = vals[14]
  ageFromYSL = vals[15]
  eps = vals[16]
  dwmax = vals[17]
  
  npreyitems = 4
  zoo_carbon = c(eup, ncas, ncao, cop)
  par_a = c(1.38E-8, 2.75E-12, 1E-10, 2.4E-8)
  par_b = c(2.92, 4.03, 3.56, 2.85)
  min_len = c(3000, 400, 200, 200)
  dlen = c(3000, 200, 200, 200)
  nsizes = c(10, 14, 7, 7)
  zoolen = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
  nallsizes = length(zoolen)
  
  zooInd = matrix(0, nrow = npreyitems, ncol = nallsizes)
  eupInd =  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
  ncasInd = c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0)
  ncaoInd = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  copInd =  c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  prey_wgt = matrix(0, nrow = npreyitems, ncol = nallsizes)
  prey_area = matrix(0, nrow = npreyitems, ncol = nallsizes)
  prey_abun = matrix(0, nrow = npreyitems, ncol = nallsizes)
  
  for(pit in 1:npreyitems) {
    
    out_zoo = zooplankton(zoo_carbon[pit], par_a[pit], par_b[pit], min_len[pit], dlen[pit], nsizes[pit])
      
      sind = 1
      for(psi in 1:nallsizes) {
        
        if(pit == 1) zooInd[pit, psi] = eupInd[psi]
        if(pit == 2) zooInd[pit, psi] = ncasInd[psi]
        if(pit == 3) zooInd[pit, psi] = ncaoInd[psi]
        if(pit == 4) zooInd[pit, psi] = copInd[psi]
        
        if(zooInd[pit, psi] == 1) {
          prey_wgt[pit, psi] = out_zoo[2, sind]
          prey_area[pit, psi] = out_zoo[3, sind]
          prey_abun[pit, psi] = out_zoo[4, sind]
          sind = sind + 1
        }
        
      }
      
  }
  
  # calculate co2 factor:
  facCO2 = calcCO2(pCO2)
  
  meta = dtday*2.38e-7*exp(0.088*temp)*(mass^0.9)*(1 + facCO2*meta_co2)
  metamax = dtday*2.38e-7*exp(0.088*temp)*(dwmax^0.9)*(1 + facCO2*meta_co2)
  
  if(eb > 0.001) {
    if(std_len > 5.5){
      meta = meta*2.5
      metamax = metamax*2.5
    } else {
      meta = meta*1.4
      metamax = metamax*1.4
    }
  } 
  
  assi = 0.8*(1 - 0.4*exp(-0.002*(mass*1000 - 50)))
  
  r = ((0.454 + 1.610*temp - 0.069*temp*temp)*exp(-2.225*mass))/100
  rmax = ((0.454 + 1.610*temp - 0.069*temp*temp)*exp(-2.225*dwmax))/100
  
  if(ageFromYSL <= 14) {
    r = r*(1 - facCO2*growth_co2_1)
    rmax = rmax*(1 - facCO2*growth_co2_1)
  }
  if((ageFromYSL > 14) & (ageFromYSL <= 35)) {
    r = r*(1 + facCO2*growth_co2_2)
    rmax = rmax*(1 + facCO2*growth_co2_2)
  }
  
  gr_mg = mass*(exp(r*dtday) - 1)
  gr_mg_max = dwmax*(exp(rmax*dtday) - 1)
    
  # START FORAGING PART:
  contrast = 0.3
  em = (std_len^2)/(contrast*0.1*0.2*0.75)
  dt_num = 100
  dt_pca = 0.1
  dr = 0.0
  pt = 0.0
  m_teta = pi/6
  var_teta = pi/6
  x_star = 0.5*std_len
  speed_fish = 10.0
  speed_prey = 100.0
  va = speed_fish*std_len
  ke_larvae = 1
  beamAttCoeff = attCoeff*3
  ke_predator = 1
  
  numing = 0
  dening = 0
  ing = 0
  enc = 0
  hand = 0
  pca = 0
  psa = 0
  prey_normal_speed = 0
  
  n_enc = 10
  gape = exp(-3.720 + 1.818*log(std_len) - 0.1219*(log(std_len)^2))
  pl_max = 0.08
  pl_min = 0.03
  
  return_vec = numeric(9)
  
  max_psize = std_len*pl_max
  min_psize = std_len*pl_min
  sum_numing = 1E-20
  sum_dening = 1E-20
  avgRankNum = 0
  avgSizeNum = 0
  stomachFullness = 0
  
  sizePref = 0.055
  ratioLens = zoolen/std_len
  diffLens = abs(ratioLens - sizePref)
  rankLens = rankify(diffLens, nallsizes)
  
  stopwar = FALSE
  
  # length_loop
  for(itm in 1:nallsizes) { 
    
      if(eb < 1E-15) { 
        break
      } 
    
      elementToFind = itm
      i = which(rankLens == elementToFind)
      
      if(zoolen[i] > max_psize | zoolen[i] < min_psize) { 
        break
      }
        
      # prey_loop
      for(pit in 1:npreyitems) {
        
        if(prey_abun[pit,i] < 0.0001) { 
          next 
        }
          
        #print(paste0('prey_loop: i is ', i, ', itm is ', itm, ', and pit is ', pit))
        
        if(stomachFullness >= 1) {
          stopwar = TRUE
          break
        }
          
        ier = 0
        visual = sqrt(em*contrast*prey_area[pit,i]*(eb/(ke_larvae+eb)))
        image = prey_area[pit,i]
        
          getr_out = getr(beamAttCoeff, contrast, image*1E-06, em, ke_larvae, eb)
          visual = getr_out[2]*1000 # in mm
          
          # Capture and approach probabilities:
          c = 0.5*gape
          rs = c + 0.1*std_len
          d_crit = 0.264/zoolen[i]
          w = speed_prey*zoolen[i]
          capt_pca = 0
          capt_psa = 0
          travel = 0.43
          max_ats = 3
          pt = 0
          
          if(std_len <= 17) {

            # Calculate the probability of approach and capture
            for(j in 1:n_enc){

              d = max(visual, c)
              k_iter_last = 1

              #approach_loop
              for(k in 1:dt_num){

                v = 0
                k_iter_last = k
                if(d > rs) {
                  v = (d_crit*2*((d^4)))/(3*c*(((d^2))-((c^2))))
                  v = dt_pca*min(std_len, v)
                } else {
                  capt_psa = capt_psa + 1
                  break
                }

                dr = -1*v*(1-(3*c/(2*d))+(c^2)/(2*(d^3)))
                d = d + dr

              }

              pt = pt + k_iter_last*dt_pca

              # THIS LOOP IS GENERATED BY MYSELF (Giancarlo). IT IS A BETTER WAY TO PROGRAM THIS PART
              # enc_loop_part2
              for(j2 in 1:max_ats) {

                # Generate random number:
                  r_rand = runif(n = 1, min = 0, max = 1)
                  u1 = max(0.00001, r_rand)
                  var_teta = sqrt(-2*log(u1))*cos(2*pi*r_rand)

                  teta = (m_teta - var_teta*m_teta)

                  if(teta > pi) {
                    teta = 2*pi - teta
                  }

                  teta = abs(teta)

                  if(teta < pi*0.5) {
                    if((gape*0.5/x_star) > tan(teta)) {
                      if((x_star*cos(teta)/w) < ((rs - c + x_star)/va)) {
                        next
                      }
                    }
                  }

                  # Equation 11 in Fiksen and MacKenzie 2002
                  capture = (w/va) * (sin(teta)*(rs+c)+(gape/2)*cos(teta))

                  if(capture < (gape*0.5)) {
                    capt_pca = capt_pca + 1
                  } else {
                    next
                  }

              }

            }

            pt = pt/n_enc

            psa = min(1, capt_psa/n_enc)
            pca = min(1, capt_pca/n_enc)

          }
          
          if(std_len > 17) { 
              
              par_a_cs = 1.1*std_len/(pl_max*std_len)                                   
              pca = max(0, 1 - (par_a_cs * (zoolen[i]/std_len)))
              
          } 
          
          omega = sqrt(3.615*((eps*visual*0.001)^0.667))
          omega = omega * 1000
          
          # Equation based on Bradley et al 2013, Figure 6:
            prey_normal_speed = zoolen[i]*(1.94*(zoolen[i]^-1.005))
          # Figure 2 in Walton 1992:
            hand = exp(0.264*(10^(7.0151*(zoolen[i]/std_len))))
          # See Fiksen and MacKenzie 2002 Equation 1: 
          enc = ((0.667*pi*(visual^3)*travel + pi*(visual^2)*sqrt((prey_normal_speed^2) + 2*(omega^2))*travel*2)*(1*prey_abun[pit,i])*(1 - facCO2*preyabun_co2)*1e-6)
          numing = enc*pca*(1-facCO2*pca_co2)*prey_wgt[pit,i]*(1-facCO2*preywgt_co2)*0.001 # ug to mg
          dening = enc*hand
            
          
          sum_numing = sum_numing + numing
          sum_dening = sum_dening + dening
          ing = dt*sum_numing/(1 + sum_dening)
          
          avgRankNum = avgRankNum + numing*(pit)
          avgSizeNum = avgSizeNum + numing*(zoolen[i])
          stomachFullness = min(1, (stm_sta + ing/(mass*0.06)))
              
      } # end prey_loop
        
      #print(paste0('length_loop: stomachFull is ', stomachFullness))
      if(stopwar) break
      
  } # end of length_loop

  return_vec[1] = gr_mg
  return_vec[2] = meta
  return_vec[3] = ing
  return_vec[4] = assi
  return_vec[5] = stomachFullness
  return_vec[6] = avgRankNum/sum_numing
  return_vec[7] = avgSizeNum/sum_numing
  return_vec[8] = metamax
  return_vec[9] = gr_mg_max
  return(return_vec)
  
  
}