zooplankton = function(total_abundance, par_a, par_b, min_len, dlen, nsizes) {

  return_array = matrix(NA, ncol = nsizes, nrow = 4)
  
  prey_length = seq(from = min_len, by = dlen, length.out = nsizes) # in um
  prey_wgt = par_a * prey_length^par_b # in ug
  prey_width = prey_length*0.3/1000 # in mm
  
  sd_pl = 695.73 * exp(-0.0083*prey_length)
  m_new = exp(2.772 * log(prey_length) - 7.476)
  
  tm = 0
  for(i in 1:length(m_new)){
    tm = tm + (m_new[i]*sd_pl[i])  
  }
  
  sp_pl = (sd_pl * m_new)/tm
  prey_item_1_ug = (total_abundance*1000)*2.5
  
  return_array[1,] = prey_length/1000 # in mm
  return_array[2,] = prey_wgt # in ug
  return_array[3,] = 0.75*(prey_length/1000)*prey_width # mm^2
  return_array[4,] = sp_pl * (prey_item_1_ug/prey_wgt)/1000 # no.ind/L
  
  return(return_array)

}
