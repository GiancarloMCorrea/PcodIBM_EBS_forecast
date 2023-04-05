calcCO2 = function(co2_val) {
  
  # Resilient:
  #outVal = min(1, (1/665)*(exp(co2_val*0.005) - 1))
  # Non-resilient:
  outVal = min(1, max(0, -0.5 + (1/1000)*co2_val))
  # None:
  # outVal = 0
        
  return(outVal)
  
}


getL_fromW = function(wgt, len) {
  
  len_out = (wgt/1.976E-06)^(1/2.974)
  if(len_out < len) len_out = len
  return(len_out)
  
}

getW_fromL = function(len) {
  
  wgt_out = 1.976E-06*(len^2.974)
  return(wgt_out)
  
}


rankify = function(Aobj, nobj) {
  
  Rout = numeric(nobj)
  
  for (i in 1:nobj) {
    r = 1 
    s = 1
    
    for (j in 1:nobj)
    {
      if ((j != i) & (Aobj[j] < Aobj[i]))
        r = r + 1
        
        if ((j != i) & (Aobj[j] == Aobj[i]))
          s = s + 1   
    }
    
    Rout[i] = r + (s - 1) / 2
    
  }
  
  return(Rout)
  
}


calculatePNR = function(temp) {

  out = 34.67 * exp(-0.126 * temp)
  return(out)

}

calculateYSA = function(temp) {

  out = 14.7662 * exp(-0.235 * temp)
  return(out)

}

calcLight = function(chla, depth, bathy) {

  attCoef = 0.034 + 0.0518*(chla^0.428) + 0.0363 + 2.833*(bathy^-1.079)
  eb_tmp = exp(-1*depth*attCoef)

  out = c(attCoef, eb_tmp)
  return(out)

}