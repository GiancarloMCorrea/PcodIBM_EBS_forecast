
TotalMortality = function(larval_mm, Eb, attCoeff, new_larva_wgt, stomachFullness, dwmax) {
  
  return_mort = numeric(5)
  
  larvalShape = 0.2
  contrast = 0.3
  em = 5.0E4
  visFieldShape = 0.5
  fishSwimVel = 0.10
  aPred = 2.77e-6
  bPred = -1.3
  starvationMortality = 1e-5
  setMort = 1 
  ke_predator = 1
  fishDens = 0.0001
  deadThreshold = 0.75
  m2mm = 1000
  beamAttCoeff = attCoeff*3
  kval = 5e-6
  
  larvalWidth   = larvalShape*larval_mm
  image = larvalWidth*larval_mm
  
  ier = 0;
  visual = 0.0
  
    if(Eb < 1E-15) { 
      visual = 0;
    } else {
      getr_out = getr(beamAttCoeff, contrast, image*1E-06, em, ke_predator, eb)
      visual = getr_out[2] # in m
    }
  
  pe = 0.92/(1 + exp((larval_mm-16.3)/4.13))
  
  #fishMortality = setMort*(visFieldShape*Math.PI*Math.pow(visual,2)*fishSwimVel*fishDens);
  fishMortality = kval*pe*(visual)^2
  invertebrateMortality = setMort*OtherPred(larval_mm, aPred, bPred)
  starved = AliveOrDead(new_larva_wgt, stomachFullness, deadThreshold, dwmax)
  mortality = (invertebrateMortality + fishMortality + starved*starvationMortality)
  
  return_mort[1] = mortality
  return_mort[2] = starved
  return_mort[3] = fishMortality*1000000
  return_mort[4] = invertebrateMortality*1000000
  return_mort[5] = starved*starvationMortality*1000000
  
  return(return_mort)
  
}


OtherPred = function(larval_mm, aPred, bPred) {
  
  otherPred = aPred*(larval_mm^bPred)
  return(otherPred)

}


AliveOrDead = function(new_larva_wgt, stomachFullness, deadThreshold, dwmax) {
  
  aliveOrDead = 0
  
  if ((stomachFullness < 0.01)) {
    aliveOrDead = 1
  }
  
  if (dwmax*deadThreshold > new_larva_wgt) {
    aliveOrDead = 100000
  }
  
  return(aliveOrDead)
  
}