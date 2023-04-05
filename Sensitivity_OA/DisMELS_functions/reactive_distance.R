
getr = function(beam_att, prey_contrast, prey_area, visual_cap, Ke, Eb) {

  r2 = abs(prey_contrast)*prey_area*visual_cap*(Eb/(Ke+Eb))
  r = sqrt(r2)

  epsilon = 0.0001
  iend = 200
  tol = r
  
  fr2 = log(abs(prey_contrast)*prey_area*visual_cap)
  fr1 = log(((Ke+Eb)/Eb)*r*r*exp(beam_att*r))
  f1 = fr1 - fr2
  fder = beam_att + 2/r
  
  tolf = 100*epsilon
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
        
        fr2 = log(abs(prey_contrast)*prey_area*visual_cap)
        fr1 = log(((Ke+Eb)/Eb)*r*r*exp(beam_att*r))
        f1 = fr1 - fr2
        fder = beam_att + 2/r
        tol = epsilon
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

  ret_vec = c(ier, r)
  
  return(ret_vec)

}
