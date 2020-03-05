#define coeffitients
C0 = 2.515517
C1 = 0.802853
C2 = 0.010328
d1 = 1.432788
d2 = 0.189269
d3 = 0.001308

eddi_fun_return_all <- function(x) {
  # following Hobbins et al., 2016
      x = as.numeric(x)
      
      if(all(is.na(x))){
        return(NA)
      } else {
        
        #Rank PET (1 = max)
        rank_1 = rank(-x)
        
        #Calcualte emperical probabilities
        prob = ((rank_1 - 0.33)/(length(rank_1) + 0.33))
        
        #compute W (whaterver that is)
        W = numeric(length(prob))
        for(i in 1: length(prob)){
          if(prob[i] <= 0.5){
            W[i] = sqrt(-2*log(prob[i]))
          } else {
            W[i] = sqrt(-2*log(1 - prob[i]))
          }
        }
        
        #Find indexes which need inverse EDDI sign
        reverse_index = which(prob > 0.5)
        
        #Compute EDDI
        EDDI = W - ((C0 + C1*W + C2*W^2)/(1 + d1*W + d2*W^2 + d3*W^3))
        
        #Reverse sign of EDDI values where prob > 0.5
        EDDI[reverse_index] = -EDDI[reverse_index]
        
        #Return Current Value
        return(EDDI)
      }
}
