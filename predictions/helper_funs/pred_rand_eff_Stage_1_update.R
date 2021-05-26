

library(dplyr)

pred_random_effects_update <- function(i, index_patient, timepoint){
    
    new_patient <- data_new_stage1 %>% 
        filter(Patient.ID == index_patient & time_ichor <= timepoint & Treatment_new_final == most_recent_treat$uniq_treat)     
    
    # the following uses all the past measurments (from a given timepoint)
    #new_patient <- data_new_stage1 %>% 
    #    filter(Patient.ID == index_patient & time_ichor <= timepoint)     
    
    Xbeta <- posterior_linpred(model_first_stage, newdata = new_patient, subset = i, re_formula = NA)
    
    R <- diag(length(new_patient$time_ichor)) * (post_samples$sigma[i]^2) # The residuals variance matrix
    
    Z <- matrix(c(rep(1, length(new_patient$ichorCNA_tr)), new_patient$time_ichor), 
                nrow = length(new_patient$ichorCNA_tr), ncol = 2) # The random effects design matrix 
    
    kk = new_patient$ichorCNA_tr - Xbeta
    
    G <- matrix(c(post_samples$sd_Patient.ID__Intercept[i]^2, 
                  post_samples$cov[i], 
                  post_samples$cov[i], 
                  post_samples$sd_Patient.ID__time_ichor[i]^2), nrow = 2, ncol = 2)
    
    SigmaMatrix = Z%*%G%*%t(Z) + R
    
    Inversigma = solve(SigmaMatrix) 
    
    REPredicted = G%*%t(Z)%*%Inversigma%*%as.vector(kk)
    
    RandomIntercept = REPredicted[1,1]
    RandmSlope = REPredicted[2,1]
    
    rand_effects <- c(RandomIntercept, RandmSlope)
    
    return(rand_effects)
    
}

