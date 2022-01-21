# get array element number
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
task_id <- as.numeric(task_id_string)
#task_id = 3

########################################################################
#################################### Step 1 ############################
########################################################################
# load ichor dataset - dynamic predictions 
#load("~/Box/PhD/Code/ctDNA/updated/data_split/data_ichor_ind.Rdata") # updated dataset mac
load("~/HPC/PhD/Code/ctDNA/updated/data_split/data_ichor_ind.Rdata") # HPC

subject <- unique(df_train_ichor_ind$Patient.ID)

library(dplyr)
data_new_stage1 <- df_train_ichor_ind %>% 
    mutate(Treatment_duration = as.numeric(Treatment_duration)) %>% # duration needs to be numeric
    filter(Patient.ID == subject[task_id]) %>% 
    arrange(time_ichor)

print(data_new_stage1)

########################################################################
#################################### Step 3 ############################
########################################################################
# load stage 1 model
#load("~/Box/PhD/Code/ctDNA/updated/models/model_1st_stage.Rdata") # for mac
load("~/HPC/PhD/Code/ctDNA/updated/models/model_1st_stage.Rdata") # for HPC

library(brms)

model_first_stage <- fit1_ichor

### pre-processing MCMC chain
iters <- 1000
post_samples <- posterior_samples(model_first_stage)[1:iters,][1:28] # change upper limit depending on model
post_samples <- post_samples %>% mutate(cov = cor_Patient.ID__Intercept__time_ichor *(sd_Patient.ID__Intercept * sd_Patient.ID__time_ichor))

# load random effects prediction function - update 2 
#source("~/Box/PhD/Code/ctDNA/updated/predictions/helper_funs/pred_rand_eff_Stage_1_update.R") # mac 
source("~/HPC/PhD/Code/ctDNA/updated/predictions/helper_funs/pred_rand_eff_Stage_1_update.R") # HPC

library(tidyr)
library(purrr)

########################################################################
#################################### Step 4 ############################
########################################################################
datagrid <- expand.grid(i = as.numeric(1:iters),
                                index_patient = subject[task_id],
                                timepoint = max(data_new_stage1$time_ichor))
        
res <- datagrid %>%
            mutate(res_rand = pmap(., pred_random_effects_update_v2)) %>%
            unnest_wider(res_rand, names_sep = "_") %>%
            group_by(timepoint, index_patient) %>%
            summarise(estim_inter = mean(res_rand_1), estim_slope = mean(res_rand_2)) %>%
            rename(Patient.ID = index_patient, time_ichor = timepoint)
        
# merge df with estimated random effects
df_stage1_prelim <- cbind(data_new_stage1, estim_inter = res$estim_inter, estim_slope = res$estim_slope)
        
df_new_preds <- df_stage1_prelim %>% mutate(time = time_ichor)

########################################################################
#################################### Step 5 ############################
########################################################################
# load stage 2 model
#load("~/Box/PhD/Code/ctDNA/updated/models/model_2nd_stage_ichor.Rdata") # for mac
load("~/HPC/PhD/Code/ctDNA/updated/models/model_2nd_stage_ichor.Rdata") # for HPC

# option 1 
post_pred_ctDNA <- posterior_epred(fit2_CT_ichor, 
                                   newdata = df_new_preds, 
                                   re_formula = NULL,
                                   allow_new_levels = T, 
                                   sample_new_levels = "gaussian")

output_file <- paste0("output_pred_", subject[task_id], ".Rdata")

# Set the working directory to a subfolder within the current working directory
setwd("/rds/user/sk921/hpc-work/PhD/ctDNA/updated/predictions/output_pred_ichor_timepoints_three_patients") # HPC uni

save(data_new_stage1,
     #data_new_stage2,
     df_new_preds,
     post_pred_ctDNA, 
     file = output_file) 

