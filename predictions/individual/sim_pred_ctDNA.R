# get array element number
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
task_id <- as.numeric(task_id_string)
#task_id = 1

########################################################################
#################################### Step 1 ############################
########################################################################
# load ichor dataset - dynamic predictions 
#load("~/Box/PhD/Code/ctDNA/updated/data_split/data_ichor_ind.Rdata") # updated dataset mac
load("~/HPC/PhD/Code/ctDNA/updated/data_split/data_ichor_ind.Rdata") # HPC

subject <- unique(df_train_ichor_ind$Patient.ID)

library(dplyr)
data_new_stage1 <- df_train_ichor_ind %>% 
    filter(Patient.ID == subject[task_id]) %>% 
    arrange(time_ichor)

print(data_new_stage1)

########################################################################
#################################### Step 2 ############################
########################################################################
# load CT dataset
#load("~/Box/PhD/Code/ctDNA/updated/data_split/data_dynamic_pred_CT.Rdata") # updated dataset mac
load("~/HPC/PhD/Code/ctDNA/updated/data_split/data_dynamic_pred_CT.Rdata") # HPC

# select relevant patient 
data_new_stage2 <- data_ind_pat_CT %>% 
    filter(Patient.ID == subject[task_id]) %>% 
    arrange(time)

print(data_new_stage2)

# merge datasets     
df_combine_1 <- merge(data_new_stage2, data_new_stage1, by = "Patient.ID")

# processing - "project/predict" ichor measurement closest to the CT scan
df_new_preds_1 <- df_combine_1 %>% 
    group_by(Patient.ID) %>%
    filter(time_ichor <= time) %>% # keep ichorCNA if earlier than CT scan
    mutate(abs_diff = abs(time_ichor - time)) %>% #compute time gap 
    group_by(Patient.ID, time) %>% # for each Patient and CT timepoint
    mutate(time_ichor = ifelse(abs_diff == min(abs_diff), time, time_ichor)) %>% # project the CT timepoint to the ichor timepoint
    select(Patient.ID, 
           Her2.status.x, 
           ER.status.x, 
           Treatment_new_final.y, 
           Treatment_duration.y,  
           ichorCNA_tr, 
           time, time_ichor) %>% 
    rename(Her2.status = Her2.status.x, 
           ER.status = ER.status.x, 
           Treatment_new_final = Treatment_new_final.y,
           Treatment_duration = Treatment_duration.y) %>%
    mutate(Treatment_duration = as.numeric(Treatment_duration))

# split dataframe by groups - to avoid wrong timepoints for ichor 
df_new_preds_1_split <- df_new_preds_1 %>% group_split()

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

# loop through dataset to predict random effects
df_final <- data.frame()

system.time({
for(g in 1:length(df_new_preds_1_split)){

    data_new_stage1 <- df_new_preds_1_split[[g]]

    # select only measurements under the same treatment regime
    most_recent_treat <- data_new_stage1 %>%
        summarise(uniq_treat = unique(Treatment_new_final)) %>%
        slice(n())

    datagrid <- expand.grid(i = as.numeric(1:iters),
                            index_patient = subject[task_id],
                            timepoint = unique(data_new_stage1$time))

    res <- datagrid %>%
        mutate(res_rand = pmap(., pred_random_effects_update)) %>%
        unnest_wider(res_rand, names_sep = "_") %>%
        group_by(timepoint, index_patient) %>%
        summarise(estim_inter = mean(res_rand_1), estim_slope = mean(res_rand_2)) %>%
        rename(Patient.ID = index_patient, time_ichor = timepoint)

    # merge df with estimated random effects
    df_stage1_prelim <- merge(res, data_new_stage1, by = c("Patient.ID", "time_ichor"))

    df_final <- rbind(df_final, df_stage1_prelim)
}

})

str(df_final)
 
# merge datasets     
df_combine_2 <- merge(data_new_stage2, df_final, by = c("Patient.ID", "time")) 
 
df_new_preds <- df_combine_2 %>%
    group_by(Patient.ID) %>%
    filter(time_ichor <= time) %>% # keep ichorCNA if earlier than CT scan
    #mutate(abs_diff = abs(time_ichor - time)) %>% # compute time gap
    #filter(abs_diff <= time_window) %>% # keep if gap within the time_window
    group_by(Patient.ID, time) %>%
    #filter(abs_diff == min(abs_diff)) %>% # for each CT timepoint and patient keep min time gap
    select(Patient.ID:Her2.status.x, Treatment_new_final.x,
           Treatment_duration.x, time, time_ichor, estim_inter, estim_slope, Progression) %>%
    rename(Her2.status = Her2.status.x, ER.status = ER.status.x, Treatment_new_final = Treatment_new_final.x,
           Treatment_duration = Treatment_duration.x) %>%
    mutate(Treatment_duration = as.numeric(Treatment_duration)) %>%
    distinct(.keep_all = TRUE)

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
setwd("/rds/user/sk921/hpc-work/PhD/ctDNA/updated/predictions/output_ctDNA_project_treatment_2") # HPC uni

save(data_new_stage1,
     #data_new_stage2,
     df_new_preds,
     post_pred_ctDNA, 
     file = output_file) 


