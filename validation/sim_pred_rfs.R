
library(dplyr)
library(brms)
library(tidyr)
library(purrr)

########################################################################
#################################### Step 1 ############################
########################################################################
# load data
path_data <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/"
load(paste0(path_data, "validation_data.Rdata"))


id <- 2000

subject <- id #unique(df_train_ichor_ind$Patient.ID)

data_new_stage1 <- df_ichor_Ant %>% 
    filter(Patient.ID == subject) %>% 
    arrange(Date.ichor)

print(data_new_stage1)

data_new_stage2 <- df_RECIST_Ant %>% 
    filter(Patient.ID == subject) %>% 
    arrange(Date)

print(data_new_stage2)

########################################################################
#################################### Step 2 ############################
########################################################################

# merge datasets     
df_combine_1 <- merge(data_new_stage2, data_new_stage1, by = "Patient.ID") 

df_new_preds_1 <- df_combine_1 %>%
    dplyr::select(Patient.ID, 
                  Date, 
                  Date.ichor,
                  Her2.status.x, 
                  ER.status.x, 
                  Treatment_new_final.y, 
                  Treatment_duration.y,  
                  ichorCNA_tr, 
                  time, 
                  time_ichor) %>% 
    rename(Her2.status = Her2.status.x, 
           ER.status = ER.status.x, 
           Treatment_new_final = Treatment_new_final.y,
           Treatment_duration = Treatment_duration.y) %>%
    mutate(Treatment_duration = as.numeric(Treatment_duration))

# split dataframe by groups - to avoid wrong timepoints for ichor 
df_new_preds_1_split <- df_new_preds_1 %>% group_by(Date) %>% group_split()

########################################################################
#################################### Step 3 ############################
########################################################################
# load stage 1 model
load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/models/model_1st_stage.Rdata") # for mac
#load("~/HPC/PhD/Code/ctDNA/updated/models/model_1st_stage.Rdata") # for HPC

model_first_stage <- fit1_ichor

### pre-processing MCMC chain
iters <- 1000
post_samples <- posterior_samples(model_first_stage)[1:iters,][1:28] # change upper limit depending on model
post_samples <- post_samples %>% mutate(cov = cor_Patient.ID__Intercept__time_ichor *(sd_Patient.ID__Intercept * sd_Patient.ID__time_ichor))

# load random effects prediction function - update 2 
source("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/predictions/helper_funs/pred_rand_eff_Stage_1_update.R") # mac 
#source("~/HPC/PhD/Code/ctDNA/updated/predictions/helper_funs/pred_rand_eff_Stage_1_update.R") # HPC

########################################################################
#################################### Step 4 ############################
########################################################################

# loop through dataset to predict random effects
for(g in 1:length(df_new_preds_1_split)){
    
    g <- 2
    data_new_stage1 <- df_new_preds_1_split[[g]] %>% filter(Date.ichor <= Date)
    
    print(data_new_stage1)

    datagrid <- expand.grid(i = as.numeric(1:iters),
                            index_patient = 2000, #subject[task_id],
                            timepoint = unique(data_new_stage1$time))
    
    res <- datagrid %>%
        mutate(res_rand = pmap(., pred_random_effects_update_v2)) %>%
        unnest_wider(res_rand, names_sep = "_") %>%
        group_by(timepoint, index_patient) %>%
        summarise(estim_inter = mean(res_rand_1), estim_slope = mean(res_rand_2)) %>%
        rename(Patient.ID = index_patient, time_ichor = timepoint)
    
    # merge df with estimated random effects
    df_stage1_prelim <- merge(res, data_new_stage1, by = c("Patient.ID", "time_ichor"))
    
    df_final <- rbind(df_final, df_stage1_prelim)
}


}


