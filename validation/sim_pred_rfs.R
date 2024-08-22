
# get array element number
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
task_id <- as.numeric(task_id_string)
#task_id = 2

########################################################################

library(dplyr)
library(brms)
library(tidyr)
library(purrr)

########################################################################
#################################### Step 1 ############################
########################################################################
# load data
path_data <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/" # mac
#path_data <- "/home/sk921/rds/rds-mrc-bsu/sk921/PhD/Code/ctDNA/updated/validation/" # HPC

load(paste0(path_data, "validation_data.Rdata"))

subject <- unique(df_RECIST_Ant$Patient.ID)

data_new_stage1 <- df_ichor_Ant %>% 
    filter(Patient.ID == subject[task_id]) %>% 
    arrange(Date.ichor)

print(data_new_stage1)

data_new_stage2 <- df_RECIST_Ant %>% 
    filter(Patient.ID == subject[task_id]) %>% 
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
#load("/home/sk921/rds/rds-mrc-bsu/sk921/PhD/Code/ctDNA/updated/models/model_1st_stage.Rdata") # for HPC

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
df_final <- data.frame()

# loop through dataset to predict random effects
system.time({
for(g in 1:length(df_new_preds_1_split)){
    
    data_new_stage1 <- df_new_preds_1_split[[g]] %>% filter(Date.ichor <= Date)
    
    print(data_new_stage1)

    if(nrow(data_new_stage1) == 0){
        next
    }
 
    print(g)
    
    datagrid <- expand.grid(i = as.numeric(1:iters),
                            index_patient = subject[task_id],
                            timepoint = unique(data_new_stage1$time))
    
    res <- datagrid %>%
        mutate(res_rand = pmap(., pred_random_effects_update_v2)) %>%
        unnest_wider(res_rand, names_sep = "_") %>%
        group_by(timepoint, index_patient) %>%
        summarise(estim_inter = mean(res_rand_1), estim_slope = mean(res_rand_2)) %>%
        rename(Patient.ID = index_patient, time = timepoint) %>%
        mutate(g = g)
    
    #print(res)
    
    # merge df with estimated random effects
    df_stage1_prelim <- merge(res, data_new_stage1, by = c("Patient.ID", "time")) %>%
        distinct(Patient.ID, Date, time, .keep_all = T) %>%
        select(Patient.ID, Date, time, estim_inter, estim_slope, g)
    
    #print(df_stage1_prelim)
    
    df_final <- rbind(df_final, df_stage1_prelim)
    
    #print(df_final)
    
    }

})

print(df_final)

# merge datasets     
df_combine_2 <- merge(data_new_stage2, df_final, by = c("Patient.ID", "time")) 

df_new_preds <- df_combine_2 %>% mutate(Progression = ifelse(Progression == 0, "NO", "YES"))

str(df_new_preds)

########################################################################
#################################### Step 5 ############################
########################################################################
# load stage 2 model
#load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/models/model_2nd_stage_ichor.Rdata") # for mac
load("/home/sk921/rds/rds-mrc-bsu/sk921/PhD/Code/ctDNA/updated/models/model_2nd_stage_ichor.Rdata") # HPC

# calculate posterior predictive distribution  
post_pred_ctDNA <- posterior_epred(fit2_CT_ichor, 
                                   newdata = df_new_preds, 
                                   re_formula = NULL,
                                   allow_new_levels = T, 
                                   sample_new_levels = "gaussian")

# calculate average of posterior predictive distribution
post_pred_avg <- colMeans(post_pred_ctDNA)

df_new_preds_final <- cbind(df_new_preds, pred_prob = post_pred_avg)

df_new_preds_final

output_file <- paste0("output_pred_", task_id, ".Rdata")

#### 
setwd("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/predictions/output/") # mac
#setwd("/rds/user/sk921/hpc-work/PhD/ctDNA/updated/predictions/output_ctDNA_project_treatment_2") # HPC uni

save(data_new_stage1,
     df_new_preds_final,
     post_pred_ctDNA, 
     file = output_file) 



