
#############################################################################
# splits data into train and test sets based on CT scans
# test set: includes last observation per patient (if > 1 observation per patient)
# train set: the remaining observations
#############################################################################

################ load packages
library(dplyr)

################ load data
load("~/Box/PhD/Code/ctDNA/updated/RECIST_Treatments.Rdata") # for mac

################ 
# transformations and select cols
# time unit conversion
data_RECIST_time <- RECIST_treatments %>% 
    mutate(time = as.numeric(time/52.14), # time in years
           Treatment_duration = Treatment_duration/52.14) %>% # duration in years
    select(Patient.ID, Date, Progression, ER.status, Her2.status, Treatment_new_final, Treatment_duration, time)

################ 
#select patients for dynamic predictions
#patients_dyn_pred <- c("DT158", "DT171", "DT213", "DT232", "DT343") # first version
patients_dyn_pred <- c("DT158", "DT213", "DT343") # second version

data_ind_pat_CT <- data_RECIST_time %>% 
    filter(Patient.ID %in% patients_dyn_pred) 

data_excluded_ind_pat <- data_RECIST_time %>% 
    filter(!Patient.ID %in% patients_dyn_pred)

################ 
# keep patients with > 1 ctDNA measurement             
patients_keep <- data_excluded_ind_pat %>% 
    group_by(Patient.ID) %>% 
    summarise(measur_per_patient = n()) %>% 
    filter(measur_per_patient > 1)

# filter based on max Date per Patient.ID and >1 ctDNA measurement
df_max_Date <- data_excluded_ind_pat %>% 
    filter(Patient.ID %in% patients_keep$Patient.ID) %>% 
    group_by(Patient.ID) %>% 
    slice_max(Date)

#df_max_Date %>% ungroup() %>% summarise(n())

# return all rows from x with a match in y
data_test_CT <- data_excluded_ind_pat %>% 
    semi_join(df_max_Date, by = c("Date", "Patient.ID"), copy = T) 

# return all rows from x without a match in y
data_train_CT <- data_excluded_ind_pat %>% 
    anti_join(data_test_CT, by = c("Date", "Patient.ID"), copy = T) 

setwd("~/Box/PhD/Code/ctDNA/updated/data_split")
#save(data_train_CT, file = "data_train_CT.Rdata")
#save(data_test_CT, file = "data_test_CT.Rdata")
#save(data_ind_pat_CT, file = "data_dynamic_pred_CT.Rdata")

################
# checks
#dim(data_excluded_ind_pat)[1] == dim(data_train_CT)[1] + dim(data_test_CT)[1]

# all should be one
#data_test_CT %>% summarise(n()) %>% arrange(desc(`n()`))

# calculates the difference in measurements between full and train sets - should be 1 
#data_train_CT %>% 
#    filter(Patient.ID %in% patients_keep$Patient.ID) %>% 
#    summarise(measur_per_patient = n()) %>%
#    mutate(diff = patients_keep$measur_per_patient - measur_per_patient) %>%
#    summarise(range(diff))

