#############################################################################
# splits data into train and test sets based on ichorCNA 
# observation where time_ichor > time_CT are deleted - these should be 2
#############################################################################

library(dplyr)

##################################################################   
########################## IchorCNA ##############################
################################################################## 
# load ichor dataset
load("~/Box/PhD/Code/ctDNA/updated/ichorCNA_Treatments.Rdata") # updated dataset

# time unit conversion
# https://www.datasciencemadesimple.com/get-difference-between-two-dates-in-r-by-days-weeks-months-and-years-r-2/
# time in years - time/52.14
data_ichorCNA_time <- ichorCNA_Treatments %>% 
    mutate(time = as.numeric(time)) %>%
    mutate(time = time/52.14, # time in years
           Treatment_duration = Treatment_duration/52.14) %>% # time in years
    select(Patient.ID, Date, ichorCNA_tr, ER.status, Her2.status, time, Treatment_new_final, Treatment_duration)

##################################################################   
############################# CT ################################# 
##################################################################
# load CT dataset
load("~/Box/PhD/Code/ctDNA/updated/RECIST_treatments.Rdata") # for mac

# transformations and select cols
data_RECIST_time <- RECIST_treatments %>% 
    mutate(time = as.numeric(time/52.14), # time in years
           Treatment_duration = Treatment_duration/52.14) %>% # time in years
    select(Patient.ID, Date, Progression, ER.status, Her2.status, Treatment_new_final, time, Treatment_duration)

##################################################################   
############################# CT - after separation train/test ### 
##################################################################
# load train data - to select relevant Patient IDs
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_train_CT.Rdata")

##################################################################
# keep same patients as in train data 
data_ichorCNA_Patients_time3 <- data_ichorCNA_time %>% 
    filter(Patient.ID %in% data_train_CT$Patient.ID)

# should be 146 (3 patients are used for indiv dynamic predictions)
#length(unique(data_ichorCNA_Patients_time3$Patient.ID))

# max CT scan time - full CT dataset
df_max_time_CT <- data_RECIST_time %>% 
    group_by(Patient.ID) %>% 
    slice_max(time)

# merge the two datasets 
df_combine <- merge(data_ichorCNA_Patients_time3, df_max_time_CT, by = "Patient.ID") 

# post-processing 
df_train_ichor <- df_combine %>% 
    group_by(Patient.ID) %>%
    filter(time.x <= time.y) %>% # keep ichorCNA if earlier than max CT scan
    select(Patient.ID:Treatment_duration.x) %>% # select relevant cols
    rename(Date.ichor = Date.x, Her2.status = Her2.status.x, ER.status = ER.status.x, Treatment_new_final = Treatment_new_final.x, 
           time_ichor = time.x, Treatment_duration = Treatment_duration.x) %>%
    distinct(.keep_all = TRUE)

#setwd("~/Box/PhD/Code/ctDNA/updated/data_split")
#save(df_train_ichor, file = "data_train_ichor.Rdata")

##################################################################
## rest of patients used for individualised predictions
data_ichorCNA_Patients_time4 <- data_ichorCNA_time %>% 
    filter(!Patient.ID %in% data_train_CT$Patient.ID)

# should be 3 patients are used for indiv dynamic predictions
#length(unique(data_ichorCNA_Patients_time4$Patient.ID))

#data_ichorCNA_Patients_time4 %>% 
#    group_by(Patient.ID) %>% 
#    summarise(n())

# max CT scan time - full CT dataset
df_max_time_CT_2 <- data_RECIST_time %>% 
    filter(Patient.ID %in% unique(data_ichorCNA_Patients_time4$Patient.ID)) %>%
    group_by(Patient.ID) %>% 
    slice_max(time)

# merge the two datasets 
df_combine_2 <- merge(data_ichorCNA_Patients_time4, df_max_time_CT_2, by = "Patient.ID")

# post-processing 
df_train_ichor_ind <- df_combine_2 %>% 
    group_by(Patient.ID) %>%
    filter(time.x <= time.y) %>% # keep ichorCNA if earlier than max CT scan
    select(Patient.ID:Treatment_duration.x) %>% # select relevant cols
    rename(Date.ichor = Date.x, Her2.status = Her2.status.x, ER.status = ER.status.x, Treatment_new_final = Treatment_new_final.x, 
           time_ichor = time.x, Treatment_duration = Treatment_duration.x) %>%
    distinct(.keep_all = TRUE)

#setwd("~/Box/PhD/Code/ctDNA/updated/data_split")
#save(df_train_ichor_ind, file = "data_ichor_ind.Rdata")

######## check
# should be 146 (3 patients are used for indiv dynamic predictions)
#length(unique(df_train_ichor$Patient.ID))

# see where the discrepancy is
#miss <- setdiff(unique(df_combine$Patient.ID), unique(df_train_ichor$Patient.ID))
#miss


