
# load data files
load("~/Box/PhD/Code/ctDNA/updated/pre_process_RECIST.Rdata") # RECIST formatted
load("~/Box/PhD/Code/ctDNA/updated/treatments_simplified.Rdata") # Treatments BASE-simplified - New file

library(dplyr)

RECIST_treatments_prelim <- merge(data_RECIST_Patients_time, Treatment_BASE_update, by = "Patient.ID")

# pre-processing 
df_generic <- RECIST_treatments_prelim %>% 
    group_by(Patient.ID) %>%
    #mutate(interval = Start.Date %--% End.Date) %>%
    mutate(keep_outside = ifelse((Date < Start.Date) | (Date > End.Date), 1, 0), # data outside the start/end treatment intervals
           #keep_within = ifelse(Date %within% interval, 1, 0)) %>%  # data within the start/end treatment intervals - inclusive leads to duplication
           keep_within = ifelse( (Date > Start.Date) & (Date <= End.Date), 1, 0)) %>% # data within the start/end treatment intervals
    mutate(Treatment_new2 = ifelse(keep_within == 1, Treatment_new, "off-treatment")) # assign "off-treatment" if measurement outside treatment intervals

# outcome for timepoints within treatment periods  
df_within <- df_generic %>% filter(keep_within == 1) 

# outcome for timepoints outside treatment periods 
df_outside <- df_generic %>% filter(keep_outside == 1 & Treatment_new2 == "off-treatment") %>% distinct(Date, .keep_all = TRUE)

# merge
df_merged <- full_join(df_outside, df_within, by = c("Patient.ID", "Date")) %>% 
    mutate(Treatment_new_final = ifelse(is.na(Treatment_new2.y), Treatment_new2.x, Treatment_new2.y),
           Start.Date.y = if_else(is.na(Start.Date.y), lag(End.Date.y), Start.Date.y), # important to use dplyr::if_else()
           Start.Date.y = if_else(is.na(Start.Date.y), lag(End.Date.x), Start.Date.y), # value for non-existing rows - take End.Date.x
           Start.Date.y = if_else(is.na(Start.Date.y), End.Date.x, Start.Date.y), # value for non-existing rows - when start with NA for each patient
           Treatment_duration = abs(difftime(Date, Start.Date.y, units = c("weeks")))) # calculates treatment duration from each treatment start to Date of measurment 


# rename dataframe and cols
RECIST_treatments <- df_merged %>% 
    mutate(Progression = ifelse(is.na(Progression.x), Progression.y, Progression.x),
           ER.status = ifelse(is.na(ER.status.x), ER.status.y, ER.status.x),
           Her2.status = ifelse(is.na(Her2.status.x), Her2.status.y, Her2.status.x),
           time = ifelse(is.na(time.x), time.y, time.x),
           #ichorCNA = ifelse(is.na(ichorCNA.x), ichorCNA.y, ichorCNA.x)
           ) %>%
    select(Patient.ID, Date, Progression, Treatment_new_final, Treatment_duration, ER.status, Her2.status, time) %>%
    distinct(Date, .keep_all = TRUE)

# check NA per column
RECIST_treatments %>% ungroup() %>% summarise_all(funs(sum(is.na(.))))

#setwd("~/Box/PhD/Code/ctDNA/updated")
#save(RECIST_treatments, file = "RECIST_treatments.Rdata")

##########################################################################################
##########################################################################################
##########################################################################################
# # trial one patient
# select_patient_id <- "DT051"
# 
# # get treatment regime for selected patient
# Treatment_BASE_update %>% filter(Patient.ID == select_patient_id)
# 
# # get CT measurements for selected patient
# data_RECIST_Patients_time %>% filter(Patient.ID == select_patient_id) %>% arrange(Date) %>% print(n = Inf)
# 
# # check final dataset for selected patient
# df_merged %>% filter(Patient.ID == select_patient_id) %>% 
#     arrange(Date) %>% 
#     select(Date, Start.Date.x, End.Date.x, Progression.x, Treatment_new_final) %>% 
#     arrange(Date) %>% print(n = Inf)
# 
# ##########################################################################################
# # check all patients
# dim_df_original <- data_RECIST_Patients_time %>% 
#     group_by(Patient.ID) %>% 
#     summarise(rows = n()) #filter(Patient.ID == select_patient_id) %>% arrange(Date) %>% print(n = Inf)
# dim_df_original$dataset <- "original"
# 
# # check final dataset for selected patient
# dim_df_merged <- df_merged %>% group_by(Patient.ID) %>% summarise(rows = n())
# dim_df_merged$dataset <- "derived"
# 
# df_combined <- merge(dim_df_original, dim_df_merged, by = "Patient.ID")
# df_combined %>% group_by(Patient.ID) %>% mutate(diff = rows.x - rows.y) %>% filter(diff!= 0)


