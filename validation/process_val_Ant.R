
library(dplyr)

##############################################################################
# load original data
load("/Users/solon/Cloud-Drive/Projects/ctDNA/pre-process:data/data/Antwerp/antwerpData.Rdata")

# load treatment data
#load("/Users/solon/Cloud-Drive/Projects/ctDNA/pre-process:data/data/Antwerp/Treatments_simplified_Antwerp.Rdata")

load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/treatments_simplified_updated_Ant.Rdata")

#####
# helper fun
asinTransform <- function(p){ asin(sqrt(p)) }

######
ichorCNA <- ichorCNA %>% rename(Patient.ID = Patient_ID)

##############################################################################
# merge ichorCNA with Treatments 
data_ichorCNA_prelim <- merge(ichorCNA, Treatments_simplified_update, by = "Patient.ID")

# pre-processing 
df_generic <- data_ichorCNA_prelim %>% 
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
ichorCNA_treatments <- df_merged %>% 
    mutate(Primary_tumor = ifelse(is.na(Primary_tumor.x), Primary_tumor.y, Primary_tumor.x),
           #time = ifelse(is.na(time.x), time.y, time.x),
           ichorCNA = ifelse(is.na(OptimichorCNA.x), OptimichorCNA.y, OptimichorCNA.x)) %>%
    rename(Date.ichor = Date) %>%
    mutate(ichorCNA_tr = asinTransform(ichorCNA/100)) %>%
    dplyr::select(Patient.ID, Date.ichor, ichorCNA_tr, Treatment_new_final, Treatment_duration, Primary_tumor) %>%
    distinct(Date.ichor, .keep_all = TRUE) %>%
    mutate(ER.status = # create ER.status
               case_when((Primary_tumor == "ER+" | Primary_tumor == "ER+HER2+") ~ "ER+",
                         (Primary_tumor == "TN" | Primary_tumor == "TN (IBC)" | Primary_tumor == "HER2+") ~ "ER-",
                         TRUE ~ "missing"),
           Her2.status = # create Her2.status
               case_when((Primary_tumor == "HER2+" | Primary_tumor == "ER+HER2+") ~ "Her2+",
                         (Primary_tumor == "TN" | Primary_tumor == "TN (IBC)" | Primary_tumor == "ER+") ~ "Her2-",
                         TRUE ~ "missing"))

##############################################################################
# earliset CT scan
RECIST2 <- RECIST %>% 
    rename(Patient.ID = Patient_ID) %>%
    group_by(Patient.ID) %>%
    mutate(Earliest.Scan = min(Date))

# merge ichor and RECIST
df_ichor_RECIST <- merge(ichorCNA_treatments, RECIST2, by = "Patient.ID") %>%
    mutate(time_ichor = as.numeric(difftime(Date.ichor, Earliest.Scan, units = "weeks"))/52.14,
           ER.status = ifelse(ER.status == "ER+", 2, 1),
           Her2.status = ifelse(Her2.status == "Her2-", 1, 2)) %>%
    distinct(Patient.ID, Date.ichor, .keep_all = T) %>%
    mutate(Treatment_duration = Treatment_duration/52.14) #%>%
    #dplyr::select(Patient.ID, Date.ichor, ichorCNA_tr, Treatment_new_final, Treatment_duration, ER.status, Her2.status, time_ichor)

df_ichor_Ant <- df_ichor_RECIST %>% 
    dplyr::select(Patient.ID, Date.ichor, ichorCNA_tr, Treatment_new_final, Treatment_duration, ER.status, Her2.status, time_ichor) %>%
    filter(Treatment_new_final != "Trial Drug" & Treatment_new_final != "off-treatment") # remove "off-treatment" and "Trial Drug" - it's just a place holder for the Ant dataset

##############################################################################

df_RECIST_Ant <- merge(ichorCNA_treatments, RECIST2, by = "Patient.ID") %>%
    ungroup() %>%
    mutate(time = as.numeric(difftime(Date, Earliest.Scan, units = "weeks"))/52.14,
           Progression = ifelse(Progression == "YES", 1, 0),
           ER.status = ifelse(ER.status == "ER+", 2, 1),
           Her2.status = ifelse(Her2.status == "Her2-", 1, 2)) %>%
    distinct(Patient.ID, Date, .keep_all = T) %>%
    mutate(Treatment_duration = as.numeric(Treatment_duration)/52.14) %>%
    dplyr::select(Patient.ID, Date, Progression, Treatment_new_final, Treatment_duration, ER.status, Her2.status, time) %>% 
    filter(Treatment_new_final != "Trial Drug" & Treatment_new_final != "off-treatment") # remove "off-treatment" and "Trial Drug" see above for explanation
 
##############################################################################
#setwd("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/")
#save(df_ichor_Ant, df_RECIST_Ant, file = "validation_data.Rdata")



