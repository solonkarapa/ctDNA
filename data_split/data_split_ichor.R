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


