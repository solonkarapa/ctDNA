
library(dplyr)

### load ichorCNA data
load("~/Box/PhD/Code/ctDNA/DETECT.MODEL.FILES.RData") # 2nd UPDATED DATASET

#### check and remove NAs
# check NA per column
RECIST %>% summarise_all(funs(sum(is.na(.))))

### Remove NA values 
indicator_NAs <- which(!is.na(RECIST$Progression))

RECIST_clean <- RECIST[(indicator_NAs), ]

# check NA per column - new dataset
RECIST_clean %>% summarise_all(funs(sum(is.na(.))))

# unique patients 
length(unique(RECIST_clean$Patient.ID))

##################################################################   
################# Merge datasets ################################# 
################################################################## 
# merge RECIST with clinical data
data_RECIST <- merge(RECIST_clean, Clinical, by = "Patient.ID")

# create Earliest.Scan variable
data_RECIST_1 <- data_RECIST %>% group_by(Patient.ID) %>% mutate(Earliest.Scan = min(Date))

# creation time variable
data_RECIST_Patients_time <- data_RECIST_1 %>% 
    group_by(Patient.ID) %>% 
    mutate(time = difftime(Date, Earliest.Scan, units = "weeks"))

#save(data_RECIST_Patients_time, file = "pre_process_RECIST.Rdata")
