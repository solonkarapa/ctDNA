
library(dplyr)

### load ichorCNA data
load("~/Library/CloudStorage/Box-Box/PhD/Code/ctDNA/DETECT.MODEL.FILES.RData") # 2nd UPDATED DATASET

#### check and remove NAs
# check NA per column
RECIST %>% summarise_all(funs(sum(is.na(.))))

### Correct errors in a few entries 
# from PD to SD
RECIST_1 <- RECIST %>% 
    filter(
        (Patient.ID == "DT163" & Date == "2015-08-15") | 
            (Patient.ID == "DT183" & Date == "2018-12-16") |
            (Patient.ID == "DT200" & Date == "2019-04-15") | 
            #(Patient.ID == "DT208" & Date == "2015-04-16") |
            (Patient.ID == "DT232" & Date == "2017-06-22") |
            (Patient.ID == "DT271" & Date == "2019-04-23") |
            (Patient.ID == "DT274" & Date == "2017-07-08") |
            (Patient.ID == "DT326" & Date == "2019-01-25")
    ) %>%
    mutate(CT = replace(CT, CT == "PD", "SD"),
           Progression = replace(Progression, Progression == "YES", "NO"))

# from SD to PD
RECIST_2 <- RECIST %>% 
    filter( 
        #(Patient.ID == "DT208" & Date == "2015-04-16") |
            (Patient.ID == "DT213" & Date == "2015-06-04") |
            (Patient.ID == "DT268" & Date == "2019-03-16") |
            (Patient.ID == "DT275" & Date == "2016-11-24") |
            (Patient.ID == "DT275" & Date == "2016-12-21") |
            (Patient.ID == "DT331" & Date == "2019-10-10") 
    ) %>%
    mutate(CT = replace(CT, CT == "SD", "PD"),
           Progression = replace(Progression, Progression == "NO", "YES"))

# from SD to PR
RECIST_3 <- RECIST %>% 
    filter( 
        (Patient.ID == "DT206" & Date == "2016-11-18") |
            (Patient.ID == "DT255" & Date == "2015-12-04") |
            (Patient.ID == "DT255" & Date == "2016-02-25")
    ) %>%
    mutate(CT = replace(CT, CT == "SD", "PR"),
           Progression = replace(Progression, Progression == "NO", "NO"))

# merge again
comb <- rbind(RECIST_1, RECIST_2, RECIST_3)

df <- left_join(RECIST, comb, by = c("Patient.ID", "Date")) %>%
    mutate(CT = if_else(is.na(CT.y), CT.x, CT.y)) %>%
    mutate(Progression = if_else(is.na(Progression.y), Progression.x, Progression.y)) %>%
    select(Patient.ID, Date, CT, Progression)

# remove 2 additional CT scans            
RECIST <- df %>% 
    filter(!(Patient.ID == "DT248" & Date == "2018-08-28") | 
               !(Patient.ID == "DT268" & Date == "2019-08-01"))

    ################### check with latest dataset (December 2021)
    # # load dataset
    # RECIST_UPDATE_Dec_21 <- read.delim("/Users/work/Library/CloudStorage/Box-Box/PhD/Code/ctDNA/RECIST_UPDATE.txt")
    # RECIST_UPDATE_Dec_21 <- RECIST_UPDATE_Dec_21 %>% mutate(Date = as.Date(Date))
    # # merged datasets
    # data_merged <- merge(RECIST, RECIST_UPDATE_Dec_21, by = c("Patient.ID", "Date"))
    # # should be empty
    # data_merged %>% mutate(aggreement = CT.x == CT.y) %>% filter(aggreement == FALSE)

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

#setwd("/Users/work/Library/CloudStorage/Box-Box/PhD/Code/ctDNA/updated")
#save(data_RECIST_Patients_time, file = "pre_process_RECIST.Rdata")
