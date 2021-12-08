
library(dplyr)

### load ichorCNA data
load("~/Library/CloudStorage/Box-Box/PhD/Code/ctDNA/DETECT.MODEL.FILES.RData") # 2nd UPDATED DATASET

# check NA per column
ichorCNA %>% summarise_all(funs(sum(is.na(.))))

##################################################################   
################# transform ichorCNA ############################# 
##################################################################   
# arcis transformation
# http://strata.uga.edu/8370/rtips/proportions.html
asinTransform <- function(p) { asin(sqrt(p)) }

ichorCNA_tr <- ichorCNA %>% mutate(ichorCNA_tr = asinTransform(OptimichorCNA/100))

str(ichorCNA_tr)

##################################################################   
################# Merge datasets ################################# 
################################################################## 
# merge ichor with clinical data
data_ichorCNA <- merge(ichorCNA_tr, Clinical, by = "Patient.ID")

##################################################################   
################# Merge datasets ################################# 
################################################################## 
# intersection of patients
length(unique(data_ichorCNA$Patient.ID)) # unique Patient.ID in ichorCNA_tr
length(unique(RECIST$Patient.ID)) # unique Patient.ID in RECIST
length(intersect(data_ichorCNA$Patient.ID, RECIST$Patient.ID)) # intersection

# create Earliest.Scan variable
RECIST_1 <- RECIST %>% group_by(Patient.ID) %>% mutate(Earliest.Scan = min(Date))

# merge
data_ichorCNA_Patients <- merge(data_ichorCNA, RECIST_1, by = "Patient.ID")

# creation time variable
data_ichorCNA_Patients_time <- data_ichorCNA_Patients %>% 
    group_by(Patient.ID) %>% 
    mutate(time = difftime(Date.x, Earliest.Scan, units = "weeks")) %>% # time difference since first CT scan (in weeks)
    select(Patient.ID, Date.x, ichorCNA_tr, ER.status, Her2.status, time) %>% 
    rename(Date = Date.x) %>% 
    distinct()

#save(data_ichorCNA_Patients_time, file = "pre_process_ichor.Rdata")

##########################################################################################
##########################################################################################
##########################################################################################
# check all patients
# dim_df_original <- data_ichorCNA_Patients_time %>% 
#     group_by(Patient.ID) %>% 
#     summarise(rows = n()) #filter(Patient.ID == select_patient_id) %>% arrange(Date) %>% print(n = Inf)
# dim_df_original$dataset <- "original"
# 
# # check final dataset for selected patient
# dim_df_mergred <- ichorCNA %>% group_by(Patient.ID) %>% summarise(rows = n())
# dim_df_mergred$dataset <- "derived"
# 
# 
# df_combined <- merge(dim_df_original, dim_df_mergred, by = "Patient.ID")
# df_combined %>% group_by(Patient.ID) %>% mutate(diff = rows.x - rows.y) %>% filter(diff != 0)
