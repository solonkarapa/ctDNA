
############################################
############################################ 
############################################
path_res <-"/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/predictions/output/"

files <- list.files(path_res)
df_final <- data.frame()

for(j in 1:length(files)){
    
    load(paste0(path_res, files[j]))
    
    df_final <- rbind(df_final, df_new_preds_final)
}

str(df_final)

# sample-predictions 
dim(df_final)

# unique patients
length(unique(df_final$Patient.ID))

df_final %>% group_by(Patient.ID) %>% summarise(n()) %>% reframe(range(`n()`))

######################## 
library(pROC)
roc_val <- roc(Progression ~ pred_prob, data = df_final) # calculate stats for ichor

plot(roc_val)

auc(roc_val)
ci.auc(roc_val)

#df_final %>% ggplot(.) +
#    geom_boxplot(aes(x = pred_prob, y = Progression))

