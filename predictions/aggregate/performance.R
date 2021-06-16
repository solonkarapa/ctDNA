
#################################################################################### 
######################## STEP 1 - load data and models #############################
#################################################################################### 
######################### load test data 
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_test_CT.Rdata")

# number of patients
dim(data_test_CT)[1]

######################### load train data
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_for_2nd_stage_with_rand_effects.Rdata")

######################### load models
# load stage 2 model - ichor 
load("~/Box/PhD/Code/ctDNA/updated/models//model_2nd_stage_ichor.Rdata") # for mac

# load stage 2 model - no ichor
load("~/Box/PhD/Code/ctDNA/updated/models/model_2nd_stage_no_ichor.Rdata") # for mac

#################################################################################### 
####################### STEP 2 - pre-process data ################################## 
####################################################################################
library(dplyr)

df_combine <- left_join(data_test_CT, data_train_CT_final, by = "Patient.ID")   

df_new <- df_combine %>% 
    group_by(Patient.ID) %>%
    select(Patient.ID:Her2.status.x, Treatment_new_final.x, Treatment_duration.x, time.x, estim_inter, estim_slope) %>% 
    rename(Her2.status = Her2.status.x, ER.status = ER.status.x, Progression = Progression.x, 
           Treatment_new_final = Treatment_new_final.x, Treatment_duration = Treatment_duration.x, time = time.x) %>%
    mutate(Treatment_duration = as.numeric(Treatment_duration)) %>%
    distinct(.keep_all = TRUE)

# check NA per column
#df_new %>% ungroup() %>% summarise_all(funs(sum(is.na(.))))

#################################################################################### 
######################## STEP 3 - create predictions ###############################
#################################################################################### 
### Need to maintain this order
models <- c("fit2_CT_ichor",
            "fit2_CT_no_ichor")

list_models <- list(fit2_CT_ichor, 
                    fit2_CT_no_ichor)

library(purrr)
library(brms)

# calculate posterior predictive distribution for each patient within each model
post_pred_dist <- map(list_models, posterior_epred, newdata = df_new, re_formula = NULL, allow_new_levels = F)

# calculate average of posterior predictive distribution
post_pred_avg <- map(post_pred_dist, colMeans)

# add model names
names(post_pred_avg) <- models

# convert to data frame
library(plyr)
df <- ldply(post_pred_avg, data.frame)
detach("package:plyr")

# rename cols 
df_res <- df %>% rename(model = .id, predictions = X..i..)

outcome <- as.numeric(as.factor(df_new$Progression)) - 1

df_res$outcome <- outcome

#################################################################################### 
######################## STEP 3 - evaluate predictions (collectively) ##############
#################################################################################### 

############################################################  
############################## AUC #########################
############################################################
library(yardstick)

# calculate AUC
df <- df_res %>%
    group_by(model) %>% 
    mutate(fitted = 1 - predictions) %>% 
    roc_auc(truth = as.factor(outcome), fitted) %>% arrange(desc(.estimate))
df

#########
library(pROC)

# put all together using pROC package 
ir <- group_split(df_res %>% group_by(model))

df_auc <- map(ir, roc, response = outcome, predictor = predictions)

names(df_auc) <- models

# 95% CI 
# map(df_auc, ci.auc)

g.list <- ggroc(df_auc)  # see https://rdrr.io/cran/pROC/man/ggroc.html

g.list + 
    geom_line(size = 1.1) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed", size = 0.4) +
    coord_fixed() +
    labs(col = "Model") +
    scale_color_discrete(labels = c("with ctDNA", "without ctDNA")) +
    theme_classic() 

############################################################  
############################## Brier Score #################
############################################################
#df_res %>% group_by(model) %>% summarise(BRIER = mean((outcome - predictions)^2)) %>% arrange(BRIER)

############################################################  
############################## ICI #########################
############################################################
#res_cal %>% group_by(model) %>% summarise(ICI = mean(abs(predictions - fitted))) #%>% 
# arrange(ICI)

