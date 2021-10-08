library(ggplot2)
library(dplyr)

#################################################################################### 
######################## STEP 1 - load data and models #############################
#################################################################################### 
######################### load test data 
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_test_CT.Rdata")

# load CT dataset - three patients
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_dynamic_pred_CT.Rdata") # updated dataset mac

# keep last CT scan per patient
last_CT_three_pat <- data_ind_pat_CT %>% group_by(Patient.ID) %>% slice_max(time)

# number of patients
dim(data_test_CT)[1]
dim(last_CT_three_pat)[1]

df_all_pat <- rbind(last_CT_three_pat, data_test_CT)

######################### load train data
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_for_2nd_stage_with_rand_effects.Rdata")

# remove problematic cols
data_train_CT_final2 <- data_train_CT_final %>% 
    mutate(Treatment_duration = as.numeric(Treatment_duration)) %>%
               select(-c(Date, Progression))

######################### load train data three patients 
load("~/Box/PhD/Code/ctDNA/updated/predictions/individual_preds_ichor_timepoints/three_patients/df_final_ichor_timepoints_three_pat.Rdata")

# same col names 
df_final2 <- df_final %>% select(colnames(data_train_CT_final2))

data_CT_all <- rbind(df_final, data_train_CT_final2)

######################### load models
# load stage 2 model - ichor 
load("~/Box/PhD/Code/ctDNA/updated/models/model_2nd_stage_ichor.Rdata") # for mac

# load stage 2 model - no ichor
load("~/Box/PhD/Code/ctDNA/updated/models/model_2nd_stage_no_ichor.Rdata") # for mac

#################################################################################### 
####################### STEP 2 - pre-process data ################################## 
####################################################################################
df_combine <- left_join(df_all_pat, data_CT_all, by = "Patient.ID")   

df_new <- df_combine %>% 
    group_by(Patient.ID) %>%
    select(Patient.ID:Her2.status.x, Treatment_new_final.x, Treatment_duration.x, time.x, estim_inter, estim_slope) %>% 
    rename(Her2.status = Her2.status.x,
           ER.status = ER.status.x, 
           Treatment_new_final = Treatment_new_final.x, 
           Treatment_duration = Treatment_duration.x, 
           time = time.x) %>%
    mutate(Treatment_duration = as.numeric(Treatment_duration)) %>%
    distinct(.keep_all = TRUE)

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
post_pred_dist <- map(list_models, posterior_epred, newdata = df_new, re_formula = NULL, allow_new_levels = T)

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
#########################  CA153
load("~/Box/PhD/Code/ctDNA/updated/DETECT_CA153.Rdata")

DETECT_CA153_2 <- DETECT_CA153 %>% 
    rename(Date = Date.CA, Patient.ID = DETECT.ID) %>% # rename cols 
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) # fix Date format

CA153 <- full_join(data_test_CT, DETECT_CA153_2, by = c("Patient.ID")) %>% 
    group_by(Patient.ID) %>% 
    mutate(diff_in_dates = Date.x - Date.y) %>% 
    slice_min((abs(diff_in_dates)))

######################## 
library(pROC)
CA_roc <- roc(Progression ~ Units.CA, data = CA153) # calculate stats for ichor
CA_coord_30 <- coords(CA_roc, 30, input = "threshold", ret = c("precision", "recall", "specificity", "sensitivity"))
CA_coord_30
CA_coord_35 <- coords(CA_roc, 35, input = "threshold", ret = c("precision", "recall", "specificity", "sensitivity"))
CA_coord_35

#DETECT_CA153 %>% mutate(prog_new = ifelse(Units.CA >= 30, 1, 0)) %>%
#    summarise(sum(prog_new), sum(Progression.CA == "YES"), median(Units.CA), mean(Units.CA), range(Units.CA))

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
# sensitivity at given specificity 
df_res_ichor <- df_res %>% filter(model == "fit2_CT_ichor")

set.seed(1)
se_df_ichor <- data.frame(ci.se(outcome ~ predictions, data = df_res_ichor, specificities = seq(0.7, 0.9, by = 0.1)))
round(se_df_ichor %>% relocate(X50.), 2)

ichor <- se_df_ichor %>% 
    rename(low_CI = X2.5., mean = X50., high_CI = X97.5.) %>% 
    mutate(model = "ichor") %>%
    tibble::rownames_to_column(., "spec")

df_res_no_ichor <- df_res %>% filter(model == "fit2_CT_no_ichor")

se_df_no_ichor <- data.frame(ci.se(outcome ~ predictions, data = df_res_no_ichor, specificities = seq(0.7, 0.9, by = 0.1)))
round(se_df_no_ichor %>% relocate(X50.), 2)

no_ichor <- se_df_no_ichor %>% 
    rename(low_CI = X2.5., mean = X50., high_CI = X97.5.) %>% 
    mutate(model = "without ichor") %>%
    tibble::rownames_to_column(., "spec")

sens_models <- rbind(ichor, no_ichor)
sens_models

save(spec_models, file = "sens_models.Rdata")

#########
# put all together using pROC package 
ir <- group_split(df_res %>% group_by(model))

df_auc <- map(ir, roc, response = outcome, predictor = predictions)

names(df_auc) <- models

# 95% CI 
# map(df_auc, ci.auc)

g.list <- ggroc(df_auc)  # see https://rdrr.io/cran/pROC/man/ggroc.html

CA_coord_30$threshold <- "30"
CA_coord_35$threshold <- "35"

CA_coord <- rbind(CA_coord_30, CA_coord_35)

# ROC - both models
g.list + 
    geom_line(size = 1.1) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed", size = 0.4) +
    coord_fixed() +
    labs(col = "Model") +
    scale_color_discrete(labels = c("with ctDNA", "without ctDNA")) +
    geom_point(data = CA_coord, aes(x = specificity, y = sensitivity, shape = as.factor(threshold)), size = 2, inherit.aes = FALSE) +
    labs(shape = "CA 15-3 threshold") +
    #annotate("point", x = CA_coord_30[[3]], y = CA_coord_30[[4]], colour = "black", size = 2) + 
    #annotate("point", x = CA_coord_35[[3]], y = CA_coord_35[[4]], colour = "black", size = 2, shape = "triangle") + 
    #geom_segment(aes(x = 0.65, y = 0.5, xend = 0.57, yend = 0.6),
    #             arrow = arrow(length = unit(0.3, "cm"))) + 
    #annotate("text", x = 0.5588235, y = 0.6, colour = "black", label = "Ca15-3", size = 5) + 
    theme_classic() 

