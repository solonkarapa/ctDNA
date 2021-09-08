

library(brms)
library(dplyr)
library(ggplot2)
library(ggnewscale)
##################################################################   
########################## Load data #############################
##################################################################  

# training data with ichor 
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_train_ichor.Rdata") # train IchorCNA dataset

######################### load train data used in 2nd stage model
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_for_2nd_stage_with_rand_effects.Rdata")

############################################ 
# load helper fun to summarise posterior
source("~/Box/PhD/Code/ctDNA/updated/predictions/helper_funs/summary_posterior.R")

##################################################################   
################# Load models #################################### 
################################################################### 
# 1st stage model
#load("~/Box/PhD/Code/ctDNA/updated/models/model_1st_stage.Rdata") # for mac

# load stage 2 model - with ichor 
load("~/Box/PhD/Code/ctDNA/updated/models//model_2nd_stage_ichor.Rdata") # for mac
#
#################################################################################### 
####################### STEP 2 - pre-process data ################################## 
####################################################################################
library(dplyr)

df_combine <- left_join(df_train_ichor, data_train_CT_final, by = "Patient.ID")   

df_new <- df_combine %>% 
    group_by(Patient.ID) %>%
    select(Patient.ID:Her2.status.x, Treatment_new_final.x, time_ichor, estim_inter, estim_slope, time, Progression) %>% 
    rename(Her2.status = Her2.status.x, ER.status = ER.status.x, 
           Treatment_new_final = Treatment_new_final.x, time = time_ichor, time_CT = time) %>%
    distinct(.keep_all = TRUE)

# check the posterior_epred function 
# calculate posterior predictive distribution for each patient within each model
post_pred_dist <- posterior_epred(fit2_CT_ichor, newdata = df_new, re_formula = NULL, allow_new_levels = F, subset = 50000:60000)

quantiles = c(0.05, 0.5, 0.95)

pred_distrib_sum <- apply(post_pred_dist, MARGIN = 2, summary_fun, quantiles = quantiles)
pred_distrib_df_sum <- data.frame(t(pred_distrib_sum))
colnames(pred_distrib_df_sum) <- c("low", "median", "high", "mean") 

combine_all <- cbind(df_new, pred_distrib_df_sum)


###### Plot for one subject
subj <- "DT081"

cbp2 <- rev(c("#000000", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#CC79A7", "#0072B2", "#D55E00"))

pretty_labels <- function(x) paste0("timepoint:", x)

combine_all %>% 
    filter(Patient.ID == subj) %>% 
    group_by(time_CT)  %>%
    ggplot(.) + 
    scale_colour_grey(start = 0.1, end = 0.5) +
    coord_cartesian(ylim = c(0, 1)) +
    #geom_segment(aes(y = low, yend = high, xend = time, x = time, col = type), size = 1.5) +
    geom_linerange(aes(x = time,  ymin = low, ymax = high), size = 1.2) + 
    geom_point(aes(x = time, y = median), size = 2.2) +
    #facet_grid(. ~ round(time_CT, 2)) + #labeller = as_labeller(pretty_labels)) +
    #geom_errorbar(data = prob_trial, aes(ymin = Q_2.5, ymax = Q_97.5, x = time), width = .03) +
    #geom_point(aes(x = time, y = mean, col = type))  + 
    labs(title = paste(subj), y = "Predicted Probabilty Progression", x = "Time since first CT scan (years)") + 
    new_scale_color() +
    coord_cartesian(ylim = c(-0.1, 1.1)) + 
    # CT datapoints 
    geom_point(aes(x = time_CT, y = as.numeric(as.factor(Progression)) - 1, group = factor(Patient.ID),
                   col = factor(Progression)), shape = 8, size = 3) +
    geom_vline(aes(xintercept = time_CT, col = factor(Progression))) +
    labs(col = "Progression") + 
    scale_colour_manual(values = cbp2) +
    # ichor datapoints 
    new_scale_color() + 
    geom_point(aes(x = time, y = ichorCNA_tr, group = factor(Patient.ID)), 
               alpha = 0.6, size = 2.1, col = cbp2[5]) +
    geom_line(aes(x = time, y = ichorCNA_tr, group = factor(Patient.ID)), 
              linetype = "dotted", alpha = 0.6, size = 1.1) +
    labs(col = "ctDNA") +
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "ctDNA")) + 
    theme_linedraw(12) #+
    #theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(color = "black")) 

