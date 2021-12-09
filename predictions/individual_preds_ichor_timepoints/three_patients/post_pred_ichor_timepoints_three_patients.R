

############################################
############################################ 
############################################ 
library(dplyr)
#library(ggplot2)

############################################ 
############################################ 
############################################ 
# load data
main_path <- c("/Users/work/Library/CloudStorage/Box-Box/PhD/Code/ctDNA/updated/")

load(paste0(main_path, "data_split/data_ichor_ind.Rdata")) # updated dataset mac

subject <- unique(df_train_ichor_ind$Patient.ID)

subject

############################################ 
############################################ 
############################################ 
# load helper fun to summarise posterior
source(paste0(main_path, "predictions/helper_funs/summary_posterior.R"))

# specify the quantiles of the posterior
quantiles = c(0.05, 0.5, 0.95)

############################################ 
############################################ 
############################################ 
# load results
df_final <- data.frame()
df_patients_final <- data.frame()
df_to_merge_plot <- data.frame()

for (i in 1:length(subject)) { 
    
    m <- subject[i]
    print(m)
    
    #
    k <- paste0(main_path, "predictions/individual_preds_ichor_timepoints/three_patients/output_pred_ichor_timepoints_three_patients/output_pred_", m, ".RData")
    
    load(k)
    
    # dynamic preds 
    pred_distrib_ctDNA <- apply(post_pred_ctDNA, MARGIN = 2, summary_fun, quantiles = quantiles)
    pred_distrib_df_ctDNA <- data.frame(t(pred_distrib_ctDNA))
    colnames(pred_distrib_df_ctDNA) <- c("low", "median", "high", "mean") 
    pred_distrib_df_ctDNA$type <- "Dynamic pred"
    
    # combined
    df <- df_new_preds %>% filter(Patient.ID == m)
    combine_all <- cbind(df, pred_distrib_df_ctDNA)
    
    df_final <- rbind(df_final, combine_all)
    
    df_patients_final <- rbind(df_patients_final, df_new_preds)
    
    df_to_merge_plot <- rbind(df_to_merge_plot, data_new_stage1)
}

# set directory where the files will be saved
setwd(paste0(main_path, "predictions/individual_preds_ichor_timepoints/three_patients"))
#save(df_patients_final, file = "df_patients_final_projected.Rdata")
#save(df_to_merge_plot, file = "df_to_merge_plot_projected.Rdata")
#save(df_final, file = "df_final_ichor_timepoints_three_pat.Rdata")

# load CT dataset
# load("~/Box/PhD/Code/ctDNA/updated/data_split/data_dynamic_pred_CT.Rdata") #
# 
# # ###### Plot for one subject
# subjects_list <- unique(df_final$Patient.ID)
# subj <- 1
# #
# # library(ggnewscale)
# # pretty_labels <- function(x) paste0("t:", x)
# #
#  df_final %>%
#      filter(Patient.ID == subjects_list[subj]) %>%
#      ggplot(.) +
#      scale_colour_grey(start = 0.1, end = 0.5) +
#      coord_cartesian(ylim = c(0, 1)) +
# #     #geom_segment(aes(y = low, yend = high, xend = time, x = time, col = type), size = 1.5) +
#      geom_linerange(aes(x = time,  ymin = low, ymax = high, col = type), size = 1.7) +
#      geom_point(aes(x = time, y = median, col = type), size = 2.2) +
# #     #geom_errorbar(data = prob_trial, aes(ymin = Q_2.5, ymax = Q_97.5, x = time), width = .03) +
# #     #geom_point(aes(x = time, y = mean, col = type))  +
# #     #facet_grid(. ~ round(time, 2), labeller = as_labeller(pretty_labels)) +
# #     labs(title = paste(subjects_list[subj])) +
#      labs(y = "Predicted Probabilty Progression", x = "Years since first CT", col = "Type") +
# #     #ggtitle(paste(subjects_list[i])) +
#      geom_point(aes(x = time_ichor, y = ichorCNA_tr), shape = 2, alpha = 0.8, col = "purple", size = 3) +
# #     #geom_line(aes(x = time, y = ichorCNA_tr), alpha = 0.8, col = "purple", linetype = "dashed") +
# #     geom_point( aes(y = as.numeric(as.factor(Progression)) - 1, x = time, shape = as.factor(Progression)), shape = 3, size = 3) +
#      theme_classic(base_size = 12)
# # 

