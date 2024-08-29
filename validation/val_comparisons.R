
library(brms)
library(brmstools)
library(ggplot2)
library(dplyr)

####################################################################################
# Antwerp
path_data <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/" # mac
load(paste0(path_data, "validation_data_full.Rdata"))

str(df_ichor_Ant)
str(df_RECIST_Ant)

df_combine_1 <- merge(df_ichor_Ant, df_RECIST_Ant, by = "Patient.ID") %>%
    mutate(time_dist = difftime(Date.ichor, Date, units = "days")) %>%
    group_by(Patient.ID, Date) %>%
    slice_min(abs(time_dist))

df_combine_1 %>% ggplot() +
    geom_boxplot(aes(x = ichorCNA_tr, y = factor(Progression)))

# DETECT
main_path <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/"
load(paste0(main_path, "data_split/data_train_ichor.Rdata")) # train IchorCNA dataset
load(paste0(main_path, "data_split/data_train_CT.Rdata")) # train CT dataset

df_det <- merge(df_train_ichor, data_train_CT, by = "Patient.ID") %>%
    mutate(time_dist = time - time_ichor) %>%
    group_by(Patient.ID) %>%
    slice_min(abs(time_dist))

df_det %>% ggplot() +
    geom_boxplot(aes(x = ichorCNA_tr, y = factor(Progression)))

############################################################################
# compare model estimates
main_path <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/"
# DETECT
load(paste0(main_path, "models/model_1st_stage_no_treat.Rdata")) # no treatment
load(paste0(main_path, "models/model_2nd_stage_no_treat.Rdata")) # no treatment

#load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/models_Ant.Rdata")
load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/models_Ant_full.Rdata")

fixef(fit1_Ant)
fixef(fit1_ichor_no_Treatment)

mean(ranef(fit1_Ant)$Patient.ID[, , 1])
mean(ranef(fit1_Ant)$Patient.ID[, , 2])

mean(ranef(fit1_ichor_no_Treatment)$Patient.ID[, , 1])
mean(ranef(fit1_ichor_no_Treatment)$Patient.ID[, , 2])

fixef(fit2_CT_Ant)
fixef(fit2_CT_no_Treatment)

f_Ant1 <- as.data.frame(fixef(fit1_Ant))
f_Ant1$vars <- rownames(f_Ant1)
rownames(f_Ant1) <- NULL
f_Ant1$dataset <- "Antwerp"
f_Det1 <- as.data.frame(fixef(fit1_ichor_no_Treatment))
f_Det1$vars <- rownames(f_Det1)
rownames(f_Det1) <- NULL
f_Det1$dataset <- "DETECT"

estim_stage1 <- rbind(f_Ant1, f_Det1)

ggplot(estim_stage1) +
    geom_point(aes(x = Estimate, y = vars)) +
    geom_errorbar(aes(x = Estimate, y = vars, xmin = Q2.5, xmax = Q97.5), width = 0.1) +
    facet_grid(dataset ~ .)

f_Ant2 <- as.data.frame(fixef(fit2_CT_Ant))
f_Ant2$vars <- rownames(f_Ant2)
rownames(f_Ant2) <- NULL
f_Ant2$dataset <- "Antwerp"
f_Det2 <- as.data.frame(fixef(fit2_CT_no_Treatment))
f_Det2$vars <- rownames(f_Det2)
rownames(f_Det2) <- NULL
f_Det2$dataset <- "DETECT"

estim_stage2 <- rbind(f_Ant2, f_Det2)

ggplot(estim_stage2) +
    geom_point(aes(x = Estimate, y = vars)) +
    geom_errorbar(aes(x = Estimate, y = vars, xmin = Q2.5, xmax = Q97.5), width = 0.1) +
    facet_grid(dataset ~ .)

#################################
fictitious_data_stage1 <- data.frame(Patient.ID = c("A", "B", "C", "D"),  
                                     time_ichor = c(0.5, 0.5, 0.5, 0.5), 
                                     ER.status = c(2, 2, 1, 1), 
                                     Her2.status = c(1, 2, 1, 2), 
                                     Treatment_duration = c(0.1, 0.1, 0.1, 0.1))

post_pred_ctDNA_DETECT_stage1 <- posterior_epred(fit1_ichor_no_Treatment, 
                                            newdata = fictitious_data_stage1, 
                                            re_formula = NULL,
                                            allow_new_levels = T, 
                                            sample_new_levels = "gaussian")
colnames(post_pred_ctDNA_DETECT_stage1) <- fictitious_data_stage1$Patient.ID
df1 <- as.data.frame(post_pred_ctDNA_DETECT_stage1)
df1$dataset <- "DETECT"
data_long1 <- tidyr::gather(df1, ID, estim, A:D, factor_key = TRUE)

post_pred_ctDNA_Ant_stage1 <- posterior_epred(fit1_Ant, 
                                         newdata = fictitious_data_stage1, 
                                         re_formula = NULL,
                                         allow_new_levels = T, 
                                         sample_new_levels = "gaussian")
colnames(post_pred_ctDNA_Ant_stage1) <- fictitious_data_stage1$Patient.ID
df2 <- as.data.frame(post_pred_ctDNA_Ant_stage1)
df2$dataset <- "Antwerp"
data_long2 <- tidyr::gather(df2, ID, estim, A:D, factor_key = TRUE)

df_stage1 <- rbind(data_long1, data_long2)

df_means_stage1 <- df_stage1 %>% group_by(dataset, ID) %>% summarise(avg = mean(estim))

df_stage1 %>% ggplot() +
    geom_density(aes(x = estim, fill = dataset)) +
    geom_vline(data = df_means_stage1, aes(xintercept = avg, linetype = dataset)) +
    facet_grid(.~ID)

#################################
fictitious_data <- data.frame(Patient.ID = c("A", "B", "C", "D"),  
                              time = c(0.5, 0.5, 0.5, 0.5), 
                              ER.status = c(2, 2, 1, 1), 
                              Her2.status = c(1, 2, 1, 2), 
                              estim_inter = c(0.02, 0.02, 0.02, 0.02), 
                              estim_slope = c(0.01, 0.01, 0.01, 0.01))

post_pred_ctDNA_DETECT <- posterior_linpred(fit2_CT_no_Treatment, 
                                   newdata = fictitious_data, 
                                   re_formula = NULL,
                                   allow_new_levels = T, 
                                   sample_new_levels = "gaussian")

colMeans(post_pred_ctDNA_DETECT)

colnames(post_pred_ctDNA_DETECT) <- fictitious_data$Patient.ID
df1 <- as.data.frame(post_pred_ctDNA_DETECT)
df1$dataset <- "DETECT"
data_long1 <- tidyr::gather(df1, ID, estim, A:D, factor_key = TRUE)

post_pred_ctDNA_Ant <- posterior_linpred(fit2_CT_Ant, 
                                          newdata = fictitious_data, 
                                          re_formula = NULL,
                                          allow_new_levels = T, 
                                          sample_new_levels = "gaussian")

colnames(post_pred_ctDNA_Ant) <- fictitious_data$Patient.ID
df2 <- as.data.frame(post_pred_ctDNA_Ant)
df2$dataset <- "Antwerp"
data_long2 <- tidyr::gather(df2, ID, estim, A:D, factor_key = TRUE)

df_stage2 <- rbind(data_long1, data_long2)

df_means_stage2 <- df_stage2 %>% group_by(dataset, ID) %>% summarise(avg = mean(estim))

df_stage2 %>% ggplot() +
    geom_density(aes(x = estim, fill = dataset)) +
    geom_vline(data = df_means_stage2, aes(xintercept = avg, linetype = dataset)) +
    facet_grid(. ~ ID)

#### compare out-of-sample and in-sample random effects 
df_final2 <- df_final %>% rename(Date = Date.x) # from HPC code output
df_check <- merge(df_final2, data_train_CT_final, by = c("Patient.ID", "Date")) # from fit_models_Ant

df_check %>% ggplot() +
    geom_point(aes(x = estim_inter.x, estim_inter.y)) +
    geom_abline(intercept = 0, slope = 1)

df_check %>% ggplot() +
    geom_point(aes(x = estim_slope.x, estim_slope.y)) +
    geom_abline(intercept = 0, slope = 1)


