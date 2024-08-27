
#main_path <- c("/Users/work/Library/CloudStorage/Box-Box/PhD/Code/ctDNA/updated/")

main_path <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/"

##################################################################   
########################## Load files ############################
################################################################## 
# data
load(paste0(main_path, "data_split/data_train_CT.Rdata")) # train CT dataset

#load("~/Box/PhD/Code/ctDNA/updated/data_split/data_dynamic_pred_CT.Rdata") # train CT dataset

# exploratory
# should be 146 patients
#data_train_CT %>% summarise(n()) %>% arrange(`n()`)

# 1st stage model
#load(paste0(main_path, "models/model_1st_stage.Rdata")) # default
load(paste0(main_path, "models/model_1st_stage_no_treat.Rdata")) # no treatment

library(dplyr)

# rename 1st stage model
model_1st_stage <- fit1_ichor_no_Treatment

summary(model_1st_stage)

# check NA per column
#data_train_CT %>% ungroup() %>% summarise_all(funs(sum(is.na(.))))

##################################################################   
########################## Processing ############################
################################################################## 
# filter Patient.IDs to correspond to stage 1 model
data_train_CT_filtered <- data_train_CT %>% 
    filter(Patient.ID %in% unique(model_1st_stage$data$Patient.ID))

#length(unique(model_1st_stage$data$Patient.ID))                                                     
#length(unique(data_train_CT_filtered$Patient.ID))                           

# extract random effects
data_extract_model_1st_stage <- as.data.frame(model_1st_stage$fit@sim$samples[[1]])
#names(data_extract_model_1st_stage)

# get posterior means for random effects
r_intercept <- data_extract_model_1st_stage %>% select(r_Patient.ID.DT040.Intercept. : r_Patient.ID.DT363.Intercept.) %>% summarise(estim_inter = colMeans(.))
r_slope <- data_extract_model_1st_stage %>% select(r_Patient.ID.DT040.time_ichor.: r_Patient.ID.DT363.time_ichor.) %>% summarise(estim_slope = colMeans(.))

combine_model_1st_stage <- cbind(Patient.ID = unique(model_1st_stage$data$Patient.ID), r_intercept, r_slope)

# combine datasets
data_train_CT_final <- merge(data_train_CT_filtered, combine_model_1st_stage, by = "Patient.ID")

#str(data_train_CT_final)
#length(unique(data_train_CT_final$Patient.ID)) 

#setwd(paste0(main_path, "data_split"))
#save(data_train_CT_final, file = "data_for_2nd_stage_with_rand_effects.Rdata")

# dataset sum stats
#load("~/Box/PhD/Code/ctDNA/updated/data_split/data_for_2nd_stage_with_rand_effects.Rdata")

#data_train_CT_final %>% summarise(number_of_patients = length(unique(Patient.ID)),
#                                  total_scans = n(),
#                                  unique_treatment = length(unique(Treatment_new_final)))

##################################################################   
################# Modelling ###################################### 
##################################################################
library(brms)

#  which priors we can specify for this model
#get_prior(Progression ~ time + ER.status + Her2.status + Treatment_new_final + Treatment_duration + 
#              (1 | Patient.ID), data = data_train_CT_final)

prior_custom <- c(set_prior("normal(0, 100)", class = "b"), # fixed effects prior 
                  set_prior("cauchy(0, 2)", class = "sd"))

fit2_CT_ichor <- brm(Progression ~ time + ER.status + Her2.status + Treatment_new_final + estim_inter + estim_slope + (1 | Patient.ID),
                         data = data_train_CT_final, 
                         family = bernoulli(link = "logit"), 
                         prior = prior_custom,
                         warmup = 5000,
                         iter = 100000,
                         chains = 2,
                         cores = getOption("mc.cores", 2),
                         control = list(adapt_delta = 0.95, max_treedepth = 12))

fit2_CT_no_ichor <- brm(Progression ~ time + ER.status + Her2.status + Treatment_new_final + (1 | Patient.ID),
                        data = data_train_CT_final, 
                        family = bernoulli(link = "logit"), 
                        prior = prior_custom,
                        warmup = 5000,
                        iter = 100000,
                        chains = 2,
                        cores = getOption("mc.cores", 2),
                        control = list(adapt_delta = 0.95, max_treedepth = 12))

fit2_CT_no_Treatment <- brm(Progression ~ time + ER.status + Her2.status + estim_inter + estim_slope + (1 | Patient.ID),
                        data = data_train_CT_final, 
                        family = bernoulli(link = "logit"), 
                        prior = prior_custom,
                        warmup = 5000,
                        iter = 100000,
                        chains = 2,
                        cores = getOption("mc.cores", 2),
                        control = list(adapt_delta = 0.95, max_treedepth = 12))

############################
setwd(paste0(main_path, "models"))

# models to save
#save(fit2_CT_ichor, file = "model_2nd_stage_ichor.Rdata")
#save(fit2_CT_no_ichor, file = "model_2nd_stage_no_ichor.Rdata")
save(fit2_CT_no_Treatment, file = "model_2nd_stage_no_treat.Rdata")

# load fitted model
load(paste0(main_path, "models/model_2nd_stage_ichor.Rdata"))

# reporting Latex
library(xtable)
#str(summary(fit2_CT_ichor))
#xtable(summary(fit2_CT_ichor)$fixed[,c(1, 3, 4)])
#xtable(summary(fit2_CT_ichor)$spec_pars[,c(1, 3, 4)])
#xtable(summary(fit2_CT_ichor)$random$Patient.ID)

##################################################################   
################# Post-processing ################################
##################################################################
#library(coda) # Summary Statistics For MCMC
#p <- as.mcmc(fit2_CT_ichor)
#summary(p)

summary(fit2_CT_ichor)
#summary(fit2_CT_no_ichor)

# export to word 
export_summs(fit2_CT_ichor, error_format = "[{conf.low}, {conf.high}]", to.file = "Word", file.name = "2nd stage.docx")

#plot(fit2_CT_ichor)
#plot(fit2_CT_no_ichor)

# plot some diagnostics of the sampler
#mcmc_plot(fit2_CT_ichor, type = "rhat") # smaller than 1.1 is ok
#mcmc_plot(fit2_CT_no_ichor, type = "rhat") 


