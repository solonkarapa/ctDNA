
main_path <- "/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/"

##################################################################   
########################## Load files ############################
################################################################## 
# data
dataset_type <- "full" #"original", "full"

if(dataset_type == "original"){
    load(paste0(main_path, "validation_data.Rdata"))
} else {
    load(paste0(main_path, "validation_data_full.Rdata"))
}

# load trained models or train them below
#load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/models_Ant.Rdata")
load("/Users/solon/Cloud-Drive/Projects/ctDNA_original/ctDNA/validation/models_Ant_full.Rdata")

# exploratory
# should be 146 patients
#data_train_CT %>% summarise(n()) %>% arrange(`n()`)

# 1st stage model
prior_custom <- c(set_prior("normal(0, 100)", class = "b"), # fixed effects prior 
                  set_prior("cauchy(0, 2)", class = "sd"), # random effects sd prior
                  set_prior("lkj(1)", class = "cor")) # prior for correlation coef for random effects

fit1_Ant <- brm(ichorCNA_tr ~ time_ichor + ER.status + Her2.status + Treatment_duration +
                          (1 + time_ichor | Patient.ID),
                      data = df_ichor_Ant, 
                      family = gaussian(), 
                      prior = prior_custom,
                      warmup = 2000,
                      iter = 10000,
                      chains = 2,
                      cores = getOption("mc.cores", 2), 
                      control = list(adapt_delta = 0.99, max_treedepth = 12))

model_1st_stage <- fit1_Ant


#library(coda) # Summary Statistics For MCMC
#p <- as.mcmc(model_1st_stage)
#summary(p) # check time-series SE col

summary(model_1st_stage)

# filter Patient.IDs to correspond to stage 1 model
data_train_CT_filtered <- df_RECIST_Ant %>% 
    filter(Patient.ID %in% unique(model_1st_stage$data$Patient.ID))

#length(unique(model_1st_stage$data$Patient.ID))                                                     
#length(unique(data_train_CT_filtered$Patient.ID))                           

# extract random effects
data_extract_model_1st_stage <- as.data.frame(model_1st_stage$fit@sim$samples[[1]])
#names(data_extract_model_1st_stage)

# get posterior means for random effects
r_intercept <- data_extract_model_1st_stage %>% select(r_Patient.ID.339.Intercept. : r_Patient.ID.4036.Intercept.) %>% summarise(estim_inter = colMeans(.))
r_slope <- data_extract_model_1st_stage %>% select(r_Patient.ID.339.time_ichor.: r_Patient.ID.4036.time_ichor.) %>% summarise(estim_slope = colMeans(.))

combine_model_1st_stage <- cbind(Patient.ID = unique(model_1st_stage$data$Patient.ID), r_intercept, r_slope)

# combine datasets
data_train_CT_final <- merge(data_train_CT_filtered, combine_model_1st_stage, by = "Patient.ID")

prior_custom <- c(set_prior("normal(0, 100)", class = "b"), # fixed effects prior 
                  set_prior("cauchy(0, 2)", class = "sd"))


fit2_CT_Ant <- brm(Progression ~ time + ER.status + Her2.status + estim_inter + estim_slope + (1 | Patient.ID),
                            data = data_train_CT_final, 
                            family = bernoulli(link = "logit"), 
                            prior = prior_custom,
                            warmup = 5000,
                            iter = 100000,
                            chains = 2,
                            cores = getOption("mc.cores", 2),
                            control = list(adapt_delta = 0.95, max_treedepth = 12))

summary(fit2_CT_Ant)

############
# calculate posterior predictive distribution  
post_pred_ctDNA <- posterior_epred(fit2_CT_Ant, 
                                   newdata = data_train_CT_final, 
                                   re_formula = NULL,
                                   allow_new_levels = T, 
                                   sample_new_levels = "gaussian")

############
post_pred_avg <- colMeans(post_pred_ctDNA)

df_new_preds_final <- cbind(data_train_CT_final, pred_prob = post_pred_avg)

############
setwd(paste0(main_path))

# models to save
#save(fit1_Ant, fit2_CT_Ant, file = "models_Ant_full.Rdata")

############ evaluation
library(pROC)
roc_val <- roc(Progression ~ pred_prob, data = df_new_preds_final) # calculate stats for ichor

plot(roc_val)

auc(roc_val)
ci.auc(roc_val)
