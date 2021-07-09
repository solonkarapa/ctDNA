
##################################################################   
########################## Load data #############################
##################################################################   
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_train_ichor.Rdata") # train IchorCNA dataset

library(dplyr)

# exploratory
# should be 143 patients
#df_train_ichor %>% summarise(n()) %>% arrange(`n()`)
#df_train_ichor %>% ungroup %>% summarise(range(time_ichor))

# check NA per column
#df_train_ichor %>% ungroup() %>% summarise_all(funs(sum(is.na(.))))
#index_missing <- which(is.na(df_train_ichor$Her2.status))
#df_train_ichor[index_missing,]

##################################################################   
################# Modelling ###################################### 
##################################################################
library(brms)

#  which priors we can specify for this model
#get_prior(ichorCNA_tr ~ time_ichor + ER.status + Her2.status + Treatment_new_final + Treatment_duration + (1 + time_ichor | Patient.ID), 
#          data = df_train_ichor)

prior_custom <- c(set_prior("normal(0, 100)", class = "b"), # fixed effects prior 
                  set_prior("cauchy(0, 2)", class = "sd"), # random effects sd prior
                  set_prior("lkj(1)", class = "cor")) # prior for correlation coef for random effects

fit1_ichor <- brm(ichorCNA_tr ~ time_ichor + ER.status + Her2.status + Treatment_new_final + Treatment_duration +
                               (1 + time_ichor | Patient.ID),
                           data = df_train_ichor, 
                           family = gaussian(), 
                           prior = prior_custom,
                           warmup = 2000,
                           iter = 10000,
                           chains = 2,
                           cores = getOption("mc.cores", 2), 
                           control = list(adapt_delta = 0.99, max_treedepth = 12))

setwd("~/Box/PhD/Code/ctDNA/updated/models/")
#save(fit1_ichor, file = "model_1st_stage.Rdata")

##################################################################   
################# Post-processing ################################
##################################################################
#library(coda) # Summary Statistics For MCMC
#p <- as.mcmc(fit1_ichor)
#summary(p) # check time-series SE col

# reporting Latex
#library(xtable)
#str(summary(fit1_ichor))
#xtable(summary(fit1_ichor)$fixed[,1:4])
#xtable(summary(fit1_ichor)$spec_pars)
#xtable(summary(fit1_ichor)$random$Patient.ID[,1:4])

################################ diagnostics, summaries, plots
# summarise output
summary(fit1_ichor)

# plot posterior distributions and chains
#plot(fit1_ichor, pars = "^sd")
#plot(fit1_ichor, pars = "^sigma")

# plot posterior intervals
#mcmc_plot(fit1_ichor)

# plot some diagnostics of the sampler
#mcmc_plot(fit1_ichor, type = "neff")
#mcmc_plot(fit1_ichor, type = "rhat") # smaller than 1.1 is ok

# posterior estimates (median, 95% CI)
#library(tidybayes)
# post_beta_means <- fit1_ichor %>% 
#     recover_types(fit1_ichor) %>% 
#     gather_draws(b_Intercept, b_time_ichor, b_ER.status, b_Her2.status, sigma) %>% 
#     median_hdi() %>% # median and HDI 
#     dplyr::select(1:5)
# 
#post_beta_means

# the ggs function transforms the brms output into a longformat tibble, 
# that we can use to make different types of plots.
# library(ggmcmc)
# fit1_ichor_ggs <- ggs(fit1_ichor) 
# 
# fit1_ichor_ggs %>% 
#     filter(Parameter %in% c("b_Intercept", "b_time", "b_ER.statusERP", "b_Her2.statusHer2P")) %>%
#     ggplot(., aes(x = Iteration, y = value, col = as.factor(Chain))) +
#     geom_line() +
#     geom_vline(xintercept = 1000)+
#     facet_grid(Parameter ~ . , scale  = 'free_y', switch = 'y')+
#     labs(title = "Chain plots", col = "Chains")

# library(stringr)
# library(rebus)
# 
# # slopes df
# rand_slope <- fit1_ichor_ggs %>% 
#     filter(str_detect(Parameter, "r_")) %>%
#     filter(str_detect(Parameter, "time")) %>%
#     filter(!str_detect(Parameter, "cor")) %>%
#     mutate(coef = "random slope")
# 
# # intercept df 
# rand_intercept <- fit1_ichor_ggs %>% 
#     filter(str_detect(Parameter, "r_")) %>%
#     filter(str_detect(Parameter, "Intercept")) %>%
#     filter(!str_detect(Parameter, "cor")) %>%
#     mutate(coef = "random intercept")
# 
# df_rand <- rbind(rand_slope, rand_intercept)
# 
# df_rand %>%
#     ggs_caterpillar(.) + 
#     aes(color = Parameter) + 
#     facet_wrap(coef ~ ., scale = "free_y") +
#     geom_vline(xintercept = 0, linetype = "dashed") +
#     theme_linedraw(12) +
#     theme(legend.position = "None") +
#     theme(strip.background = element_rect(fill = "white"), 
#           strip.text.x = element_text(color = "black"))

# plot slopes
# fit1_ichor_ggs %>% 
#     filter(str_detect(Parameter, "r_")) %>%
#     filter(str_detect(Parameter, "time")) %>%
#     filter(!str_detect(Parameter, "cor")) %>%
#     ggs_caterpillar(.) +
#     geom_vline(xintercept = 0, linetype = "dashed") +
#     theme_linedraw(12)
# # 
# # # plot intercepts 
# fit1_ichor_ggs %>% 
#     filter(str_detect(Parameter, "r_")) %>%
#     filter(str_detect(Parameter, "Intercept")) %>%
#     filter(!str_detect(Parameter, "cor")) %>%
#     ggs_caterpillar(.) +
#     geom_vline(xintercept = 0, linetype = "dashed") +
#     theme_linedraw(12)

