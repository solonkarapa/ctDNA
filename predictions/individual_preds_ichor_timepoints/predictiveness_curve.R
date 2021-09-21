
library(ggplot2)
library(dplyr)

############## load data
load("~/Box/PhD/Code/ctDNA/updated/predictions/individual_preds_ichor_timepoints/predictions_ichor_timepoints.Rdata")

############## PREDICTIVENESS CURVE
# https://academic.oup.com/aje/article/167/3/362/132001
# data manipulation
comb_probs <- combine_all %>% group_by(Progression) %>% arrange(median) 
comb_ichor <- combine_all %>% group_by(Progression) %>% arrange(ichorCNA_tr) %>% 
    mutate(ichor = (sign(ichorCNA_tr)*sin(ichorCNA_tr)^2)) # transform back to original ichorCNA scale
    
# PREDICTIVENESS plot 1
# ggplot() +
#     geom_point(data = comb_probs, aes(x = order(median)/nrow(comb_probs), y = median, group = Progression, col = Progression)) +
#     geom_point(data = comb_ichor, aes(x = order(ichor)/nrow(comb_ichor), y = ichor, group = Progression, col = Progression), shape = "star") +
#     facet_grid(. ~ Progression) +
#     labs(x = "Percentile", y = "") +
#     geom_vline(xintercept = 0.9)


# PREDICTIVENESS plot 2
ggplot() +
    geom_point(data = comb_probs, aes(x = order(median)/nrow(comb_probs), y = median)) +
    geom_point(data = comb_ichor, aes(x = order(ichor)/nrow(comb_ichor), y = ichor), shape = "star") +
    #facet_grid(. ~ Progression) +
    labs(x = "Percentile", y = "") #+
    #geom_vline(xintercept = 0.9) +
    #geom_vline(xintercept = 0.1) +
    #geom_hline(yintercept = 0.07)
    

############## Precision-recall curve
##
library(pROC)
dynamic <- roc(Progression ~ median, data = comb_probs) # calculate stats for model
dynamic_coord <- coords(dynamic, ret = c("precision", "recall"))
dynamic_coord$model <- "dynamic"

ichor <- roc(Progression ~ ichor, data = comb_ichor) # calculate stats for ichor
ichor_coord <- coords(ichor, ret = c("precision", "recall"))
ichor_coord$model <- "ichor"

df_plot <- rbind(dynamic_coord, ichor_coord)

df_plot %>%
    ggplot(.) +
    geom_point(aes(x = recall, y = precision, col = model)) +
    geom_line(aes(x = recall, y = precision, col = model)) +
    geom_point(aes(x = 0.7207119, y = 0.2672791))



                          