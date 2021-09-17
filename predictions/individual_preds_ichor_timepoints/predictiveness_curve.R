
library(ggplot2)
library(dplyr)

# PREDICTIVENESS CURVE
# https://academic.oup.com/aje/article/167/3/362/132001
load("~/Box/PhD/Code/ctDNA/updated/predictions/individual_preds_ichor_timepoints/predictions_ichor_timepoints.Rdata")


# data manipulation
comb_probs <- combine_all %>% group_by(Progression) %>% arrange(median) 
comb_ichor <- combine_all %>% group_by(Progression) %>% arrange(ichorCNA_tr) %>% 
    mutate(ichor = (sign(ichorCNA_tr)*sin(ichorCNA_tr)^2)) # transform back to original ichorCNA scale
    
# plot
ggplot() +
    geom_point(data = comb_probs, aes(x = order(median)/nrow(comb_probs), y = median, group = Progression, col = Progression)) +
    geom_point(data = comb_ichor, aes(x = order(ichor)/nrow(comb_ichor), y = ichor, group = Progression, col = Progression), shape = "star") +
    facet_grid(. ~ Progression) +
    labs(x = "Percentile", y = "")





