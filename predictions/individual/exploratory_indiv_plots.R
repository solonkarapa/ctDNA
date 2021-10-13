library(dplyr)
library(ggplot2)

# load data
load("~/Box/PhD/Code/ctDNA/updated/data_split/data_ichor_ind.Rdata") # updated dataset mac

subject <- unique(df_train_ichor_ind$Patient.ID)

subject

load("~/Box/PhD/Code/ctDNA/updated/data_split/data_dynamic_pred_CT.Rdata") # updated dataset mac

unique(data_ind_pat_CT$Patient.ID)

cbp2 <- rev(c("#000000", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#CC79A7", "#0072B2", "#D55E00"))

# select patients for dynamic predictions
#Patients_to_plot <- c("DT331", "DT171", "DT232", "DT343", "DT278", "DT227")
Patients_to_plot <- c("DT158", "DT213", "DT343")

new_labels <- c("A", "B", "C")
names(new_labels) <- sort(Patients_to_plot) 

df_train_ichor_ind %>% 
    filter(Patient.ID %in% Patients_to_plot) %>%
    ggplot(.) + 
    geom_point(aes(x = time_ichor, y = ichorCNA_tr, group = factor(Patient.ID)), alpha = 0.6) +
    geom_smooth(aes(x = time_ichor, y = ichorCNA_tr),  method = "lm", colour = "black") +
    #facet_wrap(. ~ Patient.ID) +
    facet_wrap(. ~ Patient.ID, labeller = labeller(Patient.ID = new_labels)) +
    geom_point(data = data_ind_pat_CT %>% filter(Patient.ID %in% Patients_to_plot),  
               aes(x = time, y = as.numeric(as.factor(Progression)) - 1, group = factor(Patient.ID),
                   col = factor(Progression)), shape = 8, size = 4) +
    theme_linedraw(12) +
    theme(strip.background = element_rect(fill = "white")) +
    theme(strip.text.x = element_text(color = "black")) +
    scale_colour_manual(values = cbp2) + 
    labs(col = "Progression") + 
    #theme(legend.position = "none") +
    labs(y = "ctDNA", x = "Time since first CT scan (years)")