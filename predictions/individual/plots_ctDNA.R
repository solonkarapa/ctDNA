
############################################
############################################ 
############################################ 
# Multiple color (and fill) scales with ggplot2
# https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/
library(ggnewscale)
library(dplyr)
library(ggplot2)

############################################
############################################ 
############################################ 
# load data
load("~/Box/PhD/Code/ctDNA/updated/predictions/individual/df_final_projected.Rdata") 
load("~/Box/PhD/Code/ctDNA/updated/predictions/individual/df_to_merge_plot_projected.Rdata")

df_comb_plot <- merge(df_to_merge_plot, df_final, by = "Patient.ID")

subject <- unique(df_comb_plot$Patient.ID)

subject

# create ctDNA that have been utilised for predictions (for plotting)
df_comb_plot_split <- df_comb_plot %>% 
    group_by(Patient.ID, time.y) %>% 
    group_split() 

df_final_plot <- data.frame()

for(i in 1:length(df_comb_plot_split)){
    
    df <- df_comb_plot_split[[i]]
    
    most_recent_treat <- df %>%
        filter(time_ichor.x <= time.y) %>% 
        summarise(uniq_treat = unique(Treatment_new_final.x)) %>%
        slice(n())
    
    df_comb_plot_3 <- df %>% 
        group_by(Patient.ID) %>%
        mutate(timepoint_used = ifelse((time_ichor.x <= time.y) & Treatment_new_final.x == most_recent_treat$uniq_treat, 
                                       "YES", "NO")) 
    
    df_final_plot <- rbind(df_final_plot, df_comb_plot_3) 
    }

############################################
############################################ 
############################################ 
# pick a subject
subj <- subject[2]

cbp2 <- rev(c("#000000", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#CC79A7", "#0072B2", "#D55E00"))

pretty_labels <- function(x) paste0("timepoint:", x)


df_final_plot %>% 
    filter(Patient.ID == subj) %>% 
    group_by(time.y) %>%
    filter(time_ichor.x <= time.y) %>%
    ggplot(.) +
    # probabilities
    geom_linerange(aes(x = time.y,  ymin = low, ymax = high, col = type), size = 1.2, col = "black") + 
    geom_point(aes(x = time.y, y = median, col = type), size = 2.2, col = "black") +
    facet_grid(. ~ round(time.y, 2), labeller = as_labeller(pretty_labels)) +
    labs(y = "Predicted Probabilty Progression", x = "Time since first CT scan (years)") +
    new_scale_color() + 
    coord_cartesian(ylim = c(-0.1, 1.1)) +
    # CT datapoints 
    geom_point(aes(x = time.y, y = as.numeric(as.factor(Progression)) - 1, group = factor(Patient.ID),
                   col = factor(Progression)), shape = 8, size = 3) +
    labs(col = "Progression") + 
    scale_colour_manual(values = cbp2) +
    # ichor datapoints 
    new_scale_color() + 
    geom_point(aes(x = time_ichor.x, y = ichorCNA_tr, group = factor(Patient.ID),  col = factor(timepoint_used)), 
               alpha = 0.6, size = 2.1) +
    geom_line(aes(x = time_ichor.x, y = ichorCNA_tr, group = factor(Patient.ID)), 
              linetype = "dotted", alpha = 0.6, size = 1.1) +
    labs(col = "ctDNA utilised") +
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "ctDNA")) + 
    theme_linedraw(12) +
    theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(color = "black")) 

