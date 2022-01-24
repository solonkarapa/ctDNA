

library(pROC)
library(ggplot2)
library(tidyr)
library(plyr)
library(ggrepel)

############
# chose threshold
local_maximas <- coords(df_auc$fit2_CT_ichor, x = "local maximas")
chosen_threshold <- local_maximas[12,]
chosen_threshold

############ confusion matrix 
stats <- c("tp", "fp", "tn", "fn")
confusion_ichor <- coords(df_auc$fit2_CT_ichor, x = chosen_threshold$threshold, input = "threshold", ret = stats)
confusion_ichor$model <- "with ctDNA"
confusion_no_ichor <- coords(df_auc$fit2_CT_no_ichor, x = chosen_threshold$threshold, input = "threshold", ret = stats)
confusion_no_ichor$model <- "without ctDNA"

confusion_res <- rbind(confusion_ichor, confusion_no_ichor)
colnames(confusion_res) <- c("True Positives", "False Positives", "True Negatives", "False Negatives", "model")

data_long <- gather(confusion_res, stats, res, "True Positives":"False Negatives", factor_key = T)
data_long <-  data_long %>%
    mutate(stats = factor(stats, levels = c("True Positives", "True Negatives", "False Negatives", "False Positives")))

library(plyr)
df_sorted <- plyr::arrange(data_long, model, stats) 
df_cumsum <- plyr::ddply(df_sorted, c("model"),
                         transform, label_ypos = 129 - cumsum(res) + 0.5 * res)

ggplot(data = df_cumsum, aes(x = reorder(model, desc(model)), y = res, fill = stats)) +
    geom_bar(stat = "identity", color = "black", width = 0.4) +
    labs(x = "Model", y = "Number of cases", fill = "") +
    geom_text(aes(y = label_ypos, label = res)) + 
    scale_fill_brewer(palette = "Paired") +
    theme_classic(12) + 
    theme(legend.text=element_text(size = 12))
