


# helper function to create summaries of posterior
summary_fun <- function(x, quantiles, ...) {
    
    c(quantile(x, probs = quantiles), mean = mean(x))
    
}

