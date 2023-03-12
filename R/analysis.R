# Analisis of results simulations
library(ggplot2)
library(dplyr)
library(tidyr)


sim_b <- read.csv("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_b.csv")
colnames(sim_b)
res <- sim_b[, 1:93]
colnames(res)

# estimates
k=30
cc <- sim_b[1:as.numeric(k+1)]

wcc <- sim_b[as.numeric(k+2):as.numeric(k+k+2)]

lcc <- sim_b[as.numeric(k+k+3):length(res)]

cc_long <- cc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "cc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

wcc_long <- wcc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "wcc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

lcc_long <- lcc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

# Combine the three data frames into one
estimates <- bind_rows(cc_long, wcc_long, lcc_long)

# Define the regressors to plot
regressors <- paste0("β_hat", 0:30)

# Create a list to store the plots
plots <- list()

# Loop through the regressors and create a plot for each one
for (r in regressors) {
  # Filter the data for the current regressor
  estimates_subset <- estimates %>%
    filter(regressor == r)
  
  # Check if the subset has at least one value for all the faceting variables
  if (all(levels(factor(estimates_subset$algorithm)) %in% unique(estimates_subset$algorithm))) {
    # Create the plot
    p <- ggplot(estimates_subset, aes(x = algorithm, y = value, fill = algorithm)) +
      geom_boxplot() +
      labs(x = "Algorithm", y = r, fill = "Algorithm") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Add the plot to the list
    plots[[r]] <- p
  }
}

# Combine the plots using grid.arrange
grid.arrange(grobs = plots, ncol = 3)


#### variance

# Compute variances for each estimate and algorithm
var_cc <- apply(cc, 2, var)
var_wcc <- apply(wcc, 2, var)
var_lcc <- apply(lcc, 2, var)


# Create a new data frame with the variances for each regressor and algorithm
variances <- data.frame(algorithm = rep(c("cc", "wcc", "lcc"), each = ncol(cc)),
                        regressor = rep(colnames(cc), 3),
                        variance = c(var_cc, var_wcc, var_lcc))

# extract the numeric part of the regressor names using regular expressions
variances <- variances %>%
  mutate(regressor_num = as.numeric(gsub(".*_(\\d+)$", "\\1", regressor))) %>%
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

# order the data frame by regressor and algorithm
variances <- variances[order(variances$regressor_num, variances$algorithm), ]

# remove the temporary column
variances <- variances %>% 
  select(-regressor_num)


# Loop through the regressors and create a plot for each one
for (r in regressors) {
  # Filter the data for the current regressor
  estimates_subset <- estimates %>%
    filter(regressor == r)
  
  # Filter the data for the current regressor and algorithm
  variances_subset <- variances %>%
    filter(regressor == r)
  
  # Check if the subsets have at least one value for all the faceting variables
  if (all(levels(factor(estimates_subset$algorithm)) %in% unique(estimates_subset$algorithm)) &&
      all(levels(factor(variances_subset$algorithm)) %in% unique(estimates_subset$algorithm))) {
    # Calculate the scale factor for the variances plot
    scale_factor <- max(estimates_subset$value) / max(variances_subset$variance)
    
    # Create the plot
    p <- ggplot() +
      # Add the boxplot for the estimates
      geom_boxplot(data = estimates_subset, aes(x = algorithm, y = value, fill = algorithm)) +
      # Add the barplot for the variances
      geom_bar(data = variances_subset, aes(x = algorithm, y = variance * scale_factor, fill = algorithm), stat = "identity", alpha = 0.5) +
      labs(x = "Algorithm", y = r, fill = "Algorithm") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Add the plot to the list
    plots[[r]] <- p
  }
}


ggplot(variances, aes(x = algorithm, y = variance)) +
  geom_boxplot() +
  xlab("Algorithm") +
  ylab("Variance") +
  ggtitle("Boxplot of Variances by Algorithm") +
  theme_minimal()


variances %>%
  group_by(algorithm) %>%
  summarize(avg_variance = mean(variance)) %>%
  ggplot(aes(x = algorithm, y = avg_variance)) +
  geom_bar(stat = "identity") +
  xlab("Algorithm") +
  ylab("Average Variance") +
  ggtitle("Average Variance by Algorithm") +
  theme_minimal()


ggplot(variances, aes(x = regressor, y = variance, color = algorithm)) +
  geom_point() +
  xlab("Regressor") +
  ylab("Variance") +
  ggtitle("Scatter plot of Variance by Regressor and Algorithm") + 
  theme_minimal()

summary_stats <- estimates %>%
  group_by(regressor, algorithm) %>%
  summarize(mean = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value))


