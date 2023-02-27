#' Author: Carolina Alvarez
#' Code for the simulation study
#' 
library(pROC)
options(scipen = 999)

# function for the simulations

# 1. Following the simulation by Fithian and Hastie (well specified model)

k <- 5
sim <- 500
n <- 100000
r <- 0.95 #proportion of 1s
c <- 0.7
#mean1<- c(rep(1, k/2), rep(0, k/2))

beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names, beta_names_adj)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = n, r = r, distribution= "gaussian", k=k, mean1=c(1, 1, 1, 1, 0.8), mean0=0, sd1=1, sd0=1)
  
  cc_output <- cc_algorithm(df, c = c, split_r = 0.70)
  df_subsample <- cc_output$subsample_S
  df_subsample_test <- cc_output$subsample_test
  
  coef_unadjusted <- cc_output$coef_unadjusted
  coef_adjusted <- cc_output$coef_adjusted
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  model_full <- glm("y~.", data = df_train, family = binomial)
  y_hat_df <- predict(model_full, newdata = df_test, type = "response")
  
  res_df <- data.frame(t(model_full$coefficients))
  colnames(res_df) <- beta_names
  
  cc_output_df <- data.frame(t(cc_output$coef_adjusted))
  colnames(cc_output_df) <- beta_names_adj
  
  res <- rbind(res
               , cbind(res_df
                       , cc_output_df))
  
}

get.true.intercept(1-r, rep(0.5, k), rep(1, k))


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))
print(means)

# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), rep(1, k))
               , rep(1, k)
               , get.true.intercept(1-r, rep(0.5, k), rep(1, k))
               , rep(1, k))

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias






