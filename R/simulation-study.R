#' Author: Carolina Alvarez
#' Code for the simulation study
#' 
#'
library(MASS)
library(pROC)
options(scipen = 999)

# function for the simulations

# 1. Following the simulation by Fithian and Hastie (well specified model)
#

sim <- 200
k <- 10
N <- 300
r <- 0.9
a <- 0.6
mean1 <- c(rep(1, k/2), rep(0, k/2))
#mean1 <- c(rep(0.4, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names, beta_names_adj, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  cc_output <- cc_algorithm(df_train, a = a)
  df_subsample <- cc_output$subsample_S
  #df_subsample_test <- cc_output$subsample_test
  
  coef_unadjusted <- cc_output$coef_unadjusted
  coef_adjusted <- cc_output$coef_adjusted
  
  # Running model for sample and predicting
  model_full <- glm("y~.", data = df_train, family = binomial)
  y_hat_df <- predict(model_full, newdata = df_test, type = "response")
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), cc_output$coef_adjusted)
  
  # Coefficients
  res_df <- data.frame(t(model_full$coefficients))
  colnames(res_df) <- beta_names
  
  res_cc <- data.frame(t(cc_output$coef_adjusted))
  colnames(res_cc) <- beta_names_adj
  
  
  #AUC
  auc_df <- as.numeric(roc(df_test$y, y_hat_df)$auc)
  auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  
  res <- rbind(res
               , cbind(res_df, res_cc, auc_df, auc_cc))
  
}

get.true.intercept(1-r, rep(0.5, k), rep(1, k))


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))
print(means)

# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2)
               , get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias
squared_bias_logit <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_logit
squared_bias_cc <- sum(squared_bias[as.numeric(k+2):length(squared_bias)-2])
squared_bias_cc

auc_logit_mean <- mean(res$auc_df)
auc_logit_mean
auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean

# For when sim=1 :
# checking whether proportions match with theory
# nrow(df_subsample)/nrow(df)
# a_bar(a=a, r=r)
# 
# table(df_subsample$y)/nrow(df_subsample)
# prop_Ps(a=a, r=r)
# 
# 
# table(df_subsample_test$y)/nrow(df_subsample_test)
# prop_Ps(a=a, r=r)



sim <- 100
k <- 20
N <- 100000
r <- 0.99
a <- 0.7

mean1 <- c(rep(1, k/2), rep(0, k/2))
#mean1 <- c(rep(0.4, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names, beta_names_adj, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  cc_output <- cc_algorithm(df_train, a = a)
  df_subsample <- cc_output$subsample_S
  #df_subsample_test <- cc_output$subsample_test
  
  coef_unadjusted <- cc_output$coef_unadjusted
  coef_adjusted <- cc_output$coef_adjusted
  
  # Running model for sample and predicting
  model_full <- glm("y~.", data = df_train, family = binomial)
  y_hat_df <- predict(model_full, newdata = df_test, type = "response")
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), cc_output$coef_adjusted)
  
  # Coefficients
  res_df <- data.frame(t(model_full$coefficients))
  colnames(res_df) <- beta_names
  
  res_cc <- data.frame(t(cc_output$coef_adjusted))
  colnames(res_cc) <- beta_names_adj
  
  
  #AUC
  auc_df <- as.numeric(roc(df_test$y, y_hat_df)$auc)
  auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  
  res <- rbind(res
               , cbind(res_df, res_cc, auc_df, auc_cc))
  
}

get.true.intercept(1-r, rep(0.5, k), rep(1, k))


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))
print(means)

# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2)
               , get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias
squared_bias_logit <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_logit
squared_bias_cc <- sum(squared_bias[as.numeric(k+2):length(squared_bias)-2])
squared_bias_cc

auc_logit_mean <- mean(res$auc_df)
auc_logit_mean
auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean

# Results:
# > squared_bias_logit
# [1] 0.001332681
# > squared_bias_cc <- sum(squared_bias[as.numeric(k+2):length(squared_bias)-2])
# > squared_bias_cc
# [1] 0.005377831
# > 
#   > auc_logit_mean <- mean(res$auc_df)
# > auc_logit_mean
# [1] 0.9873698
# > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9871875




sim <- 300
k <- 30
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
#mean1 <- c(rep(0.4, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names, beta_names_adj, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  cc_output <- cc_algorithm(df_train, a = a)
  df_subsample <- cc_output$subsample_S
  #df_subsample_test <- cc_output$subsample_test
  
  coef_unadjusted <- cc_output$coef_unadjusted
  coef_adjusted <- cc_output$coef_adjusted
  
  # Running model for sample and predicting
  model_full <- glm("y~.", data = df_train, family = binomial)
  y_hat_df <- predict(model_full, newdata = df_test, type = "response")
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), cc_output$coef_adjusted)
  
  # Coefficients
  res_df <- data.frame(t(model_full$coefficients))
  colnames(res_df) <- beta_names
  
  res_cc <- data.frame(t(cc_output$coef_adjusted))
  colnames(res_cc) <- beta_names_adj
  
  
  #AUC
  auc_df <- as.numeric(roc(df_test$y, y_hat_df)$auc)
  auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  
  res <- rbind(res
               , cbind(res_df, res_cc, auc_df, auc_cc))
  
}

get.true.intercept(1-r, rep(1, k), rep(1, k))


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))
print(means)

# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2)
               , get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias
squared_bias_logit <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_logit
squared_bias_cc <- sum(squared_bias[as.numeric(k+2):length(squared_bias)-2])
squared_bias_cc

auc_logit_mean <- mean(res$auc_df)
auc_logit_mean
auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean


# > squared_bias_logit
# [1] 0.002102163
# > squared_bias_cc <- sum(squared_bias[as.numeric(k+2):length(squared_bias)-2])
# > squared_bias_cc
# [1] 0.02476623
# > 
#   > auc_logit_mean <- mean(res$auc_df)
# > auc_logit_mean
# [1] 0.9968629
# > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9968014
