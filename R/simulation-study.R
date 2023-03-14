#' Author: Carolina Alvarez
#' Code for the simulation study


library(MASS)
library(pROC)

options(scipen = 999)

set.seed(123)


# 1. Following the simulation by Fithian and Hastie (well specified model)


sim <- 200
k <- 10
N <- 300
r <- 0.9 # P(Y=0)
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
# [1] 0.002401736
# > squared_bias_cc <- sum(squared_bias[as.numeric(k+2):length(squared_bias)-2])
# > squared_bias_cc
# [1] 0.01669469
# > 
#   > auc_logit_mean <- mean(res$auc_df)
# > auc_logit_mean
# [1] 0.9968637
# > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9967912




sim <- 300
k <- 30
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
#mean1 <- c(rep(0.4, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

#beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names_adj, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  wcc_output <- wcc_algorithm(df_train, a = a)
  df_subsample <- wcc_output$subsample_S
  
  coef_unadjusted <- wcc_output$coef_unadjusted
  
  # Running model for sample and predicting
  #model_full <- glm("y~.", data = df_train, family = binomial)
  #y_hat_df <- predict(model_full, newdata = df_test, type = "response")
  
  # Predicting subsampling
  y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), wcc_output$coef_unadjusted)
  
  # Coefficients
  # res_df <- data.frame(t(model_full$coefficients))
  # colnames(res_df) <- beta_names
  
  res_wcc <- data.frame(t(wcc_output$coef_unadjusted))
  colnames(res_wcc) <- beta_names_adj
  
  
  #AUC
  #auc_df <- as.numeric(roc(df_test$y, y_hat_df)$auc)
  auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  
  res <- rbind(res
               , cbind(res_wcc, auc_wcc))
  
}


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))
print(means)

# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_wcc <- sum(squared_bias[as.numeric(1):length(squared_bias)-1])
squared_bias_wcc

auc_wcc_mean <- mean(res$auc_wcc)
auc_wcc_mean



# > squared_bias_wcc
# [1] 0.08291384
# > 
#   > auc_wcc_mean <- mean(res$auc_wcc)
# > auc_wcc_mean
# [1] 0.9967379



sim <- 100
k <- 30
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
#mean1 <- c(rep(0.4, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

#beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names_adj, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  wcc_output <- wcc_algorithm_2(df_train, a = a)
  df_subsample <- wcc_output$subsample_S
  
  coef_adjusted <- wcc_output$coef_adjusted
  
  # Running model for sample and predicting
  #model_full <- glm("y~.", data = df_train, family = binomial)
  #y_hat_df <- predict(model_full, newdata = df_test, type = "response")
  
  # Predicting subsampling
  y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), wcc_output$coef_adjusted)
  
  # Coefficients
  # res_df <- data.frame(t(model_full$coefficients))
  # colnames(res_df) <- beta_names
  
  res_wcc <- data.frame(t(wcc_output$coef_adjusted))
  colnames(res_wcc) <- beta_names_adj
  
  
  #AUC
  #auc_df <- as.numeric(roc(df_test$y, y_hat_df)$auc)
  auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  
  res <- rbind(res
               , cbind(res_wcc, auc_wcc))
  
}


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))
print(means)

# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_wcc <- sum(squared_bias[as.numeric(1):length(squared_bias)-1])
squared_bias_wcc



### Simulated with the implicit sampling approach, comparing just CC and WCC
# As Simulation 2 in the paper
# Taking Ns=30,000 in order to have a Ns_train = 21,000 and Ns_test = 9,000

sim <- 100
k <- 40
Ns <- 30000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

df_test <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df_Ns <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                          , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                          , sigma0 = cov_mat)
  
  strat <- strat_sampling(df_Ns, 0.70)
  df_Ns_train <- strat$df_train
  df_Ns_test <- strat$df_test
  
  cc_output <- cc_algorithm_Ns(df_Ns_train, a = a)
  df_subsample_cc <- cc_output$subsample_S
  coef_adjusted_cc <- cc_output$coef_adjusted

  wcc_output <- wcc_algorithm_Ns(df_Ns_train, a = a)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_Ns_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  y_hat_wcc <- logit_predict(df_Ns_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  
  #AUC
  auc_cc <- as.numeric(roc(df_Ns_test$y, y_hat_cc)$auc)
  auc_wcc <- as.numeric(roc(df_Ns_test$y, y_hat_wcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, auc_cc, auc_wcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


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
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
squared_bias_wcc

# Take the variance of the realizations
variances <- apply(res, 2, var)
var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+4):length(variances)-2])
var_wcc

auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean
auc_wcc_mean <- mean(res$auc_wcc)
auc_wcc_mean


# > squared_bias_cc
# [1] 0.08300709
# > squared_bias_wcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
# > squared_bias_wcc
# [1] 0.2884721
# > 
#   > # Take the variance of the realizations
#   > variances <- apply(res, 2, var)
# > var_cc <- sum(variances[1:as.numeric(k+1)])
# > var_cc
# [1] 0.3431406
# > var_wcc <- sum(variances[as.numeric(k+4):length(variances)-2])
# > var_wcc
# [1] 0.6308715
# > 
#   > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.99918
# > auc_wcc_mean <- mean(res$auc_wcc)
# > auc_wcc_mean
# [1] 0.9991544




sim <- 500
k <- 40
Ns <- 30000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

df_test <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df_Ns <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)
  
  strat <- strat_sampling(df_Ns, 0.70)
  df_Ns_train <- strat$df_train
  df_Ns_test <- strat$df_test
  
  cc_output <- cc_algorithm_Ns(df_Ns_train, a = a)
  df_subsample_cc <- cc_output$subsample_S
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_Ns(df_Ns_train, a = a)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_Ns_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  y_hat_wcc <- logit_predict(df_Ns_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  
  #AUC
  auc_cc <- as.numeric(roc(df_Ns_test$y, y_hat_cc)$auc)
  auc_wcc <- as.numeric(roc(df_Ns_test$y, y_hat_wcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, auc_cc, auc_wcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


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
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
squared_bias_wcc

# Take the variance of the realizations
variances <- apply(res, 2, var)
var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+4):length(variances)-2])
var_wcc

auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean
auc_wcc_mean <- mean(res$auc_wcc)
auc_wcc_mean


# > squared_bias_cc
# [1] 0.1030429
# > squared_bias_wcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
# > squared_bias_wcc
# [1] 0.3429523
# > 
#   > # Take the variance of the realizations
#   > variances <- apply(res, 2, var)
# > var_cc <- sum(variances[1:as.numeric(k+1)])
# > var_cc
# [1] 0.3026316
# > var_wcc <- sum(variances[as.numeric(k+4):length(variances)-2])
# > var_wcc
# [1] 0.5547373
# > 
#   > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9991688
# > auc_wcc_mean <- mean(res$auc_wcc)
# > auc_wcc_mean
# [1] 0.9991424


sim <- 500
k <- 36
Ns <- 30000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

df_test <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df_Ns <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)
  
  strat <- strat_sampling(df_Ns, 0.70)
  df_Ns_train <- strat$df_train
  df_Ns_test <- strat$df_test
  
  cc_output <- cc_algorithm_Ns(df_Ns_train, a = a)
  df_subsample_cc <- cc_output$subsample_S
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_Ns(df_Ns_train, a = a)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_Ns_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  y_hat_wcc <- logit_predict(df_Ns_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  
  #AUC
  auc_cc <- as.numeric(roc(df_Ns_test$y, y_hat_cc)$auc)
  auc_wcc <- as.numeric(roc(df_Ns_test$y, y_hat_wcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, auc_cc, auc_wcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


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
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
squared_bias_wcc

# Take the variance of the realizations
variances <- apply(res, 2, var)
var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+4):length(variances)-2])
var_wcc

auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean
auc_wcc_mean <- mean(res$auc_wcc)
auc_wcc_mean


# > squared_bias_cc
# [1] 0.04279658
# > squared_bias_wcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
# > squared_bias_wcc
# [1] 0.1250105
# > 
#   > # Take the variance of the realizations
#   > variances <- apply(res, 2, var)
# > var_cc <- sum(variances[1:as.numeric(k+1)])
# > var_cc
# [1] 0.1835521
# > var_wcc <- sum(variances[as.numeric(k+4):length(variances)-2])
# > var_wcc
# [1] 0.3292369
# > 
#   > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9985831
# > auc_wcc_mean <- mean(res$auc_wcc)
# > auc_wcc_mean
# [1] 0.998554


###----------------------------------------------------------------------------------------------------

# Simulation study to compare the three Hastie algorithms, not doing implicit sampling

# Simulation A
set.seed(123)

sim <- 100
k <- 30
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  cc_output <- cc_algorithm(df_train, a = a)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm(df_train, a = a)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm(df_train, a_wcc = a)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  a_bar_lambda <- mean(lcc_output$subsample_S$a)

  # Predicting subsampling
  y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, 
                       auc_cc, auc_wcc, auc_lcc,
                       a_bar_lambda))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
, rep(0, k/2))

beta_true <- rep(beta_true, 3)
  
# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+7):length(squared_bias)-4])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+7):length(squared_bias)-4])
var_lcc

# mean a_bar_lambda
a_bar_lambda_mean <- mean(res$a_bar_lambda)
a_bar_lambda_mean

#AUCs
auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean
auc_wcc_mean <- mean(res$auc_wcc)
auc_wcc_mean
auc_lcc_mean <- mean(res$auc_lcc)
auc_lcc_mean

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_a.csv", row.names = FALSE)

# > squared_bias_cc
# [1] 0.02771759
# > squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
# > squared_bias_wcc
# [1] 0.07324386
# > squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+7):length(squared_bias)-4])
# > squared_bias_lcc
# [1] 0.002930574
# > 
#   > # Take the variance of the realizations
#   > variances <- apply(res, 2, var)
# > 
#   > var_cc <- sum(variances[1:as.numeric(k+1)])
# > var_cc
# [1] 0.1681018
# > var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
# > var_wcc
# [1] 0.3459877
# > var_lcc <- sum(variances[as.numeric(k+k+7):length(squared_bias)-4])
# > var_lcc
# [1] 0.3119675
# > 
#   > # mean a_bar_lambda
#   > a_bar_lambda_mean <- mean(res$a_bar_lambda)
# > a_bar_lambda_mean
# [1] 0.5234505
# > 
#   > #AUCs
#   > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9967873
# > auc_wcc_mean <- mean(res$auc_wcc)
# > auc_wcc_mean
# [1] 0.9967381
# > auc_lcc_mean <- mean(res$auc_lcc)
# > auc_lcc_mean
# [1] 0.9967249


# Simulation B
set.seed(123)

sim <- 300
k <- 30
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  strat <- strat_sampling(df, 0.70)
  df_train <- strat$df_train
  df_test <- strat$df_test
  
  cc_output <- cc_algorithm(df_train, a = a)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm(df_train, a = a)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm(df_train, a_wcc = a)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, 
                       auc_cc, auc_wcc, auc_lcc,
                       a_bar_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+7):length(squared_bias)-4])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+7):length(squared_bias)-4])
var_lcc

# mean a_bar_lambda
a_bar_lcc_mean <- mean(res$a_bar_lcc)
a_bar_lcc_mean

#AUCs
auc_cc_mean <- mean(res$auc_cc)
auc_cc_mean
auc_wcc_mean <- mean(res$auc_wcc)
auc_wcc_mean
auc_lcc_mean <- mean(res$auc_lcc)
auc_lcc_mean

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_b.csv", row.names = FALSE)


# > squared_bias_cc
# [1] 0.02081066
# > squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
# > squared_bias_wcc
# [1] 0.08076986
# > squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+7):length(squared_bias)-4])
# > squared_bias_lcc
# [1] 0.001614152
# > 
#   > # Take the variance of the realizations
#   > variances <- apply(res, 2, var)
# > 
#   > var_cc <- sum(variances[1:as.numeric(k+1)])
# > var_cc
# [1] 0.1643952
# > var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
# > var_wcc
# [1] 0.3078258
# > var_lcc <- sum(variances[as.numeric(k+k+7):length(squared_bias)-4])
# > var_lcc
# [1] 0.3449388
# > 
#   > # mean a_bar_lambda
#   > a_bar_lcc_mean <- mean(res$a_bar_lcc)
# > a_bar_lcc_mean
# [1] 0.02161723
# > 
#   > #AUCs
#   > auc_cc_mean <- mean(res$auc_cc)
# > auc_cc_mean
# [1] 0.9967874
# > auc_wcc_mean <- mean(res$auc_wcc)
# > auc_wcc_mean
# [1] 0.9967286
# > auc_lcc_mean <- mean(res$auc_lcc)
# > auc_lcc_mean
# [1] 0.9967131




# Simulation C
set.seed(123)

sim <- 300
k <- 20
N <- 100000
r <- 0.9
a <- 0.9
ns_fixed1 <- 4000
ns_fixed2 <- 2000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  

  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

  # Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_c.csv", row.names = FALSE)


# Simulation D

set.seed(123)

sim <- 300
k <- 30
N <- 100000
r <- 0.9
a <- 0.9
ns_fixed1 <- 4000
ns_fixed2 <- 2000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_d.csv", row.names = FALSE)

# Simulation E

set.seed(123)

sim <- 500
k <- 30
N <- 100000
r <- 0.9
a <- 0.9
ns_fixed1 <- 4000
ns_fixed2 <- 2000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_e.csv", row.names = FALSE)


# Simulation F

set.seed(123)

sim <- 300
k <- 36
N <- 100000
r <- 0.9
a <- 0.9
ns_fixed1 <- 4000
ns_fixed2 <- 2000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_f.csv", row.names = FALSE)



# Simulation G

set.seed(123)

sim <- 300
k <- 26
N <- 100000
r <- 0.9
a <- 0.9
ns_fixed1 <- 4000
ns_fixed2 <- 2000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_g.csv", row.names = FALSE)



# Simulation H, 25 min

set.seed(123)

sim <- 1000
k <- 26
N <- 100000
r <- 0.9
a <- 0.9
ns_fixed1 <- 4000
ns_fixed2 <- 2000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)


output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- gdp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_h.csv", row.names = FALSE)






