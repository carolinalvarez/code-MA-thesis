#' Author: Carolina Alvarez
#' Code for the simulation study 3
#' Varying degree of unconditional imbalance
library(MASS)
library(pROC)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

# Varying degree of imbalance (not conditional, just marginal imbalance)
# r = {0.7, 0.8, 0.9, 0.95, 0.99}

# the fact that p(Y=1) changes, also means that a(y) needs to be adjusted (this for ensuring that
# we get the same 50-50 split that Fithian and Hastie talk about)
# N needs to be fixed, so as k

# fixed parameters
sim=100
N = 10^5
k = 30
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

r_values <- c(0.7, 0.8, 0.9, 0.95, 0.99)

results <- list()

set.seed(123)

for (r in r_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(r)]] <- result
}

summary_df <- do.call(rbind, results)


write.csv(summary_df, file = paste0(path_output, "sim3_average_subsamples_LCC"), row.names = TRUE)

path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim3_average_subsamples_LCC"
subsamples <- read.csv(path)

# Start simulations

# Simulation 3.1
#  r= 0.7 and Average for LCC is 3500

set.seed(123)

sim <- 1000
k <- 30
N <- 10^5
r <- 0.7
a <- 0.7
ns_fixed1 <- 7000
ns_fixed2 <- 3500

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
beta_names_logit <- paste0("β_hat_logit_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc, beta_names_logit)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  logit_output <- glm(y~., data = df, family = binomial)
  coef_logit <- logit_output$coefficients
  
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  res_logit <- data.frame(t(coef_logit))
  colnames(res_logit) <- beta_names_logit
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, res_logit, a_bar_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+5):length(squared_bias)-1])
squared_bias_logit

mean_a_bar <- mean(res$a_bar_lcc)
mean_a_bar

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit


write.csv(res, file = paste0(path_output, "sim_P_a"), row.names = TRUE)



#################

# Sim 3.2
# r=0.8, average subsample LCC is 
set.seed(123)

sim <- 500
k <- 30
N <- 10^5
r <- 0.8
a <- 0.8
ns_fixed1 <- 6000
ns_fixed2 <- 3000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
beta_names_logit <- paste0("β_hat_logit_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc, beta_names_logit)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  logit_output <- glm(y~., data = df, family = binomial)
  coef_logit <- logit_output$coefficients
  
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  res_logit <- data.frame(t(coef_logit))
  colnames(res_logit) <- beta_names_logit
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, res_logit, a_bar_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+5):length(squared_bias)-1])
squared_bias_logit

mean_a_bar <- mean(res$a_bar_lcc)
mean_a_bar

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit


write.csv(res, file = paste0(path_output, "sim_Prob_b"), row.names = TRUE)


#################

# Sim 3.3
# r=0.9, average subsample LCC is 2000
set.seed(123)

sim <- 1000
k <- 30
N <- 10^5
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
beta_names_logit <- paste0("β_hat_logit_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc, beta_names_logit)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  logit_output <- glm(y~., data = df, family = binomial)
  coef_logit <- logit_output$coefficients
  
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  res_logit <- data.frame(t(coef_logit))
  colnames(res_logit) <- beta_names_logit
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, res_logit, a_bar_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+5):length(squared_bias)-1])
squared_bias_logit

mean_a_bar <- mean(res$a_bar_lcc)
mean_a_bar

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit


write.csv(res, file = paste0(path_output, "sim_Prob_c"), row.names = TRUE)


#################

# Sim 3.4
# r=0.5, average subsample LCC is 
set.seed(123)

sim <- 300
k <- 30
N <- 10^5
r <- 0.95
a <- 0.95
ns_fixed1 <- 2800
ns_fixed2 <- 1400

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
beta_names_logit <- paste0("β_hat_logit_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc, beta_names_logit)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  logit_output <- glm(y~., data = df, family = binomial)
  coef_logit <- logit_output$coefficients
  
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  res_logit <- data.frame(t(coef_logit))
  colnames(res_logit) <- beta_names_logit
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, res_logit, a_bar_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+5):length(squared_bias)-1])
squared_bias_logit

mean_a_bar <- mean(res$a_bar_lcc)
mean_a_bar

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit

write.csv(res, file = paste0(path_output, "sim_Prob_d"), row.names = TRUE)



#################

# Sim 3.5
# r=0.99, average subsample LCC is 
set.seed(123)

sim <- 1000
k <- 30
N <- 10^5
r <- 0.99
a <- 0.99
ns_fixed1 <- 1440
ns_fixed2 <- 720

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
beta_names_logit <- paste0("β_hat_logit_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc, beta_names_logit)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  logit_output <- glm(y~., data = df, family = binomial)
  coef_logit <- logit_output$coefficients
  
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  res_logit <- data.frame(t(coef_logit))
  colnames(res_logit) <- beta_names_logit
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc, res_logit, a_bar_lcc))
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+5):length(squared_bias)-1])
squared_bias_logit

mean_a_bar <- mean(res$a_bar_lcc)
mean_a_bar

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit


write.csv(res, file = paste0(path_output, "sim_Prob_e"), row.names = TRUE)
