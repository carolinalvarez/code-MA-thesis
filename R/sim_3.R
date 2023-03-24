#' Author: Carolina Alvarez
#' Code for the simulation study 3
library(MASS)
library(pROC)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

set.seed(123)

# Simulation I: testing for variance of LCC vs Logistic regression on full sample

set.seed(123)

sim <- 1000
k <- 26
N <- 100000
r <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_log <- paste0("β_hat_log_", 0:k)
output <- c(beta_names_log)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  logit_output <- glm(y~., data = df, family = binomial)
  coef_logit <- logit_output$coefficients
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Predicting subsampling
  # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
  # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
  
  # Coefficients
  res_logit <- data.frame(t(coef_logit))
  colnames(res_logit) <- beta_names_log
  
  #AUC
  
  # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
  # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
  # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
  
  res <- rbind(res
               , res_logit)
  
}

# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))


# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_logit <- sum(squared_bias)
squared_bias_logit

# Display variance

variances <- apply(res, 2, var)

var_logit <- sum(variances)
var_logit

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_i.csv", row.names = FALSE)


squared_bias_cc
squared_bias_wcc
squared_bias_lcc
squared_bias_logit

var_cc
var_wcc
var_lcc
var_logit



# Simulation J: running lcc with no restrictions on subsample size 

set.seed(123)

sim <- 1000
k <- 26
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_log <- paste0("β_hat_lcc_", 0:k)
output <- c(beta_names_log)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  lcc_output <- lcc_algorithm(df, a_wcc = a)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_log
  
  
  res <- rbind(res
               , cbind(res_lcc, a_bar_lcc))
  
}


# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))


# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_lcc <- sum(squared_bias[1:27])
squared_bias_lcc

# Display variance

variances <- apply(res, 2, var)

var_lcc <- sum(variances[1:27])
var_lcc #0.11
mean(res$a_bar_lcc) # 0.02866 / en el de hastie es menor 0.005, pero ellos se refieren al true pilot model

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_j.csv", row.names = FALSE)



# Simulation K: running lcc with no restrictions on subsample size and another version of lcc 

set.seed(123)

sim <- 1000
k <- 26
N <- 100000
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_log <- paste0("β_hat_lcc_", 0:k)
output <- c(beta_names_log)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  lcc_output <- lcc_algorithm_v2(df, a_wcc = a)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  
  # Coefficients
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc #antes estaba beta_names_log, ojo
  
  
  res <- rbind(res
               , cbind(res_lcc, a_bar_lcc))
  
}


# take the mean of the results
means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))


# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_lcc <- sum(squared_bias[1:27])
squared_bias_lcc

# Display variance

variances <- apply(res, 2, var)

var_lcc <- sum(variances[1:27])
var_lcc #0.047
mean(res$a_bar_lcc) # 0.02866 / en el de hastie es menor 0.005, pero ellos se refieren al true pilot model

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_k.csv", row.names = FALSE)


# Simulation L: Increasing N for asymptotic variance results, both logistic reg and lcc

set.seed(123)

sim <- 300
k <- 26
N <- 10^6
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_log <- paste0("β_hat_log_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
output <- c(beta_names_log)

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

time_measure_L <- system.time(
  
  for (i in 1:sim) {
    
    df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                         , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
    
    
    logit_output <- glm(y~., data = df, family = binomial)
    coef_logit <- logit_output$coefficients
    
    lcc_output <- lcc_algorithm_v2(df, a_wcc = a)
    coef_adjusted_lcc <- lcc_output$coef_adjusted
    
    # a_bar(lambda)
    a_bar_lcc <- mean(lcc_output$a_bar_lcc)
    
    # Coefficients
    res_logit <- data.frame(t(coef_logit))
    colnames(res_logit) <- beta_names_log
    
    res_lcc <- data.frame(t(coef_adjusted_lcc))
    colnames(res_lcc) <- beta_names_lcc
    
    
    res <- rbind(res
                 , cbind(res_logit, res_lcc, a_bar_lcc))
    
  }
  
  
)

time_measure_L

writeLines(capture.output(time_measure_L), paste0(path_output, "time_measure_L.txt"))


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 2)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_logit <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_logit
squared_bias_lcc <- sum(squared_bias[as.numeric(k+3):length(squared_bias)-1])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_logit <- sum(variances[1:as.numeric(k+1)])
var_logit #0.001423112
var_lcc <- sum(variances[as.numeric(k+3):length(squared_bias)-1])
var_lcc #0.00439175

# Again, is not 2 times as large as logit but 3 times as large as logit. Could it be because the pilot has variance?

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_l.csv", row.names = FALSE)
