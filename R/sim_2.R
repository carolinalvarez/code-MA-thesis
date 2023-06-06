#' Author: Carolina Alvarez
#' Code for the simulation study 2
library(MASS)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

# Simulation M: Increasing N for asymptotic variance results, both logistic reg and lcc, increasing number of simulations and 
#getting subsample size of LCC, also running with k=30

set.seed(123)

sim <- 1000
k <- 30
N <- 10^5
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_log <- paste0("β_hat_log_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
output <- c(beta_names_log, beta_names_lcc)

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
    
    lcc_subsample_size <- nrow(lcc_output$subsample_lcc)
    
    # Coefficients
    res_logit <- data.frame(t(coef_logit))
    colnames(res_logit) <- beta_names_log
    
    res_lcc <- data.frame(t(coef_adjusted_lcc))
    colnames(res_lcc) <- beta_names_lcc
    
    
    res <- rbind(res
                 , cbind(res_logit, res_lcc, a_bar_lcc, lcc_subsample_size))
    
  }
  
  
)

time_measure_L

writeLines(capture.output(time_measure_L), paste0(path_output, "time_measure_m.txt"))


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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_logit <- sum(variances[1:as.numeric(k+1)])
var_logit 
var_lcc <- sum(variances[as.numeric(k+3):length(squared_bias)-1])
var_lcc 

a_bar <- mean(res$a_bar_lcc)
a_bar


subsample <- mean(res$lcc_subsample_size)
subsample

write.csv(res, file = paste0(path_output, "sim_m.csv"), row.names = FALSE)



# Simulation N: Increasing N for asymptotic variance results, both logistic reg and lcc, increasing number of simulations and 
#getting subsample size of LCC, also running with k=30 and N=million

set.seed(123)

sim <- 1000
k <- 30
N <- 10^6
r <- 0.9
a <- 0.9

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names_log <- paste0("β_hat_log_", 0:k)
beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
output <- c(beta_names_log, beta_names_lcc, "a_bar_lcc", "lcc_subsample_size")

#res <- data.frame(matrix(ncol = length(output), nrow = 0))
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
    
    lcc_subsample_size <- nrow(lcc_output$subsample_lcc)
    
    # Coefficients
    res_logit <- data.frame(t(coef_logit))
    colnames(res_logit) <- beta_names_log
    
    res_lcc <- data.frame(t(coef_adjusted_lcc))
    colnames(res_lcc) <- beta_names_lcc
    
    
    res <- rbind(res
                 , cbind(res_logit, res_lcc, a_bar_lcc, lcc_subsample_size))
    
  }
  
  
)

time_measure_L

writeLines(capture.output(time_measure_L), paste0(path_output, "time_measure_n.txt"))


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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+4):length(squared_bias)-2])
squared_bias_lcc

# squared bias only intercept
squared_bias[as.numeric(k+3):length(squared_bias)-2]

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_logit <- sum(variances[1:as.numeric(k+1)])
var_logit
var_lcc <- sum(variances[as.numeric(k+4):length(squared_bias)-2])
var_lcc 

#variance only intercept
variances[1:as.numeric(k+1)]

a_bar <- mean(res$a_bar_lcc)
a_bar

subsample <- mean(res$lcc_subsample_size)
subsample

#getting mean of just the intercept for lcc
mean(res$ß_hat_lcc_0)
mean(res$ß_hat_log_0)

variances[1] # variance of intercept

write.csv(res, file = paste0(path_output, "sim_n.csv"), row.names = FALSE)
