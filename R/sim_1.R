#' Author: Carolina Alvarez
#' Code for the simulation study 1
#' Replicating Fithian and Hastie sim 2 Results
library(MASS)
library(pROC)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"


# Sim following the simulation by Fithian and Hastie (well specified model)
# Purpose: replicate direction of results

# subsample sizes
sim=100
k = 30
r = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

N_values <- c(10^5, 10^6)

results <- list()

set.seed(123)

for (N in N_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(N)]] <- result
}

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim1_average_subsamples_LCC"), row.names = TRUE)



# In the next simulations, I cannot follow the implicit subsampling approach of
# the authors because the prob function a(x,y) is not always the same for each
# random draw, so the proportions of y=1 and y=0 are not fixed for all subsamples.

# In this sense, i cannot generate data with the N implicit by generating the 
# data according to the expected proportions in the subsample for LCC.
# Therefore, i do generate the population size N.

# Simulation E (final simulation)

set.seed(123)

sim <- 1000
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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                       , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  
  cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
  coef_adjusted_cc <- cc_output$coef_adjusted
  
  wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
  coef_adjusted_lcc <- lcc_output$coef_adjusted
  
  # a_bar(lambda)
  #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
  
  # Coefficients
  res_cc <- data.frame(t(coef_adjusted_cc))
  colnames(res_cc) <- beta_names_cc
  
  res_wcc <- data.frame(t(coef_unadjusted_wcc))
  colnames(res_wcc) <- beta_names_wcc
  
  res_lcc <- data.frame(t(coef_adjusted_lcc))
  colnames(res_lcc) <- beta_names_lcc
  
  res <- rbind(res
               , cbind(res_cc, res_wcc, res_lcc))
  
}

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)

squared_bias <- (means - beta_true)^2

colnames(squared_bias) <- colnames(means)

squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_e.csv", row.names = FALSE)


