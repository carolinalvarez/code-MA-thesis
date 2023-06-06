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

################################################################################
###########################   Sim N=10^5  ######################################
################################################################################

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


set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_a")

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
var_logit <- sum(variances[as.numeric(k+k+k+5):length(variances)-1])
var_logit


#write.csv(res, file = paste0(path_output, "sim_Prob_a"), row.names = TRUE)



#################
# Sim 3.2
# r=0.8, average subsample LCC is 
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

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_b")

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


#write.csv(res, file = paste0(path_output, "sim_Prob_b"), row.names = TRUE)


#################

# Sim 3.3
# r=0.9, average subsample LCC is 2000
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

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_c")
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


#write.csv(res, file = paste0(path_output, "sim_Prob_c"), row.names = TRUE)


#################

# Sim 3.4
# r=0.5, average subsample LCC is RERUN
sim <- 1000
k <- 30
N <- 10^5
r <- 0.95
a <- 0.95
ns_fixed1 <- 2800
ns_fixed2 <- 1400

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_b, a=a_b, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_d")

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

#write.csv(res, file = paste0(path_output, "sim_Prob_d"), row.names = TRUE)



#################

# Sim 3.5
# r=0.99, average subsample LCC 
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

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_b, a=a_b, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_e")

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

# Analyzing the intercept
lcc_res <- res[as.numeric(k+k+3):as.numeric(k+k+k+3)]
summary(lcc_res)

library(DescTools)
df_lcc_wins <- as.data.frame(lapply(lcc_res, function(x) Winsorize(x, probs = c(0.1, 0.9))))

summary(df_lcc_wins)

means_lcc <- data.frame(t(colMeans(df_lcc_wins)))
colnames(means_lcc) <- gsub("β_hat_", "", colnames(means_lcc))

squared_bias_lcc_wins <- (means_lcc - beta_true)^2
squared_bias_lcc_wins <- sum(squared_bias_lcc_wins)
squared_bias_lcc_wins

variances_lcc <- apply(df_lcc_wins, 2, var)
var_lcc_wins <- sum(variances_lcc)
var_lcc_wins

#write.csv(res_list, file = paste0(path_output, "sim_Prob_e"), row.names = TRUE)

####################################################################################
#################### Repeat simulations but with a smaller k #######################
#################################################################################### 

path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"
# I need to recalculate the subsample sizes...

# fixed parameters
sim=100
N = 10^5
k = 10
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

df_subsamples <- do.call(rbind, results)
write.csv(df_subsamples, file = paste0(path_output, "sim3_average_subsamples_LCC_2"),
          row.names = TRUE)


# General parameters
k = 10
sim <- 1000
N <- 10^5
#####################

# sim Prob a
r_a <- 0.7
a_a <- 0.7
ns_fixed1_a <- 33400
ns_fixed2_a <- 16700

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_a, a=a_a, ns_fixed1 = ns_fixed1_a,
                                  ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                  name_res = "sim_Prob_a_2")
#res_analysis_a <- res_analysis_sim3(res = res_a, k=k, a = a_a)

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_a, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
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
var_logit <- sum(variances[as.numeric(k+k+k+7):length(squared_bias)-3])
var_logit


# sim prob b
r_b <- 0.8
a_b <- 0.8
ns_fixed1_b <- 27600
ns_fixed2_b <- 13800

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_b, a=a_b, ns_fixed1 = ns_fixed1_b,
                                   ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                   name_res = "sim_Prob_b_2")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_b, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob c
r_c <- 0.9
a_c <- 0.9
ns_fixed1_c <- 18000
ns_fixed2_c <- 9000

set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_c, a=a_c, ns_fixed1 = ns_fixed1_c,
                                   ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                   name_res = "sim_Prob_c_2")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_c, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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



# sim prob d
r_d <- 0.95
a_d <- 0.95
ns_fixed1_d <- 11200
ns_fixed2_d <- 5600

set.seed(123)
res_d <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_d, a=a_d, ns_fixed1 = ns_fixed1_d,
                                   ns_fixed2 = ns_fixed2_d, path_output = path_output, 
                                   name_res = "sim_Prob_d_2")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_d, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob e
r_e <- 0.99
a_e <- 0.99
ns_fixed1_e <- 3100
ns_fixed2_e <- 1550

set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_e, a=a_e, ns_fixed1 = ns_fixed1_e,
                                   ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                   name_res = "sim_Prob_e_2")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_e, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

################################################################################

# fixed parameters
sim=100
N = 10^5
k = 20
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

df_subsamples <- do.call(rbind, results)
write.csv(df_subsamples, file = paste0(path_output, "sim3_average_subsamples_LCC_3"),
          row.names = TRUE)




# General parameters
k = 20
sim <- 1000
N <- 10^5

#####################

# sim Prob a
r_a <- 0.7
a_a <- 0.7
ns_fixed1_a <- 15000
ns_fixed2_a <- 7500

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_a, a=a_a, ns_fixed1 = ns_fixed1_a,
                                   ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                   name_res = "sim_Prob_a_3")
#res_analysis_a <- res_analysis_sim3(res = res_a, k=k, a = a_a)

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_a, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob b
r_b <- 0.8
a_b <- 0.8
ns_fixed1_b <- 12600
ns_fixed2_b <- 6300

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_b, a=a_b, ns_fixed1 = ns_fixed1_b,
                                   ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                   name_res = "sim_Prob_b_3")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_b, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob c
r_c <- 0.9
a_c <- 0.9
ns_fixed1_c <- 8800
ns_fixed2_c <- 4400

set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_c, a=a_c, ns_fixed1 = ns_fixed1_c,
                                   ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                   name_res = "sim_Prob_c_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_c, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)
beta_true

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

# sim prob d
r_d <- 0.95
a_d <- 0.95
ns_fixed1_d <- 5600
ns_fixed2_d <- 2800

set.seed(123)
res_d <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_d, a=a_d, ns_fixed1 = ns_fixed1_d,
                                   ns_fixed2 = ns_fixed2_d, path_output = path_output, 
                                   name_res = "sim_Prob_d_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_d, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)
beta_true

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

# sim prob e
r_e <- 0.99
a_e <- 0.99
ns_fixed1_e <- 2000
ns_fixed2_e <- 1000

set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_e, a=a_e, ns_fixed1 = ns_fixed1_e,
                                   ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                   name_res = "sim_Prob_e_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_e, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)
beta_true

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

###############################################################################
###########################   Sim N=10^6   ####################################
###############################################################################

# fixed parameters
sim=100
N = 10^6
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

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim3_average_subsamples_LCC_4"), row.names = TRUE)

path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim3_average_subsamples_LCC"
subsamples <- read.csv(path)

# Start simulations

# Simulation 3.1
#  r= 0.7 and Average for LCC is 
sim <- 1000
k <- 30
N <- 10^6
r <- 0.7
a <- 0.7
ns_fixed1 <- 71600
ns_fixed2 <- 35800

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_a_4_Mill")
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


#write.csv(res, file = paste0(path_output, "sim_Prob_a_4"), row.names = TRUE)



#################

# Sim 3.2
# r=0.8, average subsample LCC is 
set.seed(123)

sim <- 500
k <- 30
N <- 10^6
r <- 0.8
a <- 0.8
ns_fixed1 <- 61000
ns_fixed2 <- 30500

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_b_4_Mill")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))



beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)


squared_bias <- (means - beta_true)^2


colnames(squared_bias) <- colnames(means)


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

variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit


#write.csv(res, file = paste0(path_output, "sim_Prob_b_4"), row.names = TRUE)


#################

# Sim 3.3
# r=0.9, average subsample LCC is 2000
sim <- 1000
k <- 30
N <- 10^6
r <- 0.9
a <- 0.9
ns_fixed1 <- 42800
ns_fixed2 <- 21400

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_c_4_Mill")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

squared_bias <- (means - beta_true)^2

colnames(squared_bias) <- colnames(means)

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

variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(squared_bias)-1])
var_logit


#write.csv(res, file = paste0(path_output, "sim_Prob_c_4"), row.names = TRUE)


#################

# Sim 3.4
# r=0.5, average subsample LCC is 
sim <- 1000
k <- 30
N <- 10^6
r <- 0.95
a <- 0.95
ns_fixed1 <- 28600
ns_fixed2 <- 14300

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_d_4_Mill")

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

write.csv(res, file = paste0(path_output, "sim_Prob_d_4"), row.names = TRUE)



#################
# Sim 3.5
# r=0.99, average subsample LCC is 
sim <- 1000
k <- 30
N <- 10^6
r <- 0.99
a <- 0.99
ns_fixed1 <- 10000
ns_fixed2 <- 5000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_Prob_e_4_Mill")

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


#write.csv(res, file = paste0(path_output, "sim_Prob_e_4"), row.names = TRUE)


#################### Repeat simulations but with a smaller k #######################


path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"
# I need to recalculate the subsample sizes...

# fixed parameters
sim=100
N = 10^6
k = 10
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

df_subsamples <- do.call(rbind, results)
write.csv(df_subsamples, file = paste0(path_output, "sim3_average_subsamples_LCC_5"),
          row.names = TRUE)




# General parameters
k = 10
sim <- 1000
N <- 10^6
#####################

# sim Prob a
r_a <- 0.7
a_a <- 0.7
ns_fixed1_a <- 335400
ns_fixed2_a <- 167700

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_a, a=a_a, ns_fixed1 = ns_fixed1_a,
                                      ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                      name_res = "sim_Prob_a_5_Mill")
#res_analysis_a <- res_analysis_sim3(res = res_a, k=k, a = a_a)

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_a, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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


# sim prob b
r_b <- 0.8
a_b <- 0.8
ns_fixed1_b <- 277400
ns_fixed2_b <- 138700

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_b, a=a_b, ns_fixed1 = ns_fixed1_b,
                                      ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                      name_res = "sim_Prob_b_5_Mill")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_b, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob c
r_c <- 0.9
a_c <- 0.9
ns_fixed1_c <- 182800
ns_fixed2_c <- 91400

set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_c, a=a_c, ns_fixed1 = ns_fixed1_c,
                                      ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                      name_res = "sim_Prob_c_5_Mill")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_c, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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



# sim prob d
r_d <- 0.95
a_d <- 0.95
ns_fixed1_d <- 112400
ns_fixed2_d <- 56200

set.seed(123)
res_d <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_d, a=a_d, ns_fixed1 = ns_fixed1_d,
                                      ns_fixed2 = ns_fixed2_d, path_output = path_output, 
                                      name_res = "sim_Prob_d_5_Mill")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_d, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob e
r_e <- 0.99
a_e <- 0.99
ns_fixed1_e <- 30800
ns_fixed2_e <- 15400

set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_e, a=a_e, ns_fixed1 = ns_fixed1_e,
                                      ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                      name_res = "sim_Prob_e_5_Mill")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_e, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

################################################################################

# fixed parameters
sim=100
N = 10^6
k = 20
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

df_subsamples <- do.call(rbind, results)
write.csv(df_subsamples, file = paste0(path_output, "sim3_average_subsamples_LCC_6"),
          row.names = TRUE)




# General parameters
k = 20
sim <- 1000
N <- 10^6

#####################

# sim Prob a
r_a <- 0.7
a_a <- 0.7
ns_fixed1_a <- 151200
ns_fixed2_a <- 75600

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_a, a=a_a, ns_fixed1 = ns_fixed1_a,
                                      ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                      name_res = "sim_Prob_a_6_Mill")
#res_analysis_a <- res_analysis_sim3(res = res_a, k=k, a = a_a)

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_a, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob b
r_b <- 0.8
a_b <- 0.8
ns_fixed1_b <- 127400
ns_fixed2_b <- 63700

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_b, a=a_b, ns_fixed1 = ns_fixed1_b,
                                      ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                      name_res = "sim_Prob_b_5_Mill")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_b, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
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

# sim prob c
r_c <- 0.9
a_c <- 0.9
ns_fixed1_c <- 87600
ns_fixed2_c <- 43800

set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_c, a=a_c, ns_fixed1 = ns_fixed1_c,
                                      ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                      name_res = "sim_Prob_c_5_Mill")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_c, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)
beta_true

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

# sim prob d
r_d <- 0.95
a_d <- 0.95
ns_fixed1_d <- 57000
ns_fixed2_d <- 28500

set.seed(123)
res_d <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_d, a=a_d, ns_fixed1 = ns_fixed1_d,
                                      ns_fixed2 = ns_fixed2_d, path_output = path_output, 
                                      name_res = "sim_Prob_d_5_Mill")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_d, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)
beta_true

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

# sim prob e
r_e <- 0.99
a_e <- 0.99
ns_fixed1_e <- 18360
ns_fixed2_e <- 9180

set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r_e, a=a_e, ns_fixed1 = ns_fixed1_e,
                                      ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                      name_res = "sim_Prob_e_5_Mill")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r_e, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)
beta_true

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
