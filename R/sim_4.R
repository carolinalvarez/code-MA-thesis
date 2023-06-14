#' Author: Carolina Alvarez
#' Code for the simulation study 4
library(MASS)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"


# fixed parameters
sim=100
r = 0.9
k = 10
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

N_values <- c(10^5, 10^4, 5000, 2000, 1500)

results <- list()

set.seed(123)

for (N in N_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(N)]] <- result
}

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim4_average_subsamples_LCC"),
          row.names = TRUE)


################# Simulations decreasing k
# General parameters
k = 10
sim <- 1000
r <- 0.9
a <- 0.9
#####################

# sim_smallk_a
N <- 10^5
ns_fixed1 <- 18000
ns_fixed2 <- 9000
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_smallk_a_1")


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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar


# sim_smallk_b
N <- 10^4
ns_fixed1 <- 1800
ns_fixed2 <- 900

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_smallk_b_1")
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

squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

#write.csv(res, file = "C:/Users/Philipp/Desktop/code-MA-thesis copy/output/sim_smallk_b.csv", row.names = FALSE)


#sim_smallk_c
N <- 5000
ns_fixed1 <- 900
ns_fixed2 <- 450

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_smallk_c_1")
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

squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar



# sim_smallk_d


# set.seed(123)
# 
# sim <- 1000
# k <- 10
# N <- 1000
# r <- 0.9
# a <- 0.9
# ns_fixed1 <- 180
# ns_fixed2 <- 180
# 
# mean1 <- c(rep(1, k/2), rep(0, k/2))
# mean0 <- c(rep(0, k))
# cov_mat <- diag(k)
# 
# beta_names_cc <- paste0("β_hat_cc_", 0:k)
# beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
# beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
# 
# 
# output <- c(beta_names_cc, beta_names_wcc, beta_names_lcc)
# 
# res <- data.frame(matrix(ncol = length(output), nrow = 0))
# colnames(res) <- output
# 
# for (i in 1:sim) {
#   
#   df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
#                        , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
#   
#   
#   cc_output <- cc_algorithm_fixed(data=df, r=r, a=a, ns_fixed=ns_fixed1)
#   coef_adjusted_cc <- cc_output$coef_adjusted
#   
#   wcc_output <- wcc_algorithm_fixed(data=df, r=r, a=a, ns_fixed = ns_fixed1)
#   coef_unadjusted_wcc <- wcc_output$coef_unadjusted
#   
#   lcc_output <- lcc_algorithm_fixed(data=df, r=r, a_wcc=a, ns_fixed = ns_fixed2)
#   coef_adjusted_lcc <- lcc_output$coef_adjusted
#   
#   # a_bar(lambda)
#   #a_bar_lcc <- mean(lcc_output$a_bar_lcc)
#   
#   # Predicting subsampling
#   # y_hat_cc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_cc)
#   # y_hat_wcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_unadjusted_wcc)
#   # y_hat_lcc <- logit_predict(df_test, c(paste0("X", 1:k)), coef_adjusted_lcc)
#   
#   # Coefficients
#   res_cc <- data.frame(t(coef_adjusted_cc))
#   colnames(res_cc) <- beta_names_cc
#   
#   res_wcc <- data.frame(t(coef_unadjusted_wcc))
#   colnames(res_wcc) <- beta_names_wcc
#   
#   res_lcc <- data.frame(t(coef_adjusted_lcc))
#   colnames(res_lcc) <- beta_names_lcc
#   
#   #AUC
#   
#   # auc_cc <- as.numeric(roc(df_test$y, y_hat_cc)$auc)
#   # auc_wcc <- as.numeric(roc(df_test$y, y_hat_wcc)$auc)
#   # auc_lcc <- as.numeric(roc(df_test$y, y_hat_lcc)$auc)
#   
#   res <- rbind(res
#                , cbind(res_cc, res_wcc, res_lcc))
#   
# }
# 
# # take the mean of the results
# means <- data.frame(t(colMeans(res)))
# colnames(means) <- gsub("β_hat_", "", colnames(means))
# 
# 
# # True coefficient values
# beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
#                , rep(0, k/2))
# 
# beta_true <- rep(beta_true, 3)
# 
# # Calculate squared bias
# squared_bias <- (means - beta_true)^2
# 
# # Add column names to squared_bias
# colnames(squared_bias) <- colnames(means)
# 
# # Display squared_bias
# squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
# squared_bias_cc
# squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
# squared_bias_wcc
# squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
# squared_bias_lcc
# 
# # Take the variance of the realizations
# variances <- apply(res, 2, var)
# 
# var_cc <- sum(variances[1:as.numeric(k+1)])
# var_cc
# var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
# var_wcc
# var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
# var_lcc


#write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_d.csv", row.names = FALSE)


#sim_smallk_e
N <- 2000
ns_fixed1 <- 360
ns_fixed2 <- 180

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_smallk_e_1")

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


squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar


#sim_smallk_f
N <- 1500
ns_fixed1 <- 270
ns_fixed2 <- 130

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)


set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1,
                                      ns_fixed2 = ns_fixed2, path_output = path_output, 
                                      name_res = "sim_smallk_f_1")

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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar



####################################################################################
#################### Repeat simulations with diff P(Y=0) #######################
#################################################################################### 

path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

# fixed parameters
sim=100
r = 0.7
k = 10
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

N_values <- c(10^5, 10^4, 5000, 2000, 1500)

results <- list()

set.seed(123)

for (N in N_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(N)]] <- result
}

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim4_average_subsamples_LCC_2"),
          row.names = TRUE)



# General parameters
k = 10
sim <- 1000
r <- 0.7
a <- 0.7
#####################


# sim smallk a
N_a = 10^5
ns_fixed1_a <- 33400
ns_fixed2_a <- 16700

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_a, r=r, a=a, ns_fixed1 = ns_fixed1_a,
                                      ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                      name_res = "sim_smallk_a_2")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk b
N_b = 10^4
ns_fixed1_b <- 3280
ns_fixed2_b <- 1640

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_b, r=r, a=a, ns_fixed1 = ns_fixed1_b,
                                      ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                      name_res = "sim_smallk_b_2")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk c
N_c = 5000
ns_fixed1_c <- 1600
ns_fixed2_c <- 800

set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_c, r=r, a=a, ns_fixed1 = ns_fixed1_c,
                                      ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                      name_res = "sim_smallk_c_2")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

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
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk e
N_e = 2000
ns_fixed1_e <- 640
ns_fixed2_e <- 320

set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_e, r=r, a=a, ns_fixed1 = ns_fixed1_e,
                                      ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                      name_res = "sim_smallk_e_2")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk f
N_f = 1500
ns_fixed1_f <- 480
ns_fixed2_f <- 240

set.seed(123)
res_f <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_f, r=r, a=a, ns_fixed1 = ns_fixed1_f,
                                      ns_fixed2 = ns_fixed2_f, path_output = path_output, 
                                      name_res = "sim_smallk_f_2")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar
#################################  r = 0.8 #####################################

path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

# fixed parameters
sim=100
r = 0.8
k = 10
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

N_values <- c(10^5, 10^4, 5000, 2000, 1500)

results <- list()

set.seed(123)

for (N in N_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(N)]] <- result
}

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim4_average_subsamples_LCC_3"),
          row.names = TRUE)



# General parameters
k = 10
sim <- 1000
r <- 0.8
a <- 0.8
#####################


# sim smallk a
N_a = 10^5
ns_fixed1_a <- 27600
ns_fixed2_a <- 13800

set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_a, r=r, a=a, ns_fixed1 = ns_fixed1_a,
                                      ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                      name_res = "sim_smallk_a_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk b
N_b = 10^4
ns_fixed1_b <- 2720
ns_fixed2_b <- 1360

set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_b, r=r, a=a, ns_fixed1 = ns_fixed1_b,
                                      ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                      name_res = "sim_smallk_b_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk c
N_c = 5000
ns_fixed1_c <- 1340
ns_fixed2_c <- 670

set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_c, r=r, a=a, ns_fixed1 = ns_fixed1_c,
                                      ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                      name_res = "sim_smallk_c_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar


# sim smallk e
N_e = 2000
ns_fixed1_e <- 540
ns_fixed2_e <- 270

set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_e, r=r, a=a, ns_fixed1 = ns_fixed1_e,
                                      ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                      name_res = "sim_smallk_e_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk f
N_f = 1500
ns_fixed1_f <- 400
ns_fixed2_f <- 200

set.seed(123)
res_f <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_f, r=r, a=a, ns_fixed1 = ns_fixed1_f,
                                      ns_fixed2 = ns_fixed2_f, path_output = path_output, 
                                      name_res = "sim_smallk_f_3")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

#################################  r = 0.95 #####################################

path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

# fixed parameters
sim=100
r = 0.95
k = 10
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

N_values <- c(10^5, 10^4, 5000, 2000, 1500)

results <- list()

set.seed(123)

for (N in N_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(N)]] <- result
}

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim4_average_subsamples_LCC_4"),
          row.names = TRUE)



# General parameters
k = 10
sim <- 1000
r <- 0.95
a <- 0.95
#####################


# sim smallk a
N_a = 10^5
ns_fixed1_a <- 11242
ns_fixed2_a <- 5621
  
set.seed(123)
res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_a, r=r, a=a, ns_fixed1 = ns_fixed1_a,
                                      ns_fixed2 = ns_fixed2_a, path_output = path_output, 
                                      name_res = "sim_smallk_a_4")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk b
N_b = 10^4
ns_fixed1_b <- 1140
ns_fixed2_b <- 570 
  
set.seed(123)
res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_b, r=r, a=a, ns_fixed1 = ns_fixed1_b,
                                      ns_fixed2 = ns_fixed2_b, path_output = path_output, 
                                      name_res = "sim_smallk_b_4")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk c
N_c = 5000
ns_fixed1_c <- 590
ns_fixed2_c <- 295
  
set.seed(123)
res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_c, r=r, a=a, ns_fixed1 = ns_fixed1_c,
                                      ns_fixed2 = ns_fixed2_c, path_output = path_output, 
                                      name_res = "sim_smallk_c_4")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar


# sim smallk e
N_e = 2000
ns_fixed1_e <- 258
ns_fixed2_e <- 129
  
set.seed(123)
res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_e, r=r, a=a, ns_fixed1 = ns_fixed1_e,
                                      ns_fixed2 = ns_fixed2_e, path_output = path_output, 
                                      name_res = "sim_smallk_e_4")

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

# sim smallk f
N_f = 1500
ns_fixed1_f <- 208
ns_fixed2_f <- 104
  
set.seed(123)
res_f <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_f, r=r, a=a, ns_fixed1 = ns_fixed1_f,
                                      ns_fixed2 = ns_fixed2_f, path_output = path_output, 
                                      name_res = "sim_smallk_f_4")


means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))

beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 3)
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
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+7):length(squared_bias)-3])
squared_bias_logit

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

a_bar <- mean(res$a_bar_lcc)
a_bar

#################################  r = 0.99 #####################################
# 
# fixed parameters
sim=100
r = 0.99
k = 10
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

N_values <- c(10^5, 10^4, 5000, 2000, 1500)

results <- list()

set.seed(123)

for (N in N_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(N)]] <- result
}

df_subsamples <- do.call(rbind, results)


write.csv(df_subsamples, file = paste0(path_output, "sim4_average_subsamples_LCC_5"),
          row.names = TRUE)


# !!!! I am not running the ones for r=0.99 bcs the bias and var already for 0.9 and 0.95 is inf 
# # General parameters
# k = 10
# sim <- 1000
# r <- 0.99
# a <- 0.99
# #####################
# 
# 
# # sim smallk a
# N_a = 10^5
# ns_fixed1_a <- 
#   ns_fixed2_a <- 
#   
#   set.seed(123)
# res_a <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_a, r=r, a=a, ns_fixed1 = ns_fixed1_a,
#                                       ns_fixed2 = ns_fixed2_a, path_output = path_output, 
#                                       name_res = "sim_smallk_a_3")
# 
# # sim smallk b
# N_b = 10^4
# ns_fixed1_b <- 
#   ns_fixed2_b <- 
#   
#   set.seed(123)
# res_b <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_b, r=r, a=a, ns_fixed1 = ns_fixed1_b,
#                                       ns_fixed2 = ns_fixed2_b, path_output = path_output, 
#                                       name_res = "sim_smallk_b_3")
# 
# # sim smallk c
# N_c = 5000
# ns_fixed1_c <- 
#   ns_fixed2_c <- 
#   
#   set.seed(123)
# res_c <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_c, r=r, a=a, ns_fixed1 = ns_fixed1_c,
#                                       ns_fixed2 = ns_fixed2_c, path_output = path_output, 
#                                       name_res = "sim_smallk_c_3")
# 
# 
# # sim smallk e
# N_e = 2000
# ns_fixed1_e <- 
#   ns_fixed2_e <- 
#   
#   set.seed(123)
# res_e <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_e, r=r, a=a, ns_fixed1 = ns_fixed1_e,
#                                       ns_fixed2 = ns_fixed2_e, path_output = path_output, 
#                                       name_res = "sim_smallk_e_3")
# 
# # sim smallk f
# N_f = 1500
# ns_fixed1_f <- 
#   ns_fixed2_f <- 
#   
#   set.seed(123)
# res_f <- monte_carlo_runnings_sim_3_4(sim=sim, k=k, N=N_f, r=r, a=a, ns_fixed1 = ns_fixed1_f,
#                                       ns_fixed2 = ns_fixed2_f, path_output = path_output, 
#                                       name_res = "sim_smallk_f_3")
