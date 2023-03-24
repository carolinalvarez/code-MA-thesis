#' Author: Carolina Alvarez
#' Code for the simulation study 2
library(MASS)
library(pROC)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

################# Simulations decreasing k

# sim_smallk_a
set.seed(123)

sim <- 1000
k <- 10
N <- 10^5
r <- 0.9
a <- 0.9
ns_fixed1 <- 18000
ns_fixed2 <- 9000

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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_a.csv", row.names = FALSE)


# sim_smallk_b

set.seed(123)

sim <- 1000
k <- 10
N <- 10^4
r <- 0.9
a <- 0.9
ns_fixed1 <- 1800
ns_fixed2 <- 900

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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_b.csv", row.names = FALSE)


#sim_smallk_c

set.seed(123)

sim <- 1000
k <- 10
N <- 5000
r <- 0.9
a <- 0.9
ns_fixed1 <- 900
ns_fixed2 <- 450

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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_c.csv", row.names = FALSE)




# sim_smallk_d
# nota/pregunta: aqui deberia tomar un ns_fixed2 un poco mas alto que el mean porque en verdad los subsamples son tan pequenios que
# no tiene sentido poner un minimo, y hacerles mas pequenos en realidad en este caso puede ser muy grave
# para el bias y el variance. En el caso del ns_fixed1, no puede ser el doble porque ya el subsample es
#tan peqeuno que no se puede hacer un sampling sin replacement para que cuadre con ns_fixed1??

# Por ahora estoy haciendo como siempre...

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

set.seed(123)

sim <- 1000
k <- 10
N <- 2000
r <- 0.9
a <- 0.9
ns_fixed1 <- 360
ns_fixed2 <- 180

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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_e.csv", row.names = FALSE)

#sim_smallk_f

set.seed(123)

sim <- 1000
k <- 10
N <- 1500
r <- 0.9
a <- 0.9
ns_fixed1 <- 270
ns_fixed2 <- 130

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


write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_f.csv", row.names = FALSE)
