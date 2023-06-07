# Archived simulations 
# Not presented in the final paper, but results can still be found in the
# code-MA-thesis/output/ folder.

################################################################################
############################### Simulation 1 ##################################
################################################################################

set.seed(123)

sim <- 200
k <- 10
N <- 300
r <- 0.9 # P(Y=0)
a <- 0.6
mean1 <- c(rep(1, k/2), rep(0, k/-2))
#mean1 <- c(rep(0.4, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

beta_names <- paste0("β_hat_", 0:k)
beta_names_adj <- paste0("β_hat_adj_", 0:k)

output <- c(beta_names, beta_names_adj, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
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

df_test <- dgp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df_Ns <- dgp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
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

df_test <- dgp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df_Ns <- dgp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
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

df_test <- dgp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
                             , k= k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat
                             , sigma0 = cov_mat)

beta_names_cc <- paste0("β_hat_cc_", 0:k)
beta_names_wcc <- paste0("β_hat_wcc_", 0:k)

output <- c(beta_names_cc, beta_names_wcc, "auc")

res <- data.frame(matrix(ncol = length(output), nrow = 0))
colnames(res) <- output

for (i in 1:sim) {
  
  df_Ns <- dgp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns
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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_h.csv", row.names = FALSE)



# Simulation E_3

# set.seed(123)
# 
# sim <- 1000
# k <- 30
# N <- 10^5
# r <- 0.9
# a <- 0.9
# ns_fixed1 <- 4200
# ns_fixed2 <- 2100
# 
# mean1 <- c(rep(1, k/2), rep(0, k/2))
# mean0 <- c(rep(0, k))
# cov_mat <- diag(k)
# 
# beta_names_cc <- paste0("β_hat_cc_", 0:k)
# beta_names_wcc <- paste0("β_hat_wcc_", 0:k)
# beta_names_lcc <- paste0("β_hat_lcc_", 0:k)
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
# 
# write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_e_3.csv", row.names = FALSE)
# 
# 


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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
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
  
  df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_d.csv", row.names = FALSE)


# Simulation F

set.seed(123)

sim <- 1000
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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_g.csv", row.names = FALSE)

# Simulation E_1:


set.seed(123)

sim <- 1000
k <- 30
N <- 200000
r <- 0.9
a <- 0.9
ns_fixed1 <- 8000
ns_fixed2 <- 4000

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


write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_e_1.csv", row.names = FALSE)



# Simulation E_2:


set.seed(123)

sim <- 300
k <- 30
N <- 50000
r <- 0.9
a <- 0.9
ns_fixed1 <- 2400
ns_fixed2 <- 1200

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

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_e_2.csv", row.names = FALSE)



################################################################################
############################### Simulation 2 ##################################
################################################################################

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
N <- 10^5
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
    
    lcc_subsample <- logit_output$subsample_lcc
    
    # Coefficients
    res_logit <- data.frame(t(coef_logit))
    colnames(res_logit) <- beta_names_log
    
    res_lcc <- data.frame(t(coef_adjusted_lcc))
    colnames(res_lcc) <- beta_names_lcc
    
    
    
    
    res <- rbind(res
                 , cbind(res_logit, res_lcc, a_bar_lcc, lcc_subsample))
    
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

a_bar <- mean(res$a_bar_lcc)
a_bar


write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_L.csv", row.names = FALSE)



################################################################################
############################### Simulation 3 ##################################
################################################################################

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

