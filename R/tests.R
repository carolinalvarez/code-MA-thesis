library(MASS)
library(docstring)
library(roxygen2)
set.seed(3022)

# test for the DGP

k=8
N=10000
r=0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)


df_test <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)

df_test_class1 <- df_test[df_test$y==1,]
df_test_class0 <- df_test[df_test$y==0,]

summary(df_test_class1)
summary(df_test_class0)

sapply(df_test_class1, sd)
sapply(df_test_class0, sd)


cor(df_test_class1)
cor(df_test_class0)



# create histograms for each variable
par(mfrow = c(2,3))
for (i in 2:7) {
  hist(df_test_class1[,i], main = colnames(df_test_class1)[i], xlab = "", xlim = c(min(df_test_class1[,i]), max(df_test_class1[,i])))
  abline(v = mean(df_test_class1[,i]), lwd = 2, col = "red")
}

# Test for getting true coefficients
# replicating a bit Wang (2020)
k=1
cov_mat <- diag(k)
test <- gdp.imbalanced(N=1000, r=0.98, distribution = "gaussian", k=1, mean1 = 1, mean0 = 0, sigma1 = cov_mat, sigma0 = cov_mat)

summary(test)

get.true.intercept(r=0.02, 0.5, 1)
get.true.intercept(r=0.004, 0.5, 1)
get.true.intercept(r=0.0008, 0.5, 1)


# Test for CC function
k=10
N=50000
r=0.99
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)
a = 0.7
split_r = 0.7

data <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)

selection_bias <- log(a/(1-a))

prob_function <- function(data, a){
  
  data$a <- ifelse(data$y == 0, 1 - a, a)
  
  return(data)
}

tmp01 <- prob_function(data, a)
U <- runif(nrow(data), 0, 1)
tmp01$U <- U
tmp01$Z <- NA
tmp01$Z <- ifelse(tmp01$U <= tmp01$a, 1, 0)
tmp02 <- tmp01[tmp01$Z==1, ] 
class_distr_subsample <- table(tmp02$y)

# comprobando que el tamano del subsample sea el correcto
a_bar(0.7, r=r)
nrow(tmp02)/nrow(data)

# comprobando que tenga la misma proporcion de 1 y 0s que el que deberia
table(tmp02$y)/nrow(tmp02)
prop_Ps(a=a, r=r)

#Train and test
train_idx <- c()
test_idx <- c()
for (i in c('0', '1')) {
  idx <- which(tmp02$y == i)
  n_train <- round(length(idx) * split_r) 
  train_idx_i <- sample(idx, n_train)
  test_idx_i <- setdiff(idx, train_idx_i)
  train_idx <- c(train_idx, train_idx_i)
  test_idx <- c(test_idx, test_idx_i)
}

# Create train and test sets
tmp02_train <- tmp02[train_idx, ]
tmp02_test <- tmp02[test_idx, ]


# comprobando que tenga la misma proporcion de 1 y 0s que el que deberia
table(tmp02_train$y)/nrow(tmp02_train)
prop_Ps(a=a, r=r)

# comprobando que tenga la misma proporcion de 1 y 0s que el que deberia
table(tmp02_test$y)/nrow(tmp02_test)
prop_Ps(a=a, r=r)

xvars <- paste("X", 1:k, sep="")
xvars

model_subsample <- glm(as.formula(paste("y ~ ", paste(xvars, collapse= "+")))
                       , data= tmp02_train # es con train, correcto
                       , family = binomial) 

summary(model_subsample) #aqui es full raro porque de la X3 a la X6 tienen coef super cercanos a 0... cuando la dgp es mean1 <- c(rep(1, k/2), rep(0, k/2))

coef_unadjusted <- as.vector(model_subsample$coefficients)
beta0_adjusted <- coef_unadjusted[1] - selection_bias
-2.81855 - log(a/(1-a))
beta0_adjusted

coef_adjusted <- c(beta0_adjusted, coef_unadjusted[2:(k+1)])
coef_adjusted

get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2)))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

# Calculate squared bias
squared_bias <- (coef_adjusted - beta_true)^2


# Display squared_bias
squared_bias
squared_bias_cc <- sum(squared_bias)
squared_bias_cc




# taking samples
prop_Ps(0.99,0.99)


# Ns function, test for proportions of classes

k <- 6
Ns <- 1000
r <- 0.96 # P(Y=0)
a <- 0.7
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)


df_test <- gdp.imbalanced.Ns(a=a, r=r, distribution = "gaussian", Ns_size = Ns, k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)

table(df_test$y)

df_test_class1 <- df_test[df_test$y==1,]
df_test_class0 <- df_test[df_test$y==0,]

summary(df_test_class1)
summary(df_test_class0)

sapply(df_test_class1, sd)
sapply(df_test_class0, sd)


cor(df_test_class1)
cor(df_test_class0)


# testing the LCC function piece by piece

k=40
N=100000
r=0.9
a=0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)
a_wcc <- a


df_test <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)

strat <- strat_sampling(df_test, 0.70)
data_train <- strat$df_train
df_test <- strat$df_test

k <- length(data_train) - 1 # we take "y" out

#here, despite using the strat_sampling function that is built to separate 
#into train and test, we use this split as two training sets for each algorithm.
# The only thing i want to make sure is that the split is stratified.
data_split <- strat_sampling(data_train, 0.5)
data_wcc <- data_split$df_train
data_lcc <- data_split$df_test

table(data_lcc$y)
table(data_wcc$y) #tienen q tener las mismas propociones, y las tienen.

#run the pilot

wcc_output <- wcc_algorithm(data_wcc, a_wcc)
coef_unadjusted_wcc <- wcc_output$coef_unadjusted

#predict on LCC data
y_hat <- logit_predict(data_lcc, c(paste0("X", 1:k)), coef_unadjusted_wcc)


prob_function <- function(data, y_hat){
  
  data$a <- ifelse(data$y == 0, 1 - a, a)
  data$a <- ifelse(data$y == 0, y_hat, 1-y_hat)
  
  return(data)
}

tmp01 <- prob_function(data_lcc, y_hat)
tmp01$y_hat <- y_hat

a_bar_lcc_1 <- mean(tmp01$a)
a_bar_lcc_1

U <- runif(nrow(tmp01), 0, 1) # TO DO: in CC instead of tmp01 I wrote data... same?

tmp01$U <- U

tmp01$Z <- NA
tmp01$Z <- ifelse(tmp01$U <= tmp01$a, 1, 0)

xvars <- paste("X", 1:k, sep="")

model_subsample <- glm(as.formula(paste("y ~ ", paste(xvars, collapse= "+")))
                       , data= tmp02
                       , family = binomial) #imp: remove a to avoid perfect separation and convergence issues

coef_unadjusted_lcc <- as.vector(model_subsample$coefficients)

coef_adjusted_lcc <- coef_unadjusted_lcc + coef_unadjusted_wcc

coef_unadjusted_wcc
coef_adjusted_lcc


# checking the value of a bar

k=36
N=100000
r=0.9
a=0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)
a_wcc <- a

res <- c()

for (i in 1:100) {
  
  df_test <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  strat <- strat_sampling(df_test, 0.70)
  data_train <- strat$df_train
  df_test <- strat$df_test
  
  k <- length(data_train) - 1 # we take "y" out
  
  #here, despite using the strat_sampling function that is built to separate 
  #into train and test, we use this split as two training sets for each algorithm.
  # The only thing i want to make sure is that the split is stratified.
  data_split <- strat_sampling(data_train, 0.5)
  data_wcc <- data_split$df_train
  data_lcc <- data_split$df_test
  
  table(data_lcc$y)
  table(data_wcc$y) #tienen q tener las mismas propociones, y las tienen.
  
  #run the pilot
  
  wcc_output <- wcc_algorithm(data_wcc, a_wcc)
  coef_unadjusted_wcc <- wcc_output$coef_unadjusted
  
  #predict on LCC data
  y_hat <- logit_predict(data_lcc, c(paste0("X", 1:k)), coef_unadjusted_wcc)
  
  
  prob_function <- function(data, y_hat){
    
    data$a <- ifelse(data$y == 0, 1 - a, a)
    data$a <- ifelse(data$y == 0, y_hat, 1-y_hat)
    
    return(data)
  }
  
  tmp01 <- prob_function(data_lcc, y_hat)
  tmp01$y_hat <- y_hat
  
  a_bar_lcc_1 <- mean(tmp01$a)
  a_bar_lcc_1
  
  res[i] <- a_bar_lcc_1

}

mean(res)

# > mean(res)
# [1] 0.01437685 


# testing the seed

set.seed(123)

k=4
N=1000
r=0.9
a=0.7
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)
a_wcc <- a


df_test1 <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)
df_test2 <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)
df_test3 <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)

summary(df_test1)
summary(df_test2)
summary(df_test3)

#' No necesitas .rs.restart(). Si se vuelve a correr el seed, se vuelve a tener los mismos datos
#' El seed asegura que en cada run se generen los mismos datos
#' Poner en frente de cada simulacion de ahora en adelante



# Testing the size of the subsample with current lcc algorithm

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
  
  #size 
  lcc_size <- nrow(lcc_output$subsample_lcc)
  cc_size <- nrow(cc_output$subsample_cc)
  wcc_size <- nrow(wcc_output$subsample_wcc)
  pilot_size <- nrow(lcc_output$subsample_pilot)
  
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
                       a_bar_lcc, cc_size, wcc_size, pilot_size, lcc_size))
  
}

write.csv(res, file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_b_2.csv", row.names = FALSE)

summary(res$cc_size)
summary(res$wcc_size)
summary(res$pilot_size)
summary(res$lcc_size)
# samples sizes are extremely different

res <- read.csv(file = "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_b_2.csv")


# subsample fixed for cc

# Test for CC function
k=30
N=100000
r=0.9
a = 0.7
N_s = 1000

mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

set.seed(123)
data <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0
                       , sigma1 = cov_mat, sigma0 = cov_mat)

#cc
out_test <- cc_algorithm_fixed(data=data, r=r, a=a, ns_fixed = 2000)
df_test_subsample <- out_test$subsample_cc
nrow(df_test_subsample)
table(df_test_subsample$y)/nrow(df_test_subsample)
summary(df_test_subsample)

# the same subsample is always drawn 

#wcc
out_test_wcc <- wcc_algorithm_fixed(data=data, r=r, a=a, ns_fixed = 2000)
df_test_subsample <- out_test_wcc$subsample_wcc
nrow(df_test_subsample)
table(df_test_subsample$y)/nrow(df_test_subsample)
summary(df_test_subsample)

out_test_wcc$coef_unadjusted
out_test$coef_adjusted

#lcc

out_test_lcc <- lcc_algorithm_fixed(data=data, r=r, a_wcc = a, ns_fixed = 1000)
df_test_subsample <- out_test_lcc$subsample_lcc
df_test_subsample2 <- out_test_lcc$subsample_lcc_fixed
table(df_test_subsample$y)/nrow(df_test_subsample)
table(df_test_subsample2$y)/nrow(df_test_subsample2)

out_test_wcc$coef_unadjusted
out_test$coef_adjusted
out_test_lcc$coef_adjusted

# las proporciones son muy parecidas!! :)




# Testing how much sample size for lcc on average given N
# for the other algorithms, it is straightforward to calculate since we can use a_bar
# I use the lcc_algorithm_v2 because what I want to get is the subsample when there are absolutly no
# restrictions, aka when the algorithm is left alone and free. True subsample size of LCC

k=30
N=500000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res[i] <- lcc_size
  
}

mean(res) #10681.7


set.seed(123)

k=30
N=10^4
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 300
res1 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res1[i] <- lcc_size
  
}

mean(res1) #222.13


set.seed(123)

k=30
N=50000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res1 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res1[i] <- lcc_size
  
}

mean(res1) # mean 1072.28, max 1185


set.seed(123)

k=30
N=200000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #4279.79


set.seed(123)

k=30
N=20000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) # 432
summary(res2) #max 504



set.seed(123)

k=10
N=10^5
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #
summary(res2) #max 9407 


set.seed(123)

k=10
N=10^4
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #
summary(res2)


set.seed(123)

k=10
N=1000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 300
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #
summary(res2)



set.seed(123)

k=10
N=5000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #
summary(res2)


set.seed(123)

k=10
N=2000
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #
summary(res2)


set.seed(123)

k=10
N=1500
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

sim <- 100
res2 <- NA

for (i in 1:sim) {
  
  df_test <- dgp.imbalanced(N=N, r=r, distribution="gaussian", 
                            k=k, mean1 = mean1, mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)
  
  output_test <- lcc_algorithm_v2(data=df_test, a_wcc = a)
  lcc_size <- nrow(output_test$subsample_lcc)
  res2[i] <- lcc_size
  
}

mean(res2) #
summary(res2)


# testing the average subsample function
set.seed(123)
k=10
N=1500
r=0.9
a = 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

average_subsample_size(N=N, k=k, a=a, r=r, mean1=mean1, mean0 = mean0
                       , sigma1 = cov_mat, sigma0 = cov_mat, sim=100)





####

test_lcc <- res[as.numeric(k+k+3):as.numeric(k+k+k+3)]
summary(test_lcc)


############## Testing new function for monte carlo simulations #################

#baseline run
set.seed(123)

path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

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

res1 <- res


# new function
set.seed(123)

res2 <- monte_carlo_runnings_NsFixed(sim=sim, k=k, N=N, r=r, a=a, ns_fixed1 = ns_fixed1
                             , ns_fixed2 = ns_fixed2, path_output = path_output
                             , name_res = "test.csv")

#comparison

identical(apply(res1, 2, mean), apply(res2, 2, mean)) # TRUE


# checking also for function for analysing the results

res <- res1

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


test <- res_analysis_sim3(res=res2, k=k, a=a)
test$beta_true
test$squared_bias_cc
squared_bias_cc
test$squared_bias_wcc
squared_bias_wcc
test$squared_bias_lcc
squared_bias_lcc # son iguales
