library(MASS)
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
