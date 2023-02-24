# Function for predicting values within the context of Fithian and Hastie (2014)
# Author: Carolina Alvarez
# 06.01.2023

# In the context of Fithian and Hastie (2014), after doing the subsampling and fitting the logit regression on the
#subsample, one needs to adjust the coefficients in order to recover the estimates for the whole sample. In such a case,
# I would need to manually compute the prediction function so it takes external betas and not the betas from previously
# created object from the glm function logit (i.e., model$coefficients)


# Main reference: https://rdrr.io/cran/car/src/R/Predict.R

logit_predict <- function(data, names.use, betas){
  # data (dataframe): data for either training or testing the model
  # names.use (vector): vector containing strings with names of features (columns of 'data')
  # betas (vector): vector containing beta estimators (IMP for Fithian and Hastie (2014))
  
  X <- as.matrix(cbind(rep(1), data[, names.use]))
  colnames(X) <- c("intercept", names.use)
  n <- nrow(X)
  p <- ncol(X)
  p1 <- seq_len(p) 
  beta <- betas 
  predictor_vector <- 1/(1+exp((-1)*drop(X[, p1, drop = FALSE] %*% beta[p1])))
  return(predictor_vector)
  
}


# Data and benchmark model for testing the function

library(dplyr)
path <- "https://xiaoruizhu.github.io/Data-Mining-R/lecture/data/credit_default.csv"
credit_data <- read.csv(file = path, header=T)
credit_data<- rename(credit_data
                     , default=default.payment.next.month)

credit_data$SEX<- as.factor(credit_data$SEX)
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE)
credit_data$SEX1 <- ifelse(credit_data$SEX==1, 1, 0)
credit_data$SEX2 <- ifelse(credit_data$SEX==2, 1, 0)
credit_data$EDUCATION1 <- ifelse(credit_data$EDUCATION==1, 1,0)
credit_data$EDUCATION2 <- ifelse(credit_data$EDUCATION==2, 1,0)
credit_data$EDUCATION3 <- ifelse(credit_data$EDUCATION==3, 1,0)
credit_data$EDUCATION4 <- ifelse(credit_data$EDUCATION==4, 1,0)


set.seed(123) #to get always the same sample
index <- sample(nrow(credit_data),nrow(credit_data)*0.80)
credit_train = credit_data[index,]
credit_test = credit_data[-index,]

#credit_glm0<- glm(default ~ ., family=binomial, data=credit_train)
#credit_glm0<- glm(default ~ LIMIT_BAL + SEX + EDUCATION, family=binomial, data=credit_train)
credit_glm0<- glm(default ~ LIMIT_BAL + SEX2 + EDUCATION2 + EDUCATION3 + EDUCATION4, family=binomial, data=credit_train)
summary(credit_glm0)

predictor_1 <- predict(credit_glm0, newdata = credit_test, type = 'response')

#testing the function with the results from credit_glm0 

predictor_2 <- logit_predict(credit_test
                             , c("LIMIT_BAL", "SEX2", 'EDUCATION2', 'EDUCATION3', 'EDUCATION4')
                             , c(-6.474e-01
                                 , -3.268e-06
                                 , -2.119e-01
                                 , 3.811e-02
                                 , 3.738e-02
                                 , -1.060e+00))

# Result: the function logit_predict gives almost identical results as the build in R function 
#predict.

# further steps:

# once you have a prediction, it is easy to use it as the input of the ROC function in R, so you can both plot
# the roc and calculate the auc

#https://www.geeksforgeeks.org/how-to-calculate-auc-area-under-curve-in-r/

