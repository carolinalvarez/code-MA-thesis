#https://xiaoruizhu.github.io/Data-Mining-R/lecture/4.D_LogisticReg_ROC.html

credit_data <- read.csv(file = "https://xiaoruizhu.github.io/Data-Mining-R/lecture/data/credit_default.csv", header=T)

library(dplyr)
credit_data<- rename(credit_data, default=default.payment.next.month)

credit_data$SEX<- as.factor(credit_data$SEX)
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE)

index <- sample(nrow(credit_data),nrow(credit_data)*0.80)
credit_train = credit_data[index,]
credit_test = credit_data[-index,]

credit_glm0<- glm(default ~ ., family=binomial, data=credit_train)
credit_glm0<- glm(default ~ LIMIT_BAL + SEX + EDUCATION, family=binomial, data=credit_train)

summary(credit_glm0)

pred_resp <- predict(credit_glm0,type="response") #esto es lo que tengo que hacer a mano!
pred_resp <- as.data.frame(pred_resp)

test = 1/(1 + exp(6.781e-01+3.266e-06*140000+1.949e-01-1.893e-02))
test2 = 1/(1 + exp(6.781e-01+3.266e-06*60000+1.949e-01-5.569e-02))

pred <- prediction(pred_glm0_test, credit_test$default)

install.packages('ROCR')

library(ROCR)
pred_glm0_train<- predict(credit_glm0, type="response")
pred_glm0_train <- as.data.frame(pred_glm0_train)
#pred_glm0_train <- as.vector(pred_glm0_train)
pred_glm0_train <- as.array(pred_glm0_train)

pred <- prediction(pred_glm0_train, credit_train$default) #aqui le utilizo as data frame

perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

score1= pred_glm0_train[credit_train$default==1]
score0= pred_glm0_train[credit_train$default==0]

score0 = as.numeric(score0)
score1 = as.numeric(score1)

install.packages("PRROC")
library(PRROC)
roc= roc.curve(score1, score0, curve = T)
roc$auc
pr= pr.curve(score1, score0, curve = T)
pr
plot(pr, main="In-sample PR curve")

# Out-of-sample prediction: 
pred_glm0_test<- predict(credit_glm0, newdata = credit_test, type="response") #esto es lo que tengo q hacer a mano

score1.test= pred_glm0_test[credit_test$default==1]
score0.test= pred_glm0_test[credit_test$default==0]
roc.test= roc.curve(score1.test, score0.test, curve = T)
roc.test$auc

test = 1/(1 + exp(6.781e-01+3.266e-06*50000))



