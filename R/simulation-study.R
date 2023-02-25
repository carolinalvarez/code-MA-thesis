#' Author: Carolina Alvarez
#' Code for the simulation study
#' 
library(pROC)
set.seed(123)

# Train sets and model training
df <- gdp.imbalanced(N = 10000, r = 0.99, distribution= "gaussian", k=5, mean1=1, mean0=0, sd1=1, sd0=1)
table(df$y)

cc_output <- cc_algorithm(df, 0.8, 0.7)
df_subsample <- cc_output$subsample_S
df_subsample_test <- cc_output$subsample_test
cc_output$class_distr_subsample
coef_unadjusted <- cc_output$coef_unadjusted
coef_adjusted <- cc_output$coef_adjusted


# Split data into train and test sets while maintaining class proportions

strat <- strat_sampling(df, 0.70)
df_train <- strat$df_train
df_test <- strat$df_test

table(df_train$y)
table(df_test$y)

model_full <- glm("y~.", data = df_train, family = binomial)
coef_full <- model_full$coefficients


#y_hat_df <- predict(model_full, newdata = df_test, type = 'response')
y_hat_df <- logit_predict(df_test, c("X1", "X2", "X3", "X4", "X5"), coef_full)
y_hat_subsample <- logit_predict(df_subsample_test, c("X1", "X2", "X3", "X4", "X5"), coef_adjusted)

test_roc_df = roc(df_test$y ~ y_hat_df, plot = TRUE, print.auc = TRUE)
test_roc_df$auc
test_roc_s = roc(df_subsample_test$y ~ y_hat_subsample, plot = TRUE, print.auc = TRUE)
test_roc_s$auc

get.true.intercept(0.01, rep(0.5, 5), rep(1, 5))
coef_full
cc_output$coef_adjusted


# function for the simulations







