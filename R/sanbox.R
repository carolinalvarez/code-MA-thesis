prob_function <- function(data, c){
  data$a <- ifelse(data$y == 0, 1 - c, c)
  return(data)
}

c <- 0.8
k <- 5

tmp01 <- prob_function(df, c)
table(tmp01$a)


#log selection bias
log(c/(1-c))

ui <- runif(10000, 0, 1)
tmp01$ui <- ui

tmp01$zi <- NA

tmp01$zi <- ifelse(tmp01$ui <= tmp01$a, 1, 0)

tmp02 <- tmp01[tmp01$zi==1, ]
table(tmp02$y)
table(tmp02$a)

xvars <- paste("X", 1:k, sep="")

model_subsample <- glm(as.formula(paste("y ~ ", paste(xvars, collapse= "+"))), data= tmp02, family = binomial) #imp: remove a to avoid perfect separation and convergence issues

coef_subsample <- as.vector(model_subsample$coefficients)
coef_subsample
beta0_adjusted <- coef_subsample[1] - log(c/(1-c))

coef_adjusted <- c(beta0_adjusted, coef_subsample[2:(k+1)])

coef_adjusted

