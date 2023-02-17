#' Author: Carolina Alvarez
#' Date: 16.02.2023
#' 
#' 
#' Data Generation Process following Fithian and Hastie 
#'
#' Generates data set that is flexible in amount of imbalance, size of training set and class overlapping 
#' (?) also distribution from which coufounders are drawn 
#' I think this will make the class overlapping flexible in the mean parameters?
#' parameter c: the proportion of 0's we want in the data


gdp.imbalanced <- function(N
                           , r
                           , distribution
                           , k
                           , mean1
                           , mean0
                           , sd1
                           , sd0) {
  
  n_class1 <- ceiling(N * (1-r))
  n_class0 <- ceiling(N * r)
  
  y0 <- rep(0, n_class0)
  y1 <- rep(1, n_class1)
  
  if (distribution=="gaussian"){
    X_class1 <- replicate(k, rnorm(n_class1, mean = mean1, sd=sd1))
    X_class1 <- cbind(y1, X_class1)
    
    X_class0 <- replicate(k, rnorm(n_class0, mean = mean0, sd = sd0))
    X_class0 <- cbind(y0, X_class0)
    
    df <- as.data.frame(rbind(X_class1, X_class0))
    
    colnames(df) <- c("y", paste0("X", 1:k))
    
    return(df)
    
  }
  else{
    stop("Distribution not allowed.")
  }
  
}

set.seed(123)

df <- gdp.imbalanced(N = 10000, r = 0.8, distribution= "gaussian", k=5, mean1=1.5, mean0=0, sd1=1, sd0=1)


table(df$y)

# set.seed(123)
# 

#' Case-Control subsampling by Fithian and Hastie 
#'
#' Algorithm for subsampling and fitting a logistic regression in the data
#' hyperparameter a(y): the proportion of 1's we want to subsample


cc_algorithm <- function(data, c){
  k <- length(data) - 1
  
  selection_bias <- log(c/(1-c))
  
  prob_function <- function(data, c){
    
    data$a <- ifelse(data$y == 0, 1 - c, c)
    
    return(data)
  }
  
  tmp01 <- prob_function(data, c)
  
  U <- runif(nrow(data), 0, 1)
  tmp01$U <- U
  
  tmp01$Z <- NA
  tmp01$Z <- ifelse(tmp01$U <= tmp01$a, 1, 0)
  
  tmp02 <- tmp01[tmp01$Z==1, ]
  
  xvars <- paste("X", 1:k, sep="")
  
  model_subsample <- glm(as.formula(paste("y ~ ", paste(xvars, collapse= "+")))
                         , data= tmp02
                         , family = binomial) #imp: remove a to avoid perfect separation and convergence issues
  
  coef_unadjusted <- as.vector(model_subsample$coefficients)
  
  beta0_adjusted <- coef_unadjusted[1] - selection_bias
  
  coef_adjusted <- c(beta0_adjusted, coef_unadjusted[2:(k+1)])
  
  res <- list("coef_unadjusted" = coef_unadjusted
              , "coef_adjusted" = coef_adjusted)
  
  return(res)
  
}

cc_algorithm(df, 0.8)






