#' Author: Carolina Alvarez
#' Date: 16.02.2023
#' Data Generation Process following Fithian and Hastie 
#'
#' Generates data set that is flexible in amount of imbalance, size of training set and class overlapping 
#' (?) also distribution from which coufounders are drawn 


gdp.imbalanced <- function(N
                           , r
                           , distribution
                           , k
                           , mean1
                           , mean0
                           , sd1
                           , sd0) {
  
  y0 <- rep(0, N * r)
  y1 <- rep(1, N * (1-r))
  
  if (distribution=="gaussian"){
    X_class1 <- replicate(k, rnorm(N * (1-r), mean = mean1, sd=sd1))
    X_class1 <- cbind(y1, X_class1)
    
    X_class0 <- replicate(k, rnorm(N * r, mean = mean0, sd = sd0))
    X_class0 <- cbind(y0, X_class0)
    
    df <- as.data.frame(rbind(X_class1, X_class0))
    
    return(df)
    
  }
  else{
    stop("Distribution not allowed.")
  }

}

set.seed(123)
df2 <- gdp.imbalanced(N = 10000, r = 0.5, distribution= "gaussian", k=5, mean1=2.5, mean0=0, sd1=1, sd0=1)
