#' Author: Carolina Alvarez
#' Date: 16.02.2023
#' 
#' 
#' Data Generation Process following Fithian and Hastie 
#'
#' Generates data set that is flexible in amount of imbalance, size of training set and class overlapping 
#' (?) also distribution from which coufounders are drawn 
#' I think this will make the class overlapping flexible in the mean parameters?


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
    
    return(df)
    
  }
  else{
    stop("Distribution not allowed.")
  }
  
}

df2 <- gdp.imbalanced(N = 10000, r = 0.8, distribution= "gaussian", k=5, mean1=2.5, mean0=0, sd1=1, sd0=1)


table(df2$y1)


#' Case-Control subsampling by Fithian and Hastie 
#'
#' Algorithm for subsampling and fitting a logistic regression in the data
#' 




