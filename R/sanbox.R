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

#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' Other functions for the plot of g(X)
#' 
#' 
#' ###


library(tidyverse)

# Set up grid of values for r and c
r_values <- seq(0.1, 0.9, by = 0.1)
c_values <- seq(0.1, 0.9, by = 0.1)

# Generate data
set.seed(123)
N <- 10000
k <- 5
mean1 <- 1.5
mean0 <- 0
sd1 <- 1
sd0 <- 1

# Compute f(x)
f <- function(x, r) log(r/(1-r))

# Compute g(x) for each value of r and c
results <- expand.grid(r = r_values, c = c_values) %>%
  rowwise() %>%
  mutate(data = gdp.imbalanced(N, r, "gaussian", k, mean1, mean0, sd1, sd0)) %>%
  mutate(subsample = cc_algorithm(data, c)$subsample_S) %>%
  mutate(g = f(subsample$X1, r) + log(c/(1-c)))

# Plot g(x) against f(x)
ggplot(results, aes(x = subsample$X1, y = g)) +
  geom_line(color = "red", size = 1) +
  geom_line(aes(y = f(subsample$X1, r)), color = "blue", size = 1) +
  scale_y_continuous(limits = c(-3, 3)) +
  xlab("X") +
  ylab("g(X)") +
  facet_grid(~r) +
  theme_minimal()


####


gdp.imbalanced <- function(N, r, distribution, k, mean1, mean0, sd1, sd0) {
  
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

# Generate a list of data frames with different values of r and c
r_vals <- seq(0.1, 0.9, by = 0.1)
c_vals <- seq(0.1, 0.9, by = 0.1)
data_list <- lapply(r_vals, function(r) {
  lapply(c_vals, function(c) {
    gdp.imbalanced(N = 10000, r = r, distribution = "gaussian", k = 5, mean1 = 1.5, mean0 = 0, sd1 = 1, sd0 = 1)
  })
})

# Plot the log-odds function for each combination of r and c
ggplot() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("X") +
  ylab("g(x)") +
  expand_limits(x = 0, y = 0) +
  scale_color_gradient(low = "blue", high = "red") +
  facet_grid(rows = vars(r), cols = vars(c)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_smooth(data = data_list, aes(x = X1, y = log(y/(1-y)), color = as.factor(c)), method = "glm", family = "binomial", se = FALSE) +
  guides(color = guide_legend(title = "c"))


