library(MASS)
k=6
sd1 = 1
N=10000
r=0.9
n_class1 <- ceiling(N * (1-r))
n_class0 <- ceiling(N * r)

mean_vec1 = c(0, 0, 2, 3, 1, 1)
mean_vec0 = rep(0,k)
cov_mat <- diag(k)


y0 <- rep(0, n_class0)
y1 <- rep(1, n_class1)

X_class1 <- matrix(mvrnorm(n_class1, mu = mean_vec1, Sigma = cov_mat, empirical = TRUE), ncol = k)
X_class1 <- cbind(y1, X_class1)

X_class0 <- matrix(mvrnorm(n_class0, mu = mean_vec0, Sigma = cov_mat, empirical = TRUE), ncol = k)
X_class0 <- cbind(y0, X_class0)

X <- as.data.frame(X_class1)
X2 <- as.data.frame(X_class0)
summary(X)
summary(X2)

sapply(X, sd)
sapply(X2, sd)

cor(X)
cor(X2)