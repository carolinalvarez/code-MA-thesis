#' Author: Carolina Alvarez
#' Code for the simulation study 3
#' Varying degree of unconditional imbalance
library(MASS)
library(pROC)

options(scipen = 999)
path_output <-"~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"

# Varying degree of imbalance (not conditional, just marginal imbalance)
# r = {0.7, 0.8, 0.9, 0.95, 0.99}

# the fact that p(Y=1) changes, also means that a(y) needs to be adjusted (this for ensuring that
# we get the same 50-50 split that Fithian and Hastie talk about)
# N needs to be fixed, so as k

# fixed parameters
sim=100
N = 10^5
k = 30
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

# getting average subsample LCC for each r value

r_values <- c(0.7, 0.8, 0.9, 0.95, 0.99)

results <- list()

set.seed(123)

for (r in r_values) {
  result <- average_subsample_size(N=N, k=k, a=r, r=r, mean1=mean1, mean0=mean0
                                   , sigma1=cov_mat, sigma0=cov_mat, sim=sim)
  results[[as.character(r)]] <- result
}

summary_df <- do.call(rbind, results)


write.csv(summary_df, file = paste0(path_output, "sim3_average_subsamples_LCC"), row.names = TRUE)


