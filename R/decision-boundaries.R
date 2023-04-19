library(MASS)

set.seed(123)
k <- 2
N <- 100
r <- 0.5

mean1 <- c(rep(1, k))
mean0 <- c(rep(1, k))
cov_mat <- diag(k)

df <- dgp.imbalanced(N = N, r = r, distribution= "gaussian", k = k, mean1 = mean1
                     , mean0 = mean0, sigma1 = cov_mat, sigma0 = cov_mat)





# https://quantifyinghealth.com/plot-logistic-regression-decision-boundary-in-r/
# N = 50
# set.seed(1)
# x1 = rnorm(N)
# x2 = rnorm(N)
# y = (x1 + x2 + rnorm(N)) > 0
# model = glm(y ~ x1 + x2, family = binomial)
# x1_ticks = seq(min(x1), max(x1), length.out=80)
# x2_ticks = seq(min(x2), max(x2), length.out=80)
# background_grid = expand.grid(x1_ticks, x2_ticks)
# names(background_grid) = c('x1', 'x2')
# grid_predictions = predict(model,
#                            newdata = background_grid,
#                            type = 'response')
# plot(background_grid, pch = 20, cex = 0.5, col = ifelse(grid_predictions > 0.5, '#E75A7C', '#5398BE'))
# points(x1, x2, pch = 19, col = ifelse(y == TRUE, '#E75A7C', '#5398BE'))
# slope = coef(model)[2]/(-coef(model)[3])
# intercept = coef(model)[1]/(-coef(model)[3])
# clip(min(x1),max(x1), min(x2), max(x2))
# abline(intercept, slope, lwd = 2, lty = 2)



x1 = df$X1
x2 = df$X2
y = df$y
model = glm(y ~ x1 + x2, family = binomial)
x1_ticks = seq(min(x1), max(x1), length.out=80)
x2_ticks = seq(min(x2), max(x2), length.out=80)
background_grid = expand.grid(x1_ticks, x2_ticks)
names(background_grid) = c('x1', 'x2')
grid_predictions = predict(model,
                           newdata = background_grid,
                           type = 'response')
plot(background_grid, pch = 20, cex = 0.5, col = ifelse(grid_predictions > 0.5, '#E75A7C', '#5398BE'))
points(x1, x2, pch = 19, col = ifelse(y == TRUE, '#E75A7C', '#5398BE'))
slope = coef(model)[2]/(-coef(model)[3])
intercept = coef(model)[1]/(-coef(model)[3])
clip(min(x1),max(x1), min(x2), max(x2))
abline(intercept, slope, lwd = 2, lty = 2)
