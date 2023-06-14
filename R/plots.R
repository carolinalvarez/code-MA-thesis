library(ggplot2)
library(wesanderson)
library(dplyr)
library(tidyr)
library(MASS)
#library(ggcorrplot)
#library(pracma)
#library(RColorBrewer)
#library(plotly)
#library(reshape2)

#https://github.com/karthik/wesanderson/blob/master/R/colors.R
#https://github.com/karthik/wesanderson

path_output <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"
pxl <- 300

#colors
names(wes_palettes)
#brewer.pal(n = 8, name = "Dark2")

# Showing how the data looks like
# Based on simulation e

set.seed(123)

k <- 30
N <- 10^4
r <- 0.9
a <- 0.9
mean1 <- c(rep(1, k/2), rep(0, k/2))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

df_test <- dgp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)

# Run PCA on the data
pca <- prcomp(df_test[, -1], scale = TRUE)

# Extract the first two principal components
pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]

# Combine the principal components with the response variable
df_pca <- df_test %>%
  dplyr::select(y) %>%
  mutate(pc1 = pc1, pc2 = pc2)


# https://stackoverflow.com/questions/49363531/change-alpha-level-according-to-variables

ggplot(df_pca, aes(x = pc1, y = pc2, color = factor(y), alpha = factor(y)==0)) +
  geom_point(size = 0.6) +
  labs(x = "Principal Component 1", y = "Principal Component 2", color = "Class") +
  theme_classic() +
  scale_alpha_manual(values = c(0.4, 0.2), guide = "none") +
  scale_color_manual(values = c("grey", "#CB2314")) #46ACC8, DD8D29 A63126 CB2314

ggsave(paste(path_output, "plot_pca_1.png", sep = "")
       , dpi = pxl)

# Example with non class overlapp
k=8
N=3000
r=0.99
mean1 <- c(rep(1.5, k))
mean0 <- c(rep(0, k))
cov_mat <- diag(k)

df_test <- gdp.imbalanced(N=N, r=r, distribution = "gaussian", k=k, mean1=mean1, mean0=mean0, sigma1 = cov_mat, sigma0 = cov_mat)

# Run PCA on the data
pca <- prcomp(df_test[, -1], scale = TRUE)

# Extract the first two principal components
pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]

# Combine the principal components with the response variable
df_pca <- df_test %>%
  select(y) %>%
  mutate(pc1 = pc1, pc2 = pc2)

pal <- c("#7570B3", "#F46D43")

ggplot(df_pca, aes(x = pc1, y = pc2, color = factor(y), alpha = ifelse(y == 0, 0.5, 1))) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2", color = "Class") +
  theme_classic() +
  scale_color_manual(values = pal) +
  guides(alpha = "none")

ggsave(paste(path, "plot_pca_2.png", sep = "")
       , dpi = pxl)

#### The following was not used in the thesis, but remains as archive:

#plot(tmp02)
# Create a correlation matrix
corr_matrix <- cor(tmp02)

# Plot correlation matrix with ggcorrplot

ggcorrplot(corr_matrix, 
           #hc.order = TRUE, 
           type = "lower", 
           outline.color = "white", 
           ggtheme = ggplot2::theme_gray, 
           colors = c("#6D9EC1", "white", "#E46726"), 
           lab = TRUE, 
           lab_size = 4, 
           method = "circle", 
           title = "Correlation Matrix Plot")

ggsave(paste(path, "corr.png", sep = "")
       , dpi = pxl)



# Create a matrix of g values for different combinations of r and c
r_seq <- seq(0.1, 0.9, by = 0.1)
c_seq <- seq(0.1, 0.9, by = 0.1)
g_matrix <- outer(r_seq, c_seq, function(r, c) {
  b <- log(c/(1-c))
  g <- log(r/(1-r)) + b
  return(g)
})

# Reshape the g_matrix into a long format
df <- melt(g_matrix, varnames = c("r", "c"), value.name = "g")

# Create the contour plot using ggplot2
ggplot(df, aes(x = r, y = c, z = g)) +
  geom_contour() +
  labs(x = "r", y = "c", z = "g") +
  theme_bw()


contour(r_seq, c_seq, g_matrix, xlab = "r", ylab = "c", main = "Contour Plot of g(r,c)")




# Set range of values for r and c
r_vals <- seq(0.05, 0.95, by = 0.05)
c_vals <- seq(0.05, 0.95, by = 0.05)

# Create grid of all possible combinations of r and c
grid <- expand.grid(r = r_vals, c = c_vals)
# Calculate g for each combination of r and c
grid$g <- log(grid$r/(1-grid$r)) + log(grid$c/(1-grid$c))

plot_ly(x=grid$r, y=grid$c, z=grid$g, type="scatter3d", mode="markers", color=grid$g)
ggsave(paste(path, "3d.png", sep = "")
       , dpi = 300)



## show relation with c only, holding r constant


c_grid <- seq(0.1, 0.9, by = 0.1)

results <- data.frame(c = numeric(length(c_grid)),
                      coef = numeric(length(c_grid)))


for (i in 1:length(c_grid)) {
  
  c <- c_grid[i]
  cc_res <- cc_algorithm(df, c)
  # 
  results$c[i] <- c
  results$coef[i] <- cc_res$coef_unadjusted[1]
  
  
}

plot(results$c, results$coef, type = "h", xlab = "a(1)", ylab = "Adjusted Intercept") 
abline(h = cc_res$coef_unadjusted[1], lty = 2, col = "red")


