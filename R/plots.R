library(ggcorrplot)
library(ggplot2)
library(plotly)

path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"
pxl <- 300

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



