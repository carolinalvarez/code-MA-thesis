library(ggcorrplot)
library(ggplot2)


#plot(tmp02)
# Create a correlation matrix
corr_matrix <- cor(tmp02)

# Plot correlation matrix with ggcorrplot

png(file="~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/plot_corr.png",
    width=600, height=350)
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
dev.off()