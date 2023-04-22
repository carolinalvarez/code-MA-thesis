# DATA APPLICATION
library(ggplot2)
library(GGally)
library(corrplot)
library(ROSE)

rm(list = ls())
options(scipen = 999)
setwd("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/data/")

############################## CANCER DATASET ##############################  

df <- read.table("breast-cancer/ism.data", header = TRUE, sep = ",")
names(df)
names(df) <- paste0("X", 1:ncol(df))
table(df$X7)

df$X7 <- ifelse(df$X7 == 1, 0, 1)

table(df$X7)/nrow(df) # very large imbalance


cor_values <- cor(df[, -7])
corrplot(cor_values, method = "shade") # X4 and X6 might have a strong positive correlation with eachother


df_X1 <- df[df$X7 == 1, ]
df_X0 <- df[df$X7 == 0, ]



plots_list <- list()

for (col in colnames(df[, -7])) {
  p <- ggplot() +
    geom_density(data = df_X1, aes(x = .data[[col]], color = "Dataset X7 = 1"), alpha = 0.5) +
    geom_density(data = df_X0, aes(x = .data[[col]], color = "Dataset X7 = 0"), alpha = 0.5) +
    labs(x = col) +
    scale_color_discrete(name = "Dataset") +
    theme_classic()
  plots_list[[col]] <- p
}

plots_list[[1]]
plots_list[[2]]
plots_list[[3]]
plots_list[[4]]
plots_list[[5]]
plots_list[[6]]

# Investigating more whether there is conditional imbalance in the dataset
# per chatGPT: To better understand whether this relationship indicates conditional 
#imbalance, we should explore how the class distribution of Y varies across different 
#values of X, for example with scatterplots

ggpairs(df, aes(color = as.factor(X7)), columns = 1:6) +
  #ggtitle("Scatter plot matrix continuous features") +
  labs(color = "Class (Y)") + 
  theme_classic()



############################## ADULT DATASET ##############################  

# *** Data Cleaning ***

# url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/old.adult.names"
# v_names <- read.csv(url, header = FALSE)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
df_1 <- read.csv(url, header = FALSE)


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
df_2 <- read.csv(url, header = FALSE)

tmp01 <- rbind(df_1, df_2)
names(tmp01)

tmp01$y <- ifelse(tmp01$V15 == " >50K" | tmp01$V15 == " >50K.", 1, 0)
table(tmp01$y)/nrow(tmp01)


# Replace '?' with NA
tmp01[tmp01 == "?" | tmp01 == " ?"] <- NA
tmp02 <- na.omit(tmp01) # observations match with info in old.adult.names
table(tmp02$y)/nrow(tmp02)

cont_vars <- c("V1", "V3", "V5", "V11", "V12", "V13", "y")

df <- tmp02[, cont_vars]
str(df)

for (column in cont_vars) {
  df[[column]] <- as.numeric(df[[column]])
}

str(df)

var_names <- c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week", "y")
colnames(df) <- var_names

# *** Data Exploration ***

ggpairs(df, aes(color = as.factor(y)), columns = 1:6) +
  #ggtitle("Scatter plot matrix continuous features") +
  labs(color = "Class (Y)") + 
  theme_classic()

# ***Single Models ***

# Logistic regression

model_logit <- glm(as.formula(paste("y ~ ", paste(var_names[1:6], collapse= "+"))),
                   data = df, family = binomial)
model_logit$coefficients

# LCC
# pilot uses a 50-50 split

set.seed(123)
model_lcc <- lcc_algorithm_data_unfixed(data=df, a_wcc=0.7, xvars = var_names[1:6])

model_lcc$coef_adjusted
df_subsample_lcc <- model_lcc$subsample_lcc
table(df_subsample_lcc$y)

set.seed(123)
undersampled_data <- ovun.sample(y ~ ., data = df, method = "under"
                                 , p = 1)$data
undersampled_data$y <- as.factor(undersampled_data$y)
model_RS <- glm(y ~ ., data = undersampled_data, family = "binomial")

model_RS$coefficients
summary(model_RS)
