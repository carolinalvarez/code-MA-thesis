# DATA APPLICATION
library(ggplot2)
library(GGally)
library(corrplot)
library(ROSE)
library(stargazer)

rm(list = ls())
options(scipen = 999)
setwd("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/data/")
path_output <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/data-application/"

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

table(df$y)/nrow(df)

p0 <- as.numeric(as.vector(table(df$y)/nrow(df))[1])
p1 <- as.numeric(as.vector(table(df$y)/nrow(df))[2])
# *** Data Exploration ***




# Create a LaTeX table with stargazer
stargazer(df, title = "Summary Statistics - Adult (Income) dataset",
          label = "tbl:summary_stats_50k",
          table.placement = "H",
          header = FALSE,
          type = "latex",
          out=paste0(path_output, "summ-stats.tex"))

# only for summ stats
tmp03 <- df
tmp03$class_1 <- ifelse(tmp03$y==1, 1, NA)
tmp03$class_0 <- ifelse(tmp03$y==0, 1, NA)

tmp04 <- tmp03[, c("age", "fnlwgt", "education_num", "capital_gain"
                 , "capital_loss", "hours_per_week", "class_1", "class_0")]

tmp05 <- tmp04[tmp04$class_1==1, ]
tmp06 <- tmp04[tmp04$class_0==1, ]


stargazer(tmp04
          , title = "Summary Statistics - Adult (Income) dataset",
          label = "tbl:summary_stats_50k_2",
          table.placement = "H",
          header = FALSE,
          type = "latex",
          out=paste0(path_output, "summ-stats-2.tex"))

stargazer(tmp05
          , title = "Summary Statistics - Adult (Income) dataset",
          label = "tbl:summary_stats_50k_2",
          table.placement = "H",
          header = FALSE,
          type = "latex",
          out=paste0(path_output, "summ-stats-3.tex"))

stargazer(tmp06
          , title = "Summary Statistics - Adult (Income) dataset",
          label = "tbl:summary_stats_50k_2",
          table.placement = "H",
          header = FALSE,
          type = "latex",
          out=paste0(path_output, "summ-stats-4.tex"))


ggpairs(df, aes(color = as.factor(y)), columns = 1:6) +
  #ggtitle("Scatter plot matrix continuous features") +
  labs(color = "Class (Y)") + 
  theme_classic()

# ***Single Models ***

# Logistic regression

model_logit <- glm(as.formula(paste("y ~ ", paste(var_names[1:6], collapse= "+"))),
                   data = df, family = binomial)
model_logit$coefficients

# CC
set.seed(123)
model_cc <- cc_algorithm_data(data=df, a=p0, xvars = var_names[1:6])
model_cc$coef_adjusted
df_subsample_cc <- model_cc$subsample_cc
table(df_subsample_cc$y)

#WCC
model_wcc <- wcc_algorithm_data(data=df, a=p0, xvars = var_names[1:6])
model_wcc$coef_unadjusted
df_subsample_wcc <- model_wcc$subsample_wcc
table(df_subsample_wcc$y)

# LCC
# pilot uses a 50-50 split

set.seed(123)
model_lcc <- lcc_algorithm_V2_data(data=df, a_wcc=p0, xvars = var_names[1:6])

model_lcc$coef_adjusted
df_subsample_lcc <- model_lcc$subsample_lcc
table(df_subsample_lcc$y)
mean(model_lcc$a_bar_lcc) #0.2658143

# Random Undersampling
set.seed(123)
undersampled_data <- ovun.sample(y ~ ., data = df, method = "under"
                                 , p = p0)$data
undersampled_data$y <- as.factor(undersampled_data$y)
model_RS <- glm(y ~ ., data = undersampled_data, family = "binomial")

model_RS$coefficients
summary(model_RS)
table(undersampled_data$y)


# taking average LCC size for fixing the subsample


set.seed(123)
summary_df1 <- as.matrix(average_subsample_size_data(data=df, a = p0
                                                     , xvars = var_names[1:6]
                                                     , rep=100
                                                     , algorithm = "cc"
                                                     , type = "a-fixed"))
set.seed(123)
summary_df2 <- as.matrix(average_subsample_size_data(data=df, a = p0
                                                     , xvars = var_names[1:6]
                                                     , rep=100
                                                     , algorithm = "wcc"
                                                     , type = "a-fixed"))
set.seed(123)
summary_df3 <- as.matrix(average_subsample_size_data(data=df, a = p0
                                                     , xvars = var_names[1:6]
                                                     , rep=100
                                                     , algorithm = "lcc"))

df_sample_sizes <- cbind(summary_df1, summary_df2, summary_df3)
colnames(df_sample_sizes) <- c("cc", "wcc", "lcc")

write.csv(df_sample_sizes, file = paste0(path_output, "data_average_subsamples_all")
          , row.names = TRUE)

rm(summary_df1, summary_df2, summary_df3)

ns_fixed_1 = 16900

test <- lcc_algorithm_fixed_data(data=df, r=p0, a_wcc = p0
                                 , xvars = var_names[1:6]
                                 , ns_fixed = 8450)
test$coef_adjusted
model_lcc$coef_adjusted
model_logit$coefficients # I do not like this approach bcs it makes the LCC coefficients worst...



# I am not sure why this is useful anymore but here are the average subsample sizes to then
# choose the fixed Ns
set.seed(123)
summary_df1 <- as.matrix(average_subsample_size_data(data=df, xvars = var_names[1:6]
                                                    , rep=100
                                                    , algorithm = "cc"
                                                    , type = "a-flexible"
                                                    , a1=1, r = p0))
set.seed(123)
summary_df2 <- as.matrix(average_subsample_size_data(data=df, xvars = var_names[1:6]
                                                     , rep=100
                                                     , algorithm = "wcc"
                                                     , type = "a-flexible"
                                                     , a1=1, r = p0))
set.seed(123)
summary_df3 <- as.matrix(average_subsample_size_data(data=df, a = p0
                                                     , xvars = var_names[1:6]
                                                     , rep=100
                                                     , algorithm = "lcc"))
                         

df_sample_sizes <- cbind(summary_df1, summary_df2, summary_df3)
colnames(df_sample_sizes) <- c("cc", "wcc", "lcc")


write.csv(df_sample_sizes, file = paste0(path_output, "data_average_subsamples_all_2")
          , row.names = TRUE)

rm(summary_df1, summary_df2, summary_df3)



# ***Single Models, fixed Ns***

# Logistic regression
set.seed(123)
model_logit <- glm(as.formula(paste("y ~ ", paste(var_names[1:6], collapse= "+"))),
                   data = df, family = binomial)
model_logit$coefficients

# CC
set.seed(123)
model_cc <- cc_algorithm_fixed_data_2(data=df, a1=1, r = p0, xvars = var_names[1:6]
                                      , ratio_to = 1/2)
model_cc$coef_adjusted
df_subsample_cc <- model_cc$subsample_cc
nrow(df_subsample_cc)
table(df_subsample_cc$y)

#WCC
set.seed(123)
model_wcc <- wcc_algorithm_fixed_data_2(data=df, a1=1, r = p0, xvars = var_names[1:6]
                                       , ratio_to = 1/2)
model_wcc$coef_unadjusted
df_subsample_wcc <- model_wcc$subsample_wcc
nrow(df_subsample_wcc)
table(df_subsample_wcc$y)

# LCC
# pilot uses a 50-50 split

set.seed(123)
model_lcc <- lcc_algorithm_fixed_data(data=df, r = p0, a_wcc=p0
                                      , xvars = var_names[1:6]
                                      , ns_fixed = 12000)

model_lcc$coef_adjusted
df_subsample_lcc <- model_lcc$subsample_lcc_fixed
nrow(df_subsample_lcc)
table(df_subsample_lcc$y)
df_pilot <- model_lcc$subsample_pilot
table(df_pilot$y) # the pilot uses a 50-50 split
mean(model_lcc$a_bar_lcc) #0.2658143


# ***Bootstrapped data sets***

df_class_1 <- df[df$y==1, ]
df_class_0 <- df[df$y==0, ] 

n_samples <- 1000

set.seed(123)
bootstrap_samples <- bootstrap_strat(class_1 = df_class_1, class_0 = df_class_0
                                     , n_samples = n_samples)

saveRDS(bootstrap_samples, file = paste0(path_output, "bootstrap_samples"), compress = FALSE)


test <-bootstrap_samples[[1]]
test2 <-bootstrap_samples[[2]]
test3 <-bootstrap_samples[[56]]
nrow(test)
nrow(test2)

table(df$y)
table(test$y)
table(test2$y)
table(test3$y)

setequal(summary(df),summary(test)) # it should be false
setequal(summary(test),summary(test2)) # it should be false


coefficients_df <- data.frame(matrix(ncol = 4 * (length(var_names) - 1)
                                     , nrow = n_samples))

colnames(coefficients_df) <- c(paste0(var_names[1:6], "_logit"), 
                               paste0(var_names[1:6], "_cc"), 
                               paste0(var_names[1:6], "_wcc"), 
                               paste0(var_names[1:6], "_lcc"))
names(coefficients_df)

ns_fixed_lcc <- 12000

for (i in 1:n_samples) {
  # Get the current bootstrap sample
  current_sample <- bootstrap_samples[[i]]
  
  # Fit the logistic regression model using the full sample
  model_logit <- glm(as.formula(paste("y ~ ", paste(var_names[1:6], collapse= "+"))),
                     data = current_sample, family = binomial)
  
  # Fit the CC model
  set.seed(123)
  model_cc <- cc_algorithm_fixed_data_2(data=current_sample, a1=1, r = p0, xvars = var_names[1:6]
                                        , ratio_to = 1/2)
  
  # Fit the WCC model
  set.seed(123)
  model_wcc <- wcc_algorithm_fixed_data_2(data=current_sample, a1=1, r = p0, xvars = var_names[1:6]
                                          , ratio_to = 1/2)
  
  # Fit the LCC model
  set.seed(123)
  model_lcc <- lcc_algorithm_fixed_data(data=current_sample, r = p0, a_wcc=p0
                                        , xvars = var_names[1:6]
                                        , ns_fixed = ns_fixed_lcc)
  
  # Store the coefficients in the dataframe
  coefficients_df[i, ] <- c(model_logit$coefficients
                            , model_cc$coef_adjusted
                            , model_wcc$coef_unadjusted
                            , model_lcc$coef_adjusted)
}

write.csv(coefficients_df, file = paste0(path_output, "estimates_algorithms")
          , row.names = TRUE)

# I want to know how much they differ from the logistic regression

# means <- apply(coefficients_df, 2, mean)
# 
# logit_benchmark <- means[1:6]
# 
# aprox_squared_bias <- (means - logit_benchmark)^2
# 
# bias_logit <- sum(aprox_squared_bias[1:6])
# bias_logit
# bias_cc <- sum(aprox_squared_bias[7:12])
# bias_cc
# bias_wcc <- sum(aprox_squared_bias[13:18])
# bias_wcc
# bias_lcc <- sum(aprox_squared_bias[19:24])
# bias_lcc

# ** Variances *
variances <- apply(coefficients_df, 2, var)

var_logit <- sum(variances[1:6])
var_logit
var_cc <- sum(variances[7:12])
var_cc
var_wcc <- sum(variances[13:18])
var_wcc
var_lcc <- sum(variances[19:24])
var_lcc # approximately twice the variance of logistic regression, coincidencia?




# ***uniformly subsampled data sets***


# For knowing the average size of the LCC subsample, I could (in theory) take just 1 
# subsample and run the subsampling algorithm 100 times, take the average. I think the lcc
#size should not change that much between subsamples.


m = c(0.8, 0.7, 0.6)

n_samples <- 100
y_strat <- "y"

subsamples_list_1 <- vector(mode = "list", length = n_samples)
subsamples_list_2 <- vector(mode = "list", length = n_samples)
subsamples_list_3 <- vector(mode = "list", length = n_samples)


set.seed(123)

for (j in 1:length(m)) {
  
  b <- round(nrow(df) * m[j])
  
  for (i in 1:n_samples) {
    if (j == 1) {
      subsamples_list_1[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j == 2) {
      subsamples_list_2[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==3) {
      subsamples_list_3[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    }
    
  }
}


nrow(subsamples_list_1[[1]])
table(subsamples_list_1[[1]]$y)/nrow(subsamples_list_1[[1]])

nrow(subsamples_list_2[[1]])
table(subsamples_list_2[[1]]$y)/nrow(subsamples_list_2[[1]])


nrow(subsamples_list_3[[1]])
table(subsamples_list_3[[1]]$y)/nrow(subsamples_list_3[[1]])

setequal(summary(subsamples_list_1[[1]]), summary(subsamples_list_1[[2]]))

setequal(summary(subsamples_list_1[[1]]), summary(subsamples_list_1[[99]]))

setequal(summary(subsamples_list_1[[1]]), summary(subsamples_list_2[[1]]))


