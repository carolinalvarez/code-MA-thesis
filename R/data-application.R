# DATA APPLICATION
library(ggplot2)
library(GGally)
library(corrplot)
library(ROSE)
library(stargazer)
library(xtable)


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

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/old.adult.names"
v_names <- read.csv(url, header = FALSE)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
df_1 <- read.csv(url, header = FALSE)


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
df_2 <- read.csv(url, header = FALSE)

tmp01 <- rbind(df_1, df_2)
names(tmp01)

#save data set
save(tmp01, file = "income-dataset/data_raw.RData")

tmp01$y <- ifelse(tmp01$V15 == " >50K" | tmp01$V15 == " >50K.", 1, 0)
table(tmp01$y)/nrow(tmp01)


# Replace '?' with NA
tmp01[tmp01 == "?" | tmp01 == " ?"] <- NA
tmp02 <- na.omit(tmp01) # observations match with info in old.adult.names
table(tmp02$y)/nrow(tmp02)

# From v_names file:
# V1 - age
# V3 - fnlwgt
# V5 - educ num
# V12 -  capital loss
# V13 - hours worked

cont_vars <- c("V1", "V3", "V5", "V12", "V13", "y")


df <- tmp02[, cont_vars]
str(df)

for (column in cont_vars) {
  df[[column]] <- as.numeric(df[[column]])
}

str(df)

var_names <- c("age", "fnlwgt", "education_num", "capital_loss", "hours_per_week", "y")
colnames(df) <- var_names

table(df$y)/nrow(df)

p0 <- as.numeric(as.vector(table(df$y)/nrow(df))[1])
p1 <- as.numeric(as.vector(table(df$y)/nrow(df))[2])

# *** Data Exploration ***

# Check for duplicates in general
total_duplicates <- duplicated(df)
num_total_duplicates <- sum(total_duplicates)

if (num_total_duplicates == 0) {
  cat("No duplicates found in the subsample.\n")
} else {
  cat("There are", num_total_duplicates, "duplicates in the subsample.\n")
}


# check for duplicates in each strata
split_data <- split(df, df$y)

class_duplicates <- lapply(split_data, function(stratum) sum(duplicated(stratum)))

if (all(sapply(class_duplicates, function(x) x == 0))) {
  cat("No duplicates found within each stratum in the original dataset.\n")
} else {
  cat("There are duplicates within strata in the original dataset:\n")
  print(class_duplicates)
}

# NOTE: There are 376 duplicates in the data. This might become a problem when
# performing the m-bootstrap (subsample) of the data, as it will show as if the
# subsample was done with replacement. However, it is possible that the nature of
# the data allows for strata duplication, for which I will not remove the 
# duplicates from the original sample.


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


# test <- read.csv(paste0(path_output, "data_averages_subsample_LCC_2"))
# test2 <- read.csv(paste0(path_output, "final_results2.csv"))
# 
# test3 <- cbind(m, test2[, 6:length(test2)])


m = c(0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5, 0.45, 0.4)

#m <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)

n_samples <- 100
y_strat <- "y"


# here I create an empty list for each sumbsampling level in m
for (i in 1:length(m)) { # j is the subsample
  
  tmp <-paste0("subsamples_list_", i)
  assign(tmp, vector(mode = "list", length = n_samples))
}

#subsampling the data for each value in m
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
    } else if (j==4) {
      subsamples_list_4[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==5) {
      subsamples_list_5[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==6) {
      subsamples_list_6[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==7) {
      subsamples_list_7[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==8) {
      subsamples_list_8[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==9) {
      subsamples_list_9[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==10) {
      subsamples_list_10[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==11) {
      subsamples_list_11[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    } else if (j==12) {
      subsamples_list_12[[i]] <- stratified_subsample(df, df$y, p0=p0, p1=p1, b=b)
    }
    
  }
}


# Check for duplicates
duplicates <- duplicated(subsamples_list_1[[1]])

# Sum the logical vector
num_duplicates <- sum(duplicates)

if (num_duplicates == 0) {
  cat("No duplicates found in the subsample.\n")
} else {
  cat("There are", num_duplicates, "duplicates in the subsample.\n")
} #duplicates are found because original data presents duplicates



# calculating the average lcc size for each subsampling level m
summary_dfs_list <- list()


algorithms <- c("cc", "wcc", "lcc")


for (s in 1:length(m)) {
  
  tmp01 <- paste0("subsamples_list_",s)
  
  for (a in 1:length(algorithms)) {
    
    set.seed(123)
    
    for (i in 1:100) {
      
      list_to_use <- get(tmp01)
      
      if (a == 1){
        tmp <- average_subsample_size_data(data = list_to_use[[i]],
                                           xvars = var_names[1:5],
                                           rep = 1, type = "a-flexible"
                                           , algorithm = algorithms[a], a1=1, r=p0)
        
      }else if (a==2){
        tmp <- average_subsample_size_data(data = list_to_use[[i]],
                                           xvars = var_names[1:5],
                                           rep = 1, type = "a-flexible"
                                           , algorithm = algorithms[a], a1=1, r=p0)
      }else if (a==3){
        tmp <- average_subsample_size_data(data = list_to_use[[i]],
                                           a = p0,
                                           xvars = var_names[1:5],
                                           rep = 1
                                           , algorithm = algorithms[a])
      }
      
      
      summary_df <- paste0("summary_df_", s, "_", a)
      summary_dfs_list[[summary_df]] <- rbind(summary_dfs_list[[summary_df]], tmp)
      
      colnames(summary_dfs_list[[summary_df]]) <- names(tmp)
      
    }
    
  }
  
  
}


res <- c()

res_index <- 1

for (i in seq(from = 3, to = 36, by = 3)) {
  
  res[res_index] <- as.numeric(round(mean(as.data.frame(summary_dfs_list[[i]])$Mean)))
  
  res_index <- res_index + 1
  
}

res

res2 <- as.data.frame(cbind(m, res))

write.csv(res2, file = paste0(path_output, "data_averages_subsample_LCC")
          , row.names = TRUE)



subsamples_list_all <- list(subsamples_list_1, 
                            subsamples_list_2, 
                            subsamples_list_3, 
                            subsamples_list_4, 
                            subsamples_list_5, 
                            subsamples_list_6,
                            subsamples_list_7,
                            subsamples_list_8,
                            subsamples_list_9,
                            subsamples_list_10,
                            subsamples_list_11,
                            subsamples_list_12)


for (list_index in 1:length(subsamples_list_all)) {
  
  coefficients_df <- data.frame(matrix(ncol = 4 * (length(var_names) )
                                       , nrow = n_samples))
  
  regressor_names <- c("intercept", var_names)
  
  colnames(coefficients_df) <- c(paste0(regressor_names[1:6], "_logit"), 
                                 paste0(regressor_names[1:6], "_cc"), 
                                 paste0(regressor_names[1:6], "_wcc"), 
                                 paste0(regressor_names[1:6], "_lcc"))
  names(coefficients_df)
  
  ns_fixed_lcc <- res[list_index]
  
  current_subsamples_list <- subsamples_list_all[[list_index]]
  
  for (i in 1:n_samples) {
    
    current_sample <- current_subsamples_list[[i]]
    
    # Fit the logistic regression model using the full sample
    set.seed(123)
    model_logit <- glm(as.formula(paste("y ~ ", paste(var_names[1:5] , collapse= "+")))
                       , data = current_sample
                       , family = binomial)
    
    # Fit the CC model
    set.seed(123)
    model_cc <- cc_algorithm_fixed_data_2(data=current_sample, a1=1, r = p0
                                          , xvars = var_names[1:5]
                                          , ratio_to = 1/2)
    
    # Fit the WCC model
    set.seed(123)
    model_wcc <- wcc_algorithm_fixed_data_2(data=current_sample, a1=1, r = p0
                                            , xvars = var_names[1:5]
                                            , ratio_to = 1/2)
    
    # Fit the LCC model
    set.seed(123)
    model_lcc <- lcc_algorithm_fixed_data(data=current_sample, r = p0, a_wcc=p0
                                          , xvars = var_names[1:5]
                                          , ns_fixed = ns_fixed_lcc)
    
    # Store the coefficients in the dataframe
    coefficients_df[i, ] <- c(model_logit$coefficients
                              , model_cc$coef_adjusted
                              , model_wcc$coef_unadjusted
                              , model_lcc$coef_adjusted)
  }
  
  write.csv(coefficients_df, file = paste0(path_output, "estimates_algorithms_m_", list_index),
            row.names = TRUE)
  
  
  
}


#loading back coeff
loaded_coefficients_df_list <- list()

for (list_index in 1:length(subsamples_list_all)) {
  
  loaded_coefficients_df_list[[list_index]] <- read.csv(
    file = paste0(path_output, "estimates_algorithms_m_", list_index),
    row.names = 1
  )
}



results <- data.frame(matrix(ncol = 8, nrow = length(loaded_coefficients_df_list)))

colnames(results) <- c("bias_logit", "bias_cc", "bias_wcc", "bias_lcc",
                       "var_logit", "var_cc", "var_wcc", "var_lcc")

for (k in 1:length(loaded_coefficients_df_list)) {
  coefficients_df <- loaded_coefficients_df_list[[k]]
  
  means <- apply(coefficients_df, 2, mean)
  logit_benchmark <- means[1:6]
  aprox_squared_bias <- (means - logit_benchmark)^2
  
  bias_logit <- sum(aprox_squared_bias[1:6])
  bias_cc <- sum(aprox_squared_bias[7:12])
  bias_wcc <- sum(aprox_squared_bias[13:18])
  bias_lcc <- sum(aprox_squared_bias[19:24])
  
  variances <- apply(coefficients_df, 2, var)
  
  var_logit <- sum(variances[1:6])
  var_cc <- sum(variances[7:12])
  var_wcc <- sum(variances[13:18])
  var_lcc <- sum(variances[19:24])
  
  results[k, ] <- c(bias_logit, bias_cc, bias_wcc, bias_lcc,
                    var_logit, var_cc, var_wcc, var_lcc)
}


print(results)


write.csv(results, file = paste0(path_output, "final_results.csv"), row.names = FALSE)
# to make sense of the results, read again Hastie pg.1714


final <- cbind(m, results[, 6:length(results)])
final


latex_table <- xtable(final, digits = 4, include.rownames = FALSE)

sink(paste0(path_output, "final_results.tex"))
print(latex_table, type = "latex", include.rownames = FALSE)
sink()






