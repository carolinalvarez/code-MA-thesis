# Analisis of results simulations
library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)

rm(list = ls())
path_output <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/"
pxl <- 300

# Load file

path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_f.csv"
# Load csv file with results
res <- read.csv(path)
#copying hyperparameters
k <- 30
r <- 0.9
a <- 0.9

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))
beta_true

beta_true <- rep(beta_true, 3)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):length(squared_bias)])
squared_bias_lcc

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):length(variances)])
var_lcc



################################ TABLES #########################################

################################ PLOTS #########################################

#Load file

sim <- read.csv("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_e.csv")
colnames(sim)
res <- sim
colnames(res)

# Data Prep

k=30
cc <- sim[1:as.numeric(k+1)]
colnames(cc)

wcc <- sim[as.numeric(k+2):as.numeric(k+k+2)]
colnames(wcc)

lcc <- sim[as.numeric(k+k+3):length(res)]
colnames(lcc)


##########  Estimates

cc_long <- cc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "cc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

wcc_long <- wcc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "wcc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

lcc_long <- lcc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


cc0 <- cc[, 1]
cc1 <- cc[, 2:16]
cc2 <- cc[, 17:31]

wcc0 <- wcc[, 1]
wcc1 <- wcc[, 2:16]
wcc2 <- wcc[, 17:31]

lcc0 <- lcc[, 1]
lcc1 <- lcc[, 2:16]
lcc2 <- lcc[, 17:31]

#intercept
cc_long0 <- as.data.frame(cbind(cc0, rep("cc", length(cc0)))) %>%
  rename(algorithm = V2,
         value=cc0)

wcc_long0 <- as.data.frame(cbind(wcc0, rep("wcc", length(wcc0)))) %>%
  rename(algorithm = V2,
         value=wcc0)

lcc_long0 <- as.data.frame(cbind(lcc0, rep("lcc", length(lcc0)))) %>%
  rename(algorithm = V2,
         value=lcc0)

df0 <- as.data.frame(rbind(cc_long0, wcc_long0, lcc_long0))

df0$value <- as.numeric(df0$value)

# Plot intercept
ggplot(df0, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_boxplot() +
  geom_hline(yintercept = -9.697225, linetype="dashed") + #true value of intercept taken with true.intercept function
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  theme_classic() +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "boxplot_intercept_e.png", sep = "")
       , dpi = pxl)


violin_intercept <- ggplot(df0, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill= "white") + theme_classic() +
  geom_hline(yintercept = -9.697225, linetype="dashed") +
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "violin_intercept_e.png", sep = "")
       , dpi = pxl)

#betas1

cc_long1 <- cc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "cc") %>% 
  select(-regressor)

wcc_long1 <- wcc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "wcc") %>% 
  select(-regressor)

lcc_long1 <- lcc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc") %>% 
  select(-regressor)

df1 <- as.data.frame(rbind(cc_long1, wcc_long1, lcc_long1))

ggplot(df1, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype="dashed") + #true value of intercept taken with true.intercept function
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  theme_classic() +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "boxplot_1_e.png", sep = "")
       , dpi = pxl)

violin_1 <- ggplot(df1, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill= "white") + theme_classic() +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "violin_1_e.png", sep = "")
       , dpi = pxl)



#betas0

cc_long2 <- cc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "cc") %>% 
  select(-regressor)

wcc_long2 <- wcc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "wcc") %>% 
  select(-regressor)

lcc_long2 <- lcc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc") %>% 
  select(-regressor)

df2 <- as.data.frame(rbind(cc_long2, wcc_long2, lcc_long2))

ggplot(df2, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype="dashed") + #true value of intercept taken with true.intercept function
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  theme_classic() +
  labs(x="Algorithm", y = "Estimates")

ggsave(paste(path_output, "violin_2_e.png", sep = "")
       , dpi = pxl)

violin_2 <- ggplot(df2, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill= "white") + theme_classic() +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "violin_2_e.png", sep = "")
       , dpi = pxl)


#combined graphs

violin_intercept + violin_1 + violin_2 + plot_layout(guides = 'collect') 

patchwork  & 
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(), text=element_text(size=7)) & 
  ylim(0,100)

##########   variance

# Compute variances for each estimate and algorithm
var_cc <- apply(cc, 2, var)
var_wcc <- apply(wcc, 2, var)
var_lcc <- apply(lcc, 2, var)

# Create a new data frame with the variances for each regressor and algorithm
variances <- data.frame(algorithm = rep(c("cc", "wcc", "lcc"), each = ncol(cc)),
                        regressor = rep(colnames(cc), 3),
                        variance = c(var_cc, var_wcc, var_lcc))

# extract the numeric part of the regressor names using regular expressions
variances <- variances %>%
  mutate(regressor_num = as.numeric(gsub(".*_(\\d+)$", "\\1", regressor))) %>%
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))

# order the data frame by regressor and algorithm
variances <- variances[order(variances$regressor_num, variances$algorithm), ]

# remove the temporary column
variances <- variances %>% 
  select(-regressor_num) 


ggplot(variances, aes(x = algorithm, y = variance)) +
  geom_boxplot() +
  xlab("Algorithm") +
  ylab("Variance") +
  ggtitle("Boxplot of Variances by Algorithm") +
  theme_minimal()


variances %>%
  group_by(algorithm) %>%
  summarize(avg_variance = mean(variance)) %>%
  ggplot(aes(x = algorithm, y = avg_variance)) +
  geom_bar(stat = "identity") +
  xlab("Algorithm") +
  ylab("Average Variance") +
  ggtitle("Average Variance by Algorithm") +
  theme_minimal()


ggplot(variances, aes(x = regressor, y = variance, color = algorithm)) +
  geom_point() +
  xlab("Regressor") +
  ylab("Variance") +
  ggtitle("Scatter plot of Variance by Regressor and Algorithm") + 
  theme_minimal()


ggplot(variances, aes(x = regressor, y = variance, color = algorithm)) +
  geom_point() +
  xlab("Regressor") +
  ylab("Variance") +
  ggtitle("Scatter plot of Variance by Regressor and Algorithm") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot the data with modified x-axis

summary_stats <- estimates %>%
  group_by(regressor, algorithm) %>%
  summarize(mean = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value))




