# Analisis of results simulations
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)

rm(list = ls())
options(scipen = 999)
setwd("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/")
path_output <- "output/"
pxl <- 300

# Load file

#path <- paste0(path_output, "sim_Prob_e")
path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_n.csv"
res <- read.csv(path)
res <- read.csv(path, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
res <- res[, 2:ncol(res)] # sometimes when Linux file, it comes with an additional column "X" which is a duplicated index 
load(path) 

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

########## Violin Plots Estimates in Sim 1  ########## 

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
cc_long0 <- as.data.frame(cbind(cc0, rep("CC", length(cc0)))) %>%
  rename(Algorithm = V2,
         value=cc0)

wcc_long0 <- as.data.frame(cbind(wcc0, rep("WCC", length(wcc0)))) %>%
  rename(Algorithm = V2,
         value=wcc0)

lcc_long0 <- as.data.frame(cbind(lcc0, rep("LCC", length(lcc0)))) %>%
  rename(Algorithm = V2,
         value=lcc0)

df0 <- as.data.frame(rbind(cc_long0, wcc_long0, lcc_long0))

df0$value <- as.numeric(df0$value)

# Plot intercept
ggplot(df0, aes(x = factor(algorithm, levels = c("cc", "wcc", "lcc"))
                , y = value, fill = algorithm)) +            
  geom_boxplot() +
  geom_hline(yintercept = -9.697225, linetype="dotted") + #true value of intercept taken with true.intercept function
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  theme_classic() +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "boxplot_intercept_e.png", sep = "")
       , dpi = pxl)


ggplot(df0, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value, fill = Algorithm)) +            
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill= "white") + theme_classic() +
  geom_hline(yintercept = -9.697225, linetype="dotted") +
  #geom_label(aes(x = 0.6, y = -9.4, label = "-9.7"), 
  #           fill = "white", color = "black", size=2) +
  scale_fill_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                    breaks = c("CC", "WCC", "LCC")) +
  labs(x="", y = "Estimates") +
  theme(legend.position = "none") 

ggsave(paste(path_output, "violin_intercept_e.png", sep = "")
       , dpi = pxl)

plot1 <- ggplot(df0, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value)) +            
  geom_violin(trim = FALSE, fill='gray87') +
  geom_boxplot(aes(color = Algorithm), width=0.1, fill= "white")+ 
  #theme_classic() +
  theme_light() +
  geom_hline(yintercept = -9.697225, linetype="dotted") +
  scale_color_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                     breaks = c("CC", "WCC", "LCC")) +
  labs(x="", y = "Estimates distribution") +
  theme(legend.position = "bottom") 

ggsave(paste(path_output, "violin_intercept_eV2.png", sep = "")
       , dpi = pxl)


ggarrange(plot1, plot2 + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 2)

#betas1

cc_long1 <- cc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(Algorithm = "CC") %>% 
  select(-regressor)

wcc_long1 <- wcc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(Algorithm = "WCC") %>% 
  select(-regressor)

lcc_long1 <- lcc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(Algorithm = "LCC") %>% 
  select(-regressor)

df1 <- as.data.frame(rbind(cc_long1, wcc_long1, lcc_long1))

ggplot(df1, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value, fill = Algorithm)) +            
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype="dashed") + #true value of intercept taken with true.intercept function
  scale_fill_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  theme_classic() +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "boxplot_1_e.png", sep = "")
       , dpi = pxl)

ggplot(df1, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value, fill = Algorithm)) +            
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill= "white") + 
  theme_classic() +
  geom_hline(yintercept = 1, linetype="dotted") +
  scale_fill_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                    breaks = c("CC", "WCC", "LCC")) +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "violin_1_e.png", sep = "")
       , dpi = pxl)

plot2 <- ggplot(df1, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value)) +            
  geom_violin(trim = FALSE, fill='gray87') +
  geom_boxplot(aes(color = Algorithm), width=0.1, fill= "white")+ 
  #theme_classic() +
  theme_light() +
  geom_hline(yintercept = 1, linetype="dotted") +
  scale_color_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                     breaks = c("CC", "WCC", "LCC")) +
  labs(x="", y = "Estimates distribution") +
  theme(legend.position = "bottom") 

#betas0

cc_long2 <- cc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(Algorithm = "CC") %>% 
  select(-regressor)

wcc_long2 <- wcc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(Algorithm = "WCC") %>% 
  select(-regressor)

lcc_long2 <- lcc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(Algorithm = "LCC") %>% 
  select(-regressor)

df2 <- as.data.frame(rbind(cc_long2, wcc_long2, lcc_long2))

ggplot(df2, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value, fill = Algorithm)) +            
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype="dashed") + #true value of intercept taken with true.intercept function
  scale_fill_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                    breaks = c("CC", "WCC", "LCC")) +
  theme_classic() +
  labs(x="Algorithm", y = "Estimates")

ggsave(paste(path_output, "violin_2_e.png", sep = "")
       , dpi = pxl)

ggplot(df2, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                , y = value, fill = Algorithm)) +            
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill= "white") + theme_classic() +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_fill_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                    breaks = c("CC", "WCC", "LCC")) +
  labs(x="Algorithm", y = "Estimates") 

ggsave(paste(path_output, "violin_2_e.png", sep = "")
       , dpi = pxl)

plot3 <- ggplot(df2, aes(x = factor(Algorithm, levels = c("CC", "WCC", "LCC"))
                         , y = value)) +            
  geom_violin(trim = FALSE, fill='gray87') +
  geom_boxplot(aes(color = Algorithm), width=0.1, fill= "white")+ 
  #theme_classic() +
  theme_light() +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_color_manual(values = c("CC"="plum4", "WCC"="#00A08A", "LCC"="#F2AD00"), #911315 B40F20 00A08A F2AD00
                     breaks = c("CC", "WCC", "LCC")) +
  labs(x="", y = "Estimates distribution") +
  theme(legend.position = "bottom") 


#combined graphs
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

ggarrange(plot1, plot2, plot3 + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")

ggsave(paste(path_output, "all_violins.png", sep = "")
       , dpi = pxl)


########## Distribution plots for sim 2  ########## 

path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_n.csv"
sim <- read.csv(path, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

# solo intercept

k <- 30

log <- sim[1:as.numeric(k+1)]
colnames(log)

lcc <- sim[as.numeric(k+4):length(sim)-2]
colnames(lcc)

log_long <- log %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "log") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


lcc_long <- lcc %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


log0 <- log[, 1]
log1 <- log[, 2:16]
log2 <- log[, 17:31]

lcc0 <- lcc[, 1]
lcc1 <- lcc[, 2:16]
lcc2 <- lcc[, 17:31]

#intercept
log_long0 <- as.data.frame(cbind(log0, rep("log", length(log0)))) %>%
  rename(algorithm = V2,
         value=log0)

lcc_long0 <- as.data.frame(cbind(lcc0, rep("lcc", length(lcc0)))) %>%
  rename(algorithm = V2,
         value=lcc0)

# Getting theoretical distribution
# beta true for intercept, see sim2

mean0_estimate <- -9.697225 

# variance of logistic reg 0.00096818824 see sim2

theor_var = 0.00096818824*2
theor_var

set.seed(123)
theor_lcc_0 <- mvrnorm(1000, mu = mean0_estimate, Sigma = theor_var, empirical = TRUE)

theor_long_0 <- as.data.frame(cbind(theor_lcc_0, rep("LCC (Theoretical)", length(theor_lcc_0))))%>%
  rename(algorithm = V2,
         value=V1)

#checks
summary(as.numeric(theor_lcc_0))
var(theor_lcc_0)

summary(as.numeric(log_long0$value))
var(as.numeric(log_long0$value))

#it has exactly twice the variance as the logistic regression distribution

df0 <- rbind(log_long0, lcc_long0, theor_long_0)

# annotation <- data.frame(
#   x = -9.655,
#   y = 13.5,
#   label = paste('alpha', "==", -9.697)
# )

plot4 <- ggplot() + 
  geom_density(aes(x = as.numeric(log_long0$value), color = "Logit"), fill = NA) +
  geom_density(aes(x = as.numeric(lcc_long0$value), color = "LCC"), fill = NA) +
  geom_density(aes(x = as.numeric(theor_lcc_0), color = "LCC (Theoretical)")
               , fill = NA, linetype = "dashed") +
  xlab("Estimate") +
  ylab("Density") +
  geom_vline(xintercept = -9.697225, linetype="dotted", show.legend = TRUE) +
  #geom_label(data=annotation, aes(x=x, y=y, label=label), parse = TRUE, color="black", size=3) +
  scale_color_manual(name = "Algorithm",
                     values = c("Logit"="#3B9AB2", "LCC"="#F2AD00", "LCC (Theoretical)"="grey"), 
                     breaks = c("Logit", "LCC", "LCC (Theoretical)"),
                     guide = guide_legend(override.aes = list(fill = c("#3B9AB2",
                                                                       "#F2AD00",
                                                                       "grey")))) +
  theme_light() +
  theme(legend.position = "bottom") 


ggsave(paste(path_output, "sim2_intercept.png", sep = "")
       , dpi = pxl)


# betas 1

log_long1 <- log1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "Logit") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


lcc_long1 <- lcc1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "LCC") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


#computing the theoretical variance is a bit more complicated, I need to do it for every regressor

variances_log1 <- apply(log1, 2, var) #i check and same as in sim1
variances1_theory <- variances_log1*2

set.seed(123) 

theor_dist_list1 <- lapply(variances1_theory, function(x) mvrnorm(1000, mu = 1, Sigma = x))
df_theor_dist1 <- as.data.frame(theor_dist_list1)
names(df_theor_dist1) <- names(variances_log1)

#checks
apply(df_theor_dist1, 2, mean)
test <- apply(log1, 2, var)
test*2
apply(df_theor_dist1, 2, var)

theory_long1 <- df_theor_dist1 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc_theory") %>% 
  mutate(regressor = sub("_cc_|_lcc_|lcc_theory|log", "", regressor))

df1_long <- rbind(log_long1, lcc_long1, theory_long1)


# plot the distributions
plot5 <- ggplot(df1_long, aes(x = value, color = algorithm, group = regressor)) +
  geom_density(linewidth = 0.2) +
  labs(x = "Estimate", y = "Density", color = "Algorithm") +
  geom_vline(xintercept = 1, linetype="dotted", show.legend = TRUE) +
  scale_color_manual(name = "Algorithm",
                     values = c("Logit"="#3B9AB2", "LCC"="#F2AD00", "lcc_theory"= "grey"),
                     labels = c("Logit", "LCC", "LCC (Theoretical)"),
                     breaks = c("Logit", "LCC", "lcc_theory"),
                     guide = guide_legend(override.aes = list(fill = c("#3B9AB2",
                                                                       "#F2AD00",
                                                                       "grey")))) +
  theme_light() +
  theme(legend.position = "bottom") 


ggsave(paste(path_output, "sim2_betas1.png", sep = "")
       , dpi = pxl)



# betas0

log_long2 <- log2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "Logit") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


lcc_long2 <- lcc2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "LCC") %>% 
  mutate(regressor = sub("_cc_|_wcc_|_lcc_", "", regressor))


#computing the theoretical variance is a bit more complicated, I need to do it for every regressor

variances_log2 <- apply(log2, 2, var) #i check and same as in sim1
variances2_theory <- variances_log2*2

set.seed(123) 

theor_dist_list2 <- lapply(variances2_theory, function(x) mvrnorm(1000, mu = 0, Sigma = x))
df_theor_dist2 <- as.data.frame(theor_dist_list2)
names(df_theor_dist2) <- names(variances_log2)

#checks
apply(df_theor_dist2, 2, mean)
test <- apply(log2, 2, var)
test*2
apply(df_theor_dist2, 2, var)

theory_long2 <- df_theor_dist2 %>% 
  pivot_longer(everything(), names_to = "regressor", values_to = "value") %>% 
  mutate(algorithm = "lcc_theory") %>% 
  mutate(regressor = sub("_cc_|_lcc_|lcc_theory|log", "", regressor))

df2_long <- rbind(log_long2, lcc_long2, theory_long2)


# plot the distributions
plot6 <- ggplot(df2_long, aes(x = value, color = algorithm, group = regressor)) +
  geom_density(linewidth = 0.2) +
  labs(x = "Estimate", y = "Density", color = "Algorithm") +
  geom_vline(xintercept = 0, linetype="dotted", show.legend = TRUE) +
  scale_color_manual(name = "Algorithm",
                     values = c("Logit"="#3B9AB2", "LCC"="#F2AD00", "lcc_theory"= "grey"),
                     labels = c("Logit", "LCC", "LCC (Theoretical)"),
                     breaks = c("Logit", "LCC", "lcc_theory"),
                     guide = guide_legend(override.aes = list(fill = c("#3B9AB2",
                                                                       "#F2AD00",
                                                                       "grey")))) +
  theme_light() +
  theme(legend.position = "bottom") 


ggsave(paste(path_output, "sim2_betas2.png", sep = "")
       , dpi = pxl)


ggarrange(plot4, plot5, plot6 + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")

ggsave(paste(path_output, "all_densities.png", sep = "")
       , dpi = pxl)

######################## variances plots ########################################
path <- "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_Prob_a"
res <- read.csv(path, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
res <- res[, 2:ncol(res)]

k <- 30
N <- 10^5
r <- 0.7
a <- 0.7

means <- data.frame(t(colMeans(res)))
colnames(means) <- gsub("β_hat_", "", colnames(means))


# True coefficient values
beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
               , rep(0, k/2))

beta_true <- rep(beta_true, 4)

# Calculate squared bias
squared_bias <- (means - beta_true)^2

# Add column names to squared_bias
colnames(squared_bias) <- colnames(means)

# Display squared_bias
squared_bias_cc <- sum(squared_bias[1:as.numeric(k+1)])
squared_bias_cc
squared_bias_wcc <- sum(squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
squared_bias_wcc
squared_bias_lcc <- sum(squared_bias[as.numeric(k+k+3):as.numeric(k+k+k+3)])
squared_bias_lcc
squared_bias_logit <- sum(squared_bias[as.numeric(k+k+k+5):length(squared_bias)-1])
squared_bias_logit

mean_a_bar <- mean(res$a_bar_lcc)
mean_a_bar

# Take the variance of the realizations
variances <- apply(res, 2, var)

var_cc <- sum(variances[1:as.numeric(k+1)])
var_cc
var_wcc <- sum(variances[as.numeric(k+2):as.numeric(k+k+2)])
var_wcc
var_lcc <- sum(variances[as.numeric(k+k+3):as.numeric(k+k+k+3)])
var_lcc
var_logit <- sum(variances[as.numeric(k+k+k+5):length(variances)-1])
var_logit

var_cc_ratio <- variances[1:as.numeric(k+1)]/variances[as.numeric(k+k+k+5):length(squared_bias)-1]
var_wcc_ratio <- variances[as.numeric(k+2):as.numeric(k+k+2)]/variances[as.numeric(k+k+k+5):length(variances)-1]
var_lcc_ratio <- variances[as.numeric(k+k+3):as.numeric(k+k+k+3)]/variances[as.numeric(k+k+k+5):length(variances)-1]
var_logit_ratio <- variances[as.numeric(k+k+k+5):length(variances)-1]/variances[as.numeric(k+k+k+5):length(variances)-1]

# create data frame for plotting
df <- data.frame(ratio = c(var_cc_ratio, var_wcc_ratio, var_lcc_ratio, var_logit_ratio), 
                 regressor = rep(1:31, 4), 
                 method = rep(c("CC", "WCC", "LCC", "Logit"), each = 31))

# Plot that shows the variance ratio with respect to the logistic regression variance
ggplot(df, aes(x = regressor, y = ratio, color = method)) +
  geom_point(aes(shape=method)) +
  scale_shape_manual(values=c(3, 1, 18, 8)) +
  geom_hline(yintercept = 2.8, linetype="dashed") + # 2.8 is the approximation to the theoretical ratio I found in sim 2
  xlab("Regressor") +
  ylab("Variance Ratio") +
  theme_classic()



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


# Plotting for different Ns sizes 

# Set the directory containing the csv files
setwd("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/")

# Get the list of csv files in the directory
file_list <- c("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_a.csv"
              , "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_b.csv"
              , "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_c.csv"
              #, "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_d.csv"
              , "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_e.csv"
              , "~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/output/sim_smallk_f.csv")

# Initialize empty data frames to store the bias and variance results
squared_bias_cc <- data.frame()
squared_bias_wcc <- data.frame()
squared_bias_lcc <- data.frame()

var_cc <- data.frame()
var_wcc <- data.frame()
var_lcc <- data.frame()

# Loop over the csv files and calculate the bias and variance
for (file in file_list) {
  
  # Load csv file with results
  res <- read.csv(file)
  
  means <- data.frame(t(colMeans(res)))
  colnames(means) <- gsub("β_hat_", "", colnames(means))
  
  # True coefficient values
  beta_true <- c(get.true.intercept(1-r, rep(0.5, k), c(rep(1,k/2), rep(0, k/2))), rep(1, k/2)
                 , rep(0, k/2))
  beta_true <- rep(beta_true, 3)
  
  # Calculate squared bias
  squared_bias <- (means - beta_true)^2
  
  # Add column names to squared_bias
  colnames(squared_bias) <- colnames(means)
  
  # Append squared_bias results to the data frames
  squared_bias_cc <- rbind(squared_bias_cc, squared_bias[1:as.numeric(k+1)])
  squared_bias_wcc <- rbind(squared_bias_wcc, squared_bias[as.numeric(k+2):as.numeric(k+k+2)])
  squared_bias_lcc <- rbind(squared_bias_lcc, squared_bias[as.numeric(k+k+3):length(squared_bias)])
  
  # Take the variance of the realizations
  variances <- apply(res, 2, var)
  
  # Append variance results to the data frames
  var_cc <- rbind(var_cc, variances[1:as.numeric(k+1)])
  var_wcc <- rbind(var_wcc, variances[as.numeric(k+2):as.numeric(k+k+2)])
  var_lcc <- rbind(var_lcc, variances[as.numeric(k+k+3):length(variances)])
  
  
}

# Sum the rows to get the total squared bias and variance
col_names <- c("bias", "algorithm", "samplesize")

total_squared_bias_cc <- rowSums(squared_bias_cc)
total_squared_bias_wcc <- rowSums(squared_bias_wcc)
total_squared_bias_lcc <- rowSums(squared_bias_lcc)

# bias plot
total_squared_bias_cc <- as.data.frame(cbind(round(total_squared_bias_cc,6), rep("cc", length(total_squared_bias_cc))
                                             , as.numeric(c(18000, 1800, 900, 360, 270))))


total_squared_bias_wcc <- as.data.frame(cbind(round(total_squared_bias_wcc, 6), rep("wcc", length(total_squared_bias_wcc))
                                             , as.numeric(c(18000, 1800, 900, 360, 270))))

total_squared_bias_lcc <- as.data.frame(cbind(round(total_squared_bias_lcc, 6), rep("lcc", length(total_squared_bias_lcc))
                                              , as.numeric(c(9000, 900, 450, 180, 130))))


colnames(total_squared_bias_cc) <- col_names 
colnames(total_squared_bias_wcc) <- col_names 
colnames(total_squared_bias_lcc) <- col_names 

df_total_bias <- rbind(total_squared_bias_cc
                       , total_squared_bias_wcc
                       , total_squared_bias_lcc)

df_total_bias <- df_total_bias %>%
  mutate(samplesize = as.numeric(samplesize)) %>%
  mutate(bias = as.numeric(bias)) %>%
  mutate(sqrt_bias = sqrt(bias + 0.001)) %>%
  mutate(log_bias = log(bias + 0.001/(1-(bias+0.001)))) %>%
  arrange(samplesize) 


# Create a scatter plot with different colors for each algorithm
ggplot(df_total_bias, aes(x = samplesize, y = sqrt_bias, color = algorithm)) + 
  geom_point() + 
  geom_line(aes(group = algorithm)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(x = "Subsample Size", y = "Estimated bias squared") +
  theme_classic() 


ggsave(paste(path_output, "bias_subsamplesizes.png", sep = "")
       , dpi = pxl)


#variance plot

total_var_cc <- rowSums(var_cc)
total_var_wcc <- rowSums(var_wcc)
total_var_lcc <- rowSums(var_lcc)

col_names2 <- c("var", "algorithm", "samplesize")

total_var_cc <- as.data.frame(cbind(total_var_cc, rep("cc", length(total_var_cc))
                                             , as.numeric(c(18000, 1800, 900, 360, 270))))


total_var_wcc <- as.data.frame(cbind(total_var_wcc, rep("wcc", length(total_var_wcc))
                                              , as.numeric(c(18000, 1800, 900, 360, 270))))

total_var_lcc <- as.data.frame(cbind(total_var_lcc, rep("lcc", length(total_var_lcc))
                                              , as.numeric(c(9000, 900, 450, 180, 130))))


colnames(total_var_cc) <- col_names2 
colnames(total_var_wcc) <- col_names2 
colnames(total_var_lcc) <- col_names2 

df_total_var <- rbind(total_var_cc
                       , total_var_wcc
                       , total_var_lcc)

df_total_var <- df_total_var %>%
  mutate(samplesize = as.numeric(samplesize)) %>%
  mutate(var = as.numeric(var)) %>%
  mutate(sqrt_var = sqrt(var + 0.001)) %>%
  arrange(samplesize) 


# Create a scatter plot with different colors for each algorithm
ggplot(df_total_var, aes(x = samplesize, y = sqrt_var, color = algorithm)) + 
  geom_point() + 
  geom_line(aes(group = algorithm)) +
  scale_color_manual(values = c("#F8AFA8", "#FDDDA0", "#74A089")) +
  labs(x = "Subsample Size", y = "Estimated variance") +
  theme_classic() 


ggsave(paste(path_output, "bias_subsamplesizes.png", sep = "")
       , dpi = pxl)


