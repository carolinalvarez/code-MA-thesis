# Data application

rm(list = ls())
setwd("~/Documents/Master/thesis/02-Thesis/code/code-MA-thesis/data/")


# Read the .data file using read.table()
my_data <- read.table("breast-cancer/filename.data", header = TRUE, sep = ",")
