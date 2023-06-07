# Finding Needles in Haystacks: A Comparative Study of Algorithms for Handling Imbalanced Data in Logistic Regression Classification

This repository contains the code and data used in my master thesis titled "Finding Needles in Haystacks: A Comparative Study of Algorithms for Handling Imbalanced Data in Logistic Regression Classification". The thesis was submitted in June 2023 to the University of Bonn as a requirement for the degree of M.Sc. Economics.

## Usage

This project consists of several R scripts that work together to produce the analysis results. Here is how you can run them:

### Preliminary Steps
1. Ensure that you have R installed on your system. If you don't have R installed, you can download and install it from The Comprehensive R Archive Network (CRAN).

2. Clone this repository to your local machine.

### Running the Scripts
1. Open the `functions.R` file and run it first. This file contains all the necessary functions for the analysis: DGP, CC, WCC and LCC algorithms (with options to fix the subsample size), and some functions to run the simulation study, among others.

2. Once the functions in `functions.R` are loaded in the R environment, the other R scripts in the repository can be run. These scripts produce the results and figures shown in the master's thesis.

Each script can be run independently. 

Note: The paths to data and to the output files are relative to the project root directory. Make sure you set your working directory to the project root before running the scripts.


## Data

The data used for the data application section was the Income dataset from the UCI Machine Learning Repository. It contains demographic information from the 1994 USA Census database, which includes age, workclass, education level, marital status, occupation, capitan gain, among others. The target variable is income level, a binary column that indicates whether a person earns more than 50K per year or not. The data shows a mild-to-high imbalance between classes.

The data can be downloaded directly from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Adult) or found in the `data` folder of this repository. However, it is advised that the user uses the R script `data-application.R`, which contains code that downloads the data directly to the local environment, combines the test and train data found in the UCI repository, and creates the final dataset that was used for the analysis.


