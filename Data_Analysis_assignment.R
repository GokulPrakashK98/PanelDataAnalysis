# setting working directory
setwd("F:/Data_Analysis_with_R_in_Agricultural_Economics")

# ---- Installing required packages ----
install.packages("openxlsx")
install.packages("zoo")
install.packages("lmtest")
install.packages("car")
install.packages("tidyverse")
install.packages("plm")
tinytex::install_tinytex()

# Load the packages
library(plm)
library(lmtest)
library(tidyverse)
library(openxlsx)
library(dplyr)


# Reading the data set
china_data <- read.xlsx("china8691p89.xlsx")
china_data <- china_data %>%
  rename(IntermInputs = IntermediateInputs)


# Exploring the data set
anyNA(china_data)
str(china_data)
head(china_data)
summary(china_data)

# --- plots -----
# Plot a density plot to visualize the distribution
par(mfrow = c(2, 3))  # Set up a 3x2 grid for subplots
plot(density(china_data$Output), col = "blue", 
     main = "Density Plot: Output", 
     xlab = "Values", 
     ylab = "Density")
plot(density(china_data$Labour), col = "blue", 
     main = "Density Plot: Labour", 
     xlab = "Values", 
     ylab = "Density")
plot(density(china_data$Capital), col = "blue", 
     main = "Density Plot: Capital", 
     xlab = "Values", 
     ylab = "Density")
plot(density(china_data$Land), col = "blue", 
     main = "Density Plot: Land", 
     xlab = "Values", 
     ylab = "Density")
plot(density(china_data$IntermInputs), col = "blue", 
     main = "Density Plot: Interm. inputs", 
     xlab = "Values", 
     ylab = "Density")
plot(density(china_data$Trend), col = "blue", 
     main = "Density Plot: Trend", 
     xlab = "Values", 
     ylab = "Density")


# Scatter plot
par(mfrow = c(2, 3))  # Set up a 3x2 grid for subplots
# Plot independent variables against Output
plot(china_data$Capital, china_data$Output, 
     xlab = "Capital", 
     ylab = "Output", 
     main = "Capital vs Output")
plot(china_data$Labour, china_data$Output, 
     xlab = "Labour",
     ylab = "Output", 
     main = "Labour vs Output")
plot(china_data$Land, china_data$Output, 
     xlab = "Land", 
     ylab = "Output", 
     main = "Land vs Output")
plot(china_data$IntermInputs, china_data$Output, 
     xlab = "Intermediate Inputs", 
     ylab = "Output", 
     main = "Interm. Inputs vs Output")
plot(china_data$Trend, china_data$Output, 
     xlab = "Trend", 
     ylab = "Output", 
     main = "Trend vs Output")


# Box plot
par(mfrow = c(2, 3))  # Set up a 3x2 grid for subplots
boxplot(china_data$Capital, 
        main = "Box plot of Capital")
boxplot(china_data$Labour, 
        main = "Box plot of Labour")
boxplot(china_data$Land, 
        main = "Box plot of Land")
boxplot(china_data$Output, 
        main = "Box plot of Output")
boxplot(china_data$IntermInputs, 
        main = "Box plot of Interm. Inputs")
boxplot(china_data$Trend, 
        main = "Box plot of Trend")

# ---- Compute the correlation matrix ----
correlation_matrix <- cor(china_data)
print(correlation_matrix)


# Cobb-Douglas production function
# ---- Panel Data estimation ----
formula_china <- Output ~ Land + Labour + Capital + IntermediateInputs + Trend
lm_china <- lm(formula_china, data = china_data)
summary(lm_china)


# Least squared dummy variable using "lm" 
china_data$Index <- as.factor(china_data$Index)
formula_LSDV <- Output ~ Land + Labour + Capital + IntermediateInputs + Trend + Index
lm_LSDV <- lm(formula_LSDV, data = china_data)
summary(lm_LSDV)

lrtest(lm_china, lm_LSDV) # lm_LSDV is better choice
waldtest(lm_china, lm_LSDV)

# Cobb-Douglas production function using "plm" package:
# ----- Pooled regression model ------
Pool_china_model <- plm(formula_china, data = china_data, model = "pooling")
summary(Pool_china_model)

# ----- Fixed effects model -----
## Including farm fixed effects by adding 'index' as a factor
# Within estimation using the "plm" package 
formula_china <- Output ~ Land + Labour + Capital + IntermediateInputs + Trend
fixed_china_model <- plm(formula_china, data = china_data, model = "within")
summary(fixed_china_model)
fixef(fixed_china_model) # Display the fixed effects

# ----- Testing model specification (fixed effects) -----
# Ho: OLS better than fixed effects
pooltest(Pool_china_model, lm_LSDV) # p-value < 0.05 --> fixed effects are recommended.
pooltest(Pool_china_model, fixed_china_model) # p-value < 0.05 --> fixed effects are recommended.
# The fixed effects model is better than OLS.

# ------ Random effects model ------
random_china_model <- plm(formula_china, data = china_data, model = "random")
summary(random_china_model)
ranef(random_china_model) # Display the random effects

# ------ "Hausman test" ------
phtest(fixed_china_model, random_china_model) 
# p-value is < 0.05, then we prefer the fixed effects model 

# robust "Hausman test"
phtest(fixed_china_model, random_china_model, vcov = vcovHC) # similar results

# ------ Test for Panel data models ------
# Test for homoskedasticity - "Breusch-Pagan test"
bptest(fixed_china_model)  # p-value< 0.05, then, there is evidence of heteroscedasticity

# # Test for serial autocorrelation. 
# Ho: there is not serial correlation.
pbgtest(fixed_china_model)  # p-value< 0.05, then, there is serial correlation in idiosyncratic errors
pwartest(fixed_china_model) # same result 

# ----- Correcting the panel model estimation for Heteroskedasticity ------ 
coeftest(fixed_china_model, vcovHC(fixed_china_model, type = "HC3"))

# Heteroskedasticity consistent coefficients ("Arellano")
coeftest(fixed_china_model, vcov. = vcovHC(fixed_china_model, method = "arellano"))
coeftest(fixed_china_model, vcov. = vcovHC(fixed_china_model, method = "arellano", type = "HC4"))

# ---- Testing for time-fixed effects ----
# Cobb-Douglas without trend  
formula_china_time <- Output ~ Land + Labour + Capital + IntermediateInputs

formula_china_time_tw <- plm(formula_china_time, data = china_data, model = "within", effect = "twoways" )
summary(formula_china_time_tw)

# Testing time-fixed effects: Ho: no time-fixed effects is needed.
phtest(formula_china_time_tw, fixed_china_model) #  p-value > 0.05, then time fixed effects are not needed.

# Getting time and individual effect 
fixef(formula_china_time_tw,effect = "time")
fixef(formula_china_time_tw,effect = "individual")
