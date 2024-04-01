
### Introduction Things ###

# R Language website
# https://www.r-project.org/

# R Studio website
# https://posit.co/download/rstudio-desktop/

# Github
# https://github.com/krstout/R-Tutorial

# Walkthrough R Studio interface
# Console pane, environment pane, image pane, script, start new script

# Add ggplot example to end
# Where to get help for R


# "#" to comment in the code, R will not run lines with #

### Import Data ###

data <- read.csv("Erie_County_Election_Data.csv")
# Use full filepath to tell R where the data file is located
# For example: data <- read.csv("C:\\Users\\kevin\\Dropbox\\Data Projects\\R Tutorial\\Erie_County_Election_Data.csv")
# Need to add an extra "\" on Windows PC but not Macs
# "data" should appear in the "Environment" panel on the right
# Click on spreadsheet icon to bring up the data
# Click on arrow to show list of variables


### Overview of the data

summary(data)
# "summary" is a function called on the object data
# A function does some sort of action on an object

names(data)
# "names" gives a list of the variable names for the data


### Working with variables

table(data$town)
# "table" creates a table of specified variable "town"
# Use the "$" between name of the data and the variable

mean(data$total.pop)
# Get the average for total.pop

median(data$p.white)
# Median for percent white

min(data$p.colgrad)
# Minimum value in the data for percent college graduate

max(data$p.poverty)
# Maximum value in the data for percent in poverty

mean(data$pres.score.g16)
# Gives NA - which means there is missing data

summary(data$pres.score.g16)
# Summary shows 4 NA values in pres.score.g16

mean(data$pres.score.g16, na.rm = T)
# Use the argument "na.rm = T" to remove NA values

hist(data$pres.score.g16)
# Make a histogram of the pres.score.g16 variable

hist(data$pres.score.g20)
# Histogram for pres.score.g20 variable


### Creating New Objects and Variables

population.total <- sum(data$total.pop)
# "<-" is called "gets" and is used to create new objects

average.pres16 <- mean(data$pres.score.g16, na.rm = T)


# Create new variables

data$p.nonwhite <- 100 - data$p.white
# Create percent nonwhite variable 

summary(data$p.nonwhite)

data$p.graddeg <- (data$graddeg/data$edupop)*100
# Create percent with graduate degrees

summary(data$p.graddeg)

data$pres.shift <- data$pres.score.g20 - data$pres.score.g16
# Difference between 2020 and 2016 presidential vote share

summary(data$pres.shift)
# Click on spreadsheet and sort on shift to see which precinct was -50

summary(data$p.colgrad)
data$high.college <- ifelse(data$p.colgrad > 32.8, 1, 0)
# Use ifelse function to assign 1 to precincts where p.colgrad exceeds 32.8% and 0 to all others
table(data$high.college)

data$high.grad <- ifelse(data$p.graddeg > mean(data$p.graddeg), 1, 0)
# Use ifelse and mean(data$p.graddeg) to create new binary variable high.grad
table(data$high.grad)


# Create categorical variables

data$wealth[data$medinc >= 100000] <- "rich"
# New variable comes first
# Use square brackets and ">=" greater than or equal to sign to create category "rich"
# Note that rich is in quotes

data$wealth[data$medinc < 50000] <- "low"
# Square brackets and "<" less than sign

data$wealth[data$medinc >= 50000 & data$medinc < 100000] <- "middle"
# Middle income category uses two arguments separated by "&"

table(data$wealth)
table(data$wealth, data$high.college)


### Subset data

# Subset data - selecting only certain observations among the data

delaware <- data[data$town == "Buffalo Delaware",]
# Select only the observations from "Buffalo Delaware" in the "town" variable
# Note the comma before the closing square bracket
# Buff State is in precinct Del 21

mean(delaware$pres.score.g20)

highincome <- data[data$medinc > mean(data$medinc, na.rm = T),]
# Choose only the high income precincts

lowincome <- data[data$wealth == "low",]
highincome <- data[data$wealth == "rich",]
# Create low income and high income datasets

lowincome <- lowincome[!is.na(lowincome$precinct),]
highincome <- highincome[!is.na(highincome$precinct),]
# !is.na(lowincome$precinct) will remove observations where "precinct" is missing


### Correlations and Scatter Plots

cor(data$p.colgrad, data$medinc)
# Gives and error because data$medinc has missing values

?cor
# ? becore a command brings up the help page in the lower right panel

cor(data$p.colgrad, data$medinc, use = "complete.obs")
# Add the use = "complete.obs" argument to filter out NA and get answer

cor.test(data$p.colgrad, data$medinc)
# Read the output in the console to see the positive correlation is statistically significant

plot(data$p.colgrad, data$medinc)
# Get a simple scatter plot between p.colgrad on the x-axis and medinc on the y-axis

# Make some plots of variables and check out correlations
plot(data$p.white, data$pres.score.g16)
plot(data$p.black, data$pres.score.g16)
plot(data$p.hisp, data$pres.score.g16)
plot(data$p.colgrad, data$pres.score.g16)

plot(data$p.white, data$pres.score.g20)
plot(data$p.black, data$pres.score.g20)
plot(data$p.hisp, data$pres.score.g20)
plot(data$p.colgrad, data$pres.score.g20)

cor.test(data$p.hisp, data$pres.score.g16)
cor.test(data$p.hisp, data$pres.score.g20)
cor.test(data$p.colgrad, data$pres.score.g16)
cor.test(data$p.colgrad, data$pres.score.g20)

mean(lowincome$pres.score.g20, na.rm = T)
mean(highincome$pres.score.g20, na.rm = T)
# Find the means for pres.score.g20 for two different datasets

t.test(lowincome$pres.score.g20, highincome$pres.score.g20)
# Use a t.test to test if the difference between means is statistically significant


### Linear Regression Models

model1 <- lm(pres.score.g20 ~ pres.score.g16, data = data)
# lm is the command for linear model
# This is a bivariate model because one variable is regressed on one other variable
# Outcome variable (dependent variable) goes first
# Predictor variable (independent variable) follows the ~ (tilda)
# Remember to include the data = 

summary(model1)
# Use summary to see the model output
# pres.score.g16 has a coefficient of 0.86 and the p-value is <2e-16
# Adjusted r-squared is 0.9584

model2 <- lm(pres.score.g20 ~ p.nonwhite + p.colgrad + p.poverty, data = data)
# This is a multivariate model because there are 3 independent variables predicting pres.score.g20
# Use + to separate independent variables

summary(model2)
# All independent variables are statistically significant

model3 <- lm(pres.score.g20 ~ pres.score.g16 + gov.score.g18 + p.nonwhite + p.colgrad + p.owner + log(medinc), data = data)
# Note the use of log(medinc) to log median income

summary(model3)
# Not all independent variables are significant

nobs(model3)
# Get the number of observations for the model

### Make Nice Graphics with ggplot2

install.packages("ggplot2")
# Can install addons to R called packages
# Most will install from CRAN easily

library(ggplot2)
# Need to use library to tell R that you're loading a package
# Only need to install package once, but need to use library each time package is used in a script

scatterplot <- ggplot(data, aes(x = p.black, y = pres.score.g16)) +
  geom_point()
scatterplot
# Simple scatter plot
# Note the use of the "+" at the end of the line

scatterplot <- ggplot(data, aes(x = p.black, y = pres.score.g16)) + # adds data to x and y
  geom_point() +  # adds points to plot
  scale_x_continuous(labels = function(x) paste0(x, "%")) + # adds "%" to tick marks on axis
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Relationship Between % Black and % Democratic Presidential Vote",  # Change titles, axis labels
       x = "% Black", y = "Democratic Pres Vote") +
  theme_bw() + # this portion tweaks the looks of the grid lines
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank())
scatterplot
# This plot has many more changes to make it look better
# Can use the "Export" button in the plot pane to save the plot


### Resources for help with R

# Google your problem: copy and paste error message or ask how to do something "in r"
# StackOverflow for help with errors and questions
# Quick-R: https://www.statmethods.net/r-tutorial/index.html
# Cookbook for R: http://www.cookbook-r.com/
# R for Data Science (bit more advanced because they use tidyverse)