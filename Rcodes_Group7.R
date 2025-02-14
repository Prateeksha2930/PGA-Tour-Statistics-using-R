## Project : Analysis of PGA Tour Statistics

#Installing the required packages
install.packages("moderndive")
install.packages("tidyverse")
install.packages("skimr")
install.packages("gapminder")
install.packages("dplyr")
install.packages("car")
install.packages("ggplot2")


#Load the packages
library(moderndive)
library(tidyverse)
library(skimr)
library(gapminder)
library(car)
library(ggplot2)

rm(list = ls())
#Read the csv file
getwd()
# Set the working directory
setwd("/Users/csuftitan/Downloads")

# Save the data set in the PGATour
PGATour <- read.csv("/Users/csuftitan/Downloads/PGATour.csv")
head(PGATour,10)

#Change the col names
colnames(PGATour)[3] = "Money"
colnames(PGATour)[4] = "Scoring.Average"
colnames(PGATour)[8] = "Sand.Saves"
colnames(PGATour)[11] = "Bounce_Back"

# Step 1 : Exploratory Data Analysis : 
#Look at the raw data
glimpse(PGATour)
# Removing the observations which as NA values in any variables
na.omit(PGATour)
glimpse(PGATour)

#Computing summary statistics
PGATour %>% skim()

# Calculating correlation coefficient for each explanatory variable
PGATour %>% select(Money, DrDist, DrAccu, GIR, Sand.Saves, Bounce_Back, Scrambling, PPR) %>% cor()
PGATour %>% get_correlation(formula = Scoring.Average ~ Money)
PGATour %>% get_correlation(formula = Scoring.Average ~ DrDist)
PGATour %>% get_correlation(formula = Scoring.Average ~ DrAccu)
PGATour %>% get_correlation(formula = Scoring.Average ~ GIR)
PGATour %>% get_correlation(formula = Scoring.Average ~ Sand.Saves)
PGATour %>% get_correlation(formula = Scoring.Average ~ Bounce_Back)
PGATour %>% get_correlation(formula = Scoring.Average ~ Scrambling)
PGATour %>% get_correlation(formula = Scoring.Average ~ PPR)

# Visualization of the relationship
ggplot(PGATour, aes(x = Money, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = DrDist, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = DrAccu, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = GIR, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = Sand.Saves, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = Bounce_Back, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = Scrambling, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(PGATour, aes(x = PPR, y = Scoring.Average)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


# Fit regression model
PGATour_Model_Allvariables <- lm(Scoring.Average ~ ., data = PGATour[,c(-1,-2)])
summary(PGATour_Model_Allvariables)
get_regression_table(PGATour_Model_Allvariables)

# R-square = 0.829

# From the linear regression model we can see that DrDist, DrAccu, Sand.Saves and Bounce.back are the insignificant varibales so we can drop these variables
#Model by using significant explanatory variables
PGATour_sig <- lm(Scoring.Average ~ . - DrDist - DrAccu - Sand.Saves - Bounce_Back, data = PGATour[,c(-1,-2)])
summary(PGATour_sig)
get_regression_table(PGATour_sig)

# R-square = 0.8281

# Creating Test Model 
num_rows <- nrow(PGATour)
num_cols <- ncol(PGATour)
set.seed(123)
?sample
train.index <- sample(row.names(PGATour), floor(0.8*num_rows)) 
test.index <- setdiff(row.names(PGATour), train.index) 
train.df <- PGATour[train.index, -c(1,2)]
test.df <- PGATour[test.index, -c(1,2)]
PGATour_mod1 <- lm(Scoring.Average ~ ., data = train.df) 
summary(PGATour_mod1)
preds.PGATour_mod1 <- predict(PGATour_mod1, newdata = test.df)
MSE1 <- mean((preds.PGATour_mod1 - test.df$Scoring.Average)^2)
RMSE1 <- sqrt(MSE1)
print(RMSE1)

#Variance influence factor
vif(PGATour_mod1)
# 0.1845 ...R-square = 0.8089

# Another model without the insignificant variables
PGATour_mod2 <- lm(Scoring.Average ~ . - DrDist - DrAccu - Bounce_Back - Sand.Saves , data = train.df) 
summary(PGATour_mod2)
get_regression_table(PGATour_mod2)
preds.PGATour_mod2 <- predict(PGATour_mod2, newdata = test.df) 
MSE2 <- mean((preds.PGATour_mod2 - test.df$Scoring.Average)^2)
RMSE2 <- sqrt(MSE2)
print(RMSE2)

# 0.1815 ...R-square = 0.8089

# -----Not to be considered for the RMSE value models ---This model we developed just to get rid of multicollinearity-----

# Another model without the insignificant and GIR variables
PGATour_mod7 <- lm(Scoring.Average ~ . - DrDist - DrAccu - Bounce_Back - Sand.Saves - GIR , data = train.df) 
summary(PGATour_mod7)
preds.PGATour_mod7 <- predict(PGATour_mod7, newdata = test.df) 
MSE7 <- mean((preds.PGATour_mod7 - test.df$Scoring.Average)^2)
RMSE7 <- sqrt(MSE7)
print(RMSE7)


# 0.3603 without GIR... But here the R-squared reduced by 0.4062

# Another model without the insignificant and PPR variables
PGATour_mod8 <- lm(Scoring.Average ~ . - DrDist - DrAccu - Bounce_Back - Sand.Saves - PPR , data = train.df) 
summary(PGATour_mod7)
preds.PGATour_mod8 <- predict(PGATour_mod8, newdata = test.df) 
MSE8 <- mean((preds.PGATour_mod8 - test.df$Scoring.Average)^2)
RMSE8 <- sqrt(MSE8)
print(RMSE8)

# 0.2621 ..But here the R-squared reduced by 0.4062
# ------Please ignore the above two models------





#Performed EDA to improve model
hist(PGATour$Scoring.Average, xlim=c(70, 73), ylim=c(0, 40), xlab="Scoring.Average")
hist(PGATour$Money, xlab="Money")
hist(PGATour$PPR, xlab="PPR")
hist(PGATour$GIR, xlab="GIR")
hist(PGATour$Scrambling, xlab="Scrambling")
boxplot(PGATour$Scoring.Average, data = PGATour)
boxplot(PGATour$Money, data = PGATour)
boxplot(PGATour$PPR, data = PGATour)
boxplot(PGATour$GIR, data = PGATour)
boxplot(PGATour$Scrambling, data = PGATour)

options(warn=-1)
scatterplotMatrix(~ Money + DrDist + DrAccu + GIR + Sand.Saves +  Bounce_Back + Scrambling + PPR , regLine = list(col = 2),
                  col = 1, smooth = list(col.smooth = 4, col.spread = 4), data = PGATour)



# Consider this model as our final model
# Developed model using log transformation on Scoring.Average & Money variables
PGATour$GIR_new = poly(PGATour$GIR, 2)
PGATour$PPR_new = poly(PGATour$PPR, 2)
PGATour$Money_new = log(PGATour$Money)
PGATour$Money_new2 = poly(PGATour$Money, 2)
PGATour$Scrambling_new = poly(PGATour$Scrambling, 2)
PGATour$Scoring_Avg_new = log(PGATour$Scoring.Average)

# Considering test and training data with the new variables
num_rows <- nrow(PGATour)
num_cols <- ncol(PGATour)
set.seed(123)
train.index <- sample(row.names(PGATour), floor(0.8*num_rows)) 
test.index <- setdiff(row.names(PGATour), train.index) 
train.df <- PGATour[train.index, -c(1,2)]
test.df <- PGATour[test.index, -c(1,2)]


# Considered polynomial transformation of the GIR, Money, PPR and Scrambling variables with all explanatory variables
PGA_Tour_mod3 <- lm(Scoring.Average ~ Money_new2 + DrAccu + DrDist + GIR_new + Scrambling_new + Bounce_Back + Sand.Saves + PPR_new - Money_new, data = train.df) 
summary(PGA_Tour_mod3)
get_regression_table(PGA_Tour_mod3)
preds.PGA_Tour_mod3 <- predict(PGA_Tour_mod3, newdata = test.df)
MSE3 <- mean((preds.PGA_Tour_mod3 - test.df$Scoring.Average)^2)
RMSE3 <- sqrt(MSE3)
print(RMSE3)
# 0.185539  ...R-square : 0.807


# Considered polynomial transformation of the GIR, Money and Scrambling variables with only significant variables
PGA_Tour_mod4 <- lm(Scoring.Average ~ Money_new2 + GIR + Scrambling + PPR - GIR_new - Scrambling_new - PPR_new , data = train.df) 
summary(PGA_Tour_mod4)
get_regression_table(PGA_Tour_mod4)
preds.PGA_Tour_mod4 <- predict(PGA_Tour_mod4, newdata = test.df)
MSE4 <- mean((preds.PGA_Tour_mod4 - test.df$Scoring.Average)^2)
RMSE4 <- sqrt(MSE4)
print(RMSE4)

# 0.1858984 ...R-square : 0.8067

# In this model used log transformation on Money variable without considering the insignificant variables to improve model's RMSE value
PGATour_mod5 <- lm(Scoring.Average ~ + Money_new + GIR + Scrambling + PPR - DrDist - DrAccu - Bounce_Back - Sand.Saves - Money_new2 - GIR_new - Scrambling_new - PPR_new , data = train.df) 
summary(PGATour_mod5)
preds.PGATour_mod5 <- predict(PGATour_mod5, newdata = test.df) 
MSE5 <- mean((preds.PGATour_mod5 - test.df$Scoring.Average)^2)
RMSE5 <- sqrt(MSE5)
print(RMSE5)

# 0.1750503 only log of Money variable....R-square : 0.8046

#In this model we have taken log of Scoring Average variable and considered all the variables
PGA_Tour_mod6 <- lm(Scoring_Avg_new ~ + GIR + PPR + Money + Scrambling + Bounce_Back + DrDist + DrAccu + Sand.Saves - Money_new - Money_new2 - Scrambling_new - GIR_new - PPR_new, data = train.df) 
summary(PGA_Tour_mod6)
get_regression_table(PGA_Tour_mod6)
preds.PGA_Tour_mod6 <- predict(PGA_Tour_mod6, newdata = test.df)
MSE6 <- mean((preds.PGA_Tour_mod6 - test.df$Scoring_Avg_new)^2)
RMSE6 <- sqrt(MSE6)
print(RMSE6)

# 0.002599 and R-squared : 0.8087 ...

# In this model we have taken log of Money & Scoring Average and all variables
PGA_Tour_mod7 <- lm(Scoring_Avg_new ~ + GIR + PPR + Money_new + Scrambling + Bounce_Back + DrDist + DrAccu + Sand.Saves - Money_new2 - Scrambling_new - GIR_new - PPR_new , data = train.df) 
summary(PGA_Tour_mod7)
get_regression_table(PGA_Tour_mod7)
preds.PGA_Tour_mod7 <- predict(PGA_Tour_mod7, newdata = test.df)
MSE7 <- mean((preds.PGA_Tour_mod7 - test.df$Scoring_Avg_new)^2)
RMSE7 <- sqrt(MSE7)
print(RMSE7)

# 0.002542 and R-squared : 0.8061....

# Log transformation on Money and Scoring Average variable considering only significant variables
PGA_Tour_mod8 <- lm(Scoring_Avg_new ~ Money_new + GIR + Scrambling  + PPR, data = train.df) 
summary(PGA_Tour_mod8)
get_regression_table(PGA_Tour_mod8)
preds.PGA_Tour_mod8 <- predict(PGA_Tour_mod8, newdata = test.df)
MSE8 <- mean((preds.PGA_Tour_mod8 - test.df$Scoring_Avg_new)^2)
RMSE8 <- sqrt(MSE8)
print(RMSE8)
# 0.002467448...R-square : 0.8044


# Final model
PGATour_Final.df = rbind(train.df, test.df)
PGATour_Final_Model <- lm(Scoring_Avg_new ~ Money_new + GIR + Scrambling  + PPR , data = PGATour_Final.df)
summary(PGATour_Final_Model)
get_regression_table(PGATour_Final_Model)
preds.PGATour_Final_Model <- predict(PGATour_Final_Model, newdata = PGATour_Final.df)
MSE_final <- mean((preds.PGATour_Final_Model - test.df$Scoring_Avg_new)^2)
RMSE_final <- sqrt(MSE_final)
print(RMSE_final)

# 0.007352203 and R-square : 0.8261
