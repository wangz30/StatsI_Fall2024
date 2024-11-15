#####################
## Zexi Wang PS04 ###
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(car)
library(stargazer)

########### Question 1 ###########

### a ###
data(Prestige)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
Prestige$professional <- as.factor(Prestige$professional)
# It can be seen that there are 4 missing values in 
# the "type", which makes their "professional" NA.
# If we wanna dropping them, here is the code
# Prestige_clean <- na.omit(Prestige)

### b ###
model_pres <- lm(prestige ~ income + professional + income:professional, data = Prestige) 
summary(model_pres)
stargazer(model_pres, type = "text", out = "model_pres.txt")

### c ###
# Prestige = 21.142 + 0.003*Income + 37.781*Professional - 0.002*Income*Professional 

### d ###
# The coefficient for income in the regression model indicates the expected change in the prestige score for each additional dollar earned.
# For blue-collar and white-collar workers, for each additional dollar of income, the prestige score is expected to increase by 0.003 on average.

### e ###
# The coefficient for professional shows the expected change in prestige score when moving from a white-collar or blue-collar position to a professional role, with income at zero. A professional worker is expected to have a prestige score that is 37.781 points higher than an equally low-income white-collar or blue-collar worker.

### f ###
increase_effect <- (coef(model_pres)["income"] + coef(model_pres)["income:professional"]) * 1000

### g ###
change_effect <- coef(model_pres)["income"] + coef(model_pres)["income:professional"] * 6000

########### Question 2 ###########

### a ###
coef_sign <- 0.042
std_error_sign <- 0.016
t_value_sign <- coef_sign / std_error_sign
p_value_sign <- 2 * pt(-abs(t_value_sign), df = 128) 
p_value_sign < 0.05
# t = 2.625, p = 0.0097

### b ###
coef_next <- 0.042
std_error_next <- 0.013
t_value_next <- coef_next / std_error_next
p_value_next <- 2 * pt(-abs(t_value_next), df = 128) 
p_value_next < 0.05
# t =3.23, p = 0.0015
