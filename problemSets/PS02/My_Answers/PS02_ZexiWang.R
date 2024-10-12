#####################
# load libraries
# set wd
# clear global .envir
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

lapply(c(),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

######1.1######
# create data
class_bribery <- matrix(c(14,6,7,7,7,1), nrow = 2, ncol = 3, byrow = TRUE)
# rename
rownames(class_bribery) <- c("Upper class", "Lower class")
colnames(class_bribery) <- c("Not Stopped", "Bribe requested", " Stopped/given warning")

# sum
sum_class_bribery <- cbind(class_bribery, row_sum = rowSums(class_bribery))
sum_class_bribery <- rbind(sum_class_bribery,col_sum = colSums(sum_class_bribery))

# calculate frequency
expected_values <- class_bribery*0
for (i in 1:2) {
  for (j in 1:3) {
    expected_values[i,j] <- (sum_class_bribery[i, "row_sum"]*sum_class_bribery[-(1:2),j])/sum(class_bribery)
  }
}

# chi-squared statistic
chi_statistic <- sum((class_bribery-expected_values)^2/expected_values)
print(chi_statistic)

# df
df <- (nrow(class_bribery)-1) * (ncol(class_bribery)-1)
print(df)

# test
chisq.test(class_bribery)

######1.2######
pvalue <- pchisq(chi_statistic, df, lower.tail=FALSE)
print(pvalue)
print("Since the p-value (0.15) > 0.10, we don't have sufficient evidence to reject the null hypothesis at the alpha = 0.10 significance level. This suggests that there is no significant difference in the likelihood of police officers soliciting a bribe from drivers based on their class.")

######1.3######
standard_residuals_values <- class_bribery*0
se_values <- class_bribery*0
for (i in 1:2) {
  for (j in 1:3) {
    se_values[i,j] <- sqrt(expected_values[i,j]*
                          (1-sum(class_bribery[i,])/sum(class_bribery))*
                          (1-sum(class_bribery[,j])/sum(class_bribery)))}             
}
standard_residuals_values <- (class_bribery-expected_values)/se_values

# test
chisq.test(class_bribery)$stdres

# list
print(standard_residuals_values)

#####################
# Problem 2
#####################



######2.1######
print("Ho = Having reserved village council heads for female does not affect the number 
drinking water facilities in the villages.")
print("Ha = Having reserved village council heads for female affects the number drinking 
water facilities in the villages.")

######2. 2######
data_problem2 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
regression_model <- lm(water ~ reserved, data = data_problem2)
summary(regression_model)

######2.3######
print("Since the p-value (≈0.02) < 0.05, we have sufficient evidence to reject the null hypothesis at the α = 0.05 significance level. This suggests that Having reserved seats for female politicians increase the number drinking water facilities in the villages.")