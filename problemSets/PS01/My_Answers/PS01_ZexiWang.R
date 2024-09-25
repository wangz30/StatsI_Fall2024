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

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

### 1.1 ### 90% confidence interval
upper <- round(qnorm(0.95),2)
lower <- round(qnorm(0.05),2)
# mean
mean_y <- mean(y)
# sd
sd_y <- sd(y)/sqrt(length((y)))
# upper&lower bound
upper_90 = mean_y+upper*sd_y
lower_90 = mean_y+lower*sd_y
# output
cat("90% confidence interval for the average student IQ in the school is:(", round(lower_90, 2), ",", round(upper_90, 2),")")

### 1.2 ### t test
print(t.test(y, mu = 100))
print("t-test results indicate that the average student IQ in this school is not significantly different from the average IQ score of 100 among all schools in the country( p = 0.5569 > 0.05)")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

### 2.1 ### 
setwd("D:/大学/AAAA_TCD的文件/AAAA_上课/StatsI_Fall2024/problemSets/PS01/My_Answers/")
## Plot Y~X1
png(file = "output/Y~X1.png")
plot( expenditure$X1,
      expenditure$Y,
      ylab="Y",
      xlab="X1",
      main="The Relationship between Y and X1")
dev.off()
## Plot Y~X2
png(file = "output/Y~X2.png")
plot( expenditure$X2,
      expenditure$Y,
      ylab="Y",
      xlab="X2",
      main="The Relationship between Y and X2")
dev.off()
## Plot Y~X3 
png(file = "output/Y~X3.png")
plot( expenditure$X3,
      expenditure$Y,
      ylab="Y",
      xlab="X3",
      main="The Relationship between Y and X3")
dev.off()
print("The scatter plots demonstrate that as X1 and X3 increase, Y also rises correspondingly. As X2 increases, Y decreases until X2 reaches approximately 300, at which point Y begins to increase with further increases in X2")

### 2.2 ###
png(file = "output/Y~Region.png")
boxplot( expenditure$Y ~ expenditure$Region, 
        main="The Relationship between Y and Region",
        ylab="Y",
        xlab="Region",
        names=c("1", "2","3","4"))
dev.off()
print("The box plot indicates that region 4 has  has the highest per capita expenditure on housing assistance ")

### 2.3 ###