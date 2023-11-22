
# set wd
setwd("C:/Users/nicol/Downloads")

# packages for cleaning
library(tidyverse)
library(tableone)
library(survival)
library(ggplot2)
library(ggmap)
library(table1)
library(lubridate)
library(dplyr)
library(plyr)

# trying medicaldata package
library(medicaldata)

data(package = "medicaldata")

help(polyps)
help(licorice_gargle)
help(indo_rct)


# polyps dataset is smallest, so will start with that
# these three are full RCTs, should be able to collate them eventually

summary(polyps)
view(polyps)

# separate placebo and exp
pol_pla = subset(polyps, treatment == "placebo")
summary(pol_pla)

pol_exp = subset(polyps, treatment == "sulindac")
summary(pol_exp)

# baseline is not a good measurement apparently, too much difference in baseline means
# should take difference from baseline as measurement

pol_pla$diff3m <- (pol_pla$number3m-pol_pla$baseline)
print(pol_pla$diff3m)

pol_pla$diff12m <- (pol_pla$number12m-pol_pla$baseline)
print(pol_pla$diff12m)



pol_exp$diff3m <- (pol_exp$number3m-pol_exp$baseline)
print(pol_exp$diff3m)

pol_exp$diff12m <- (pol_exp$number12m-pol_exp$baseline)
print(pol_exp$diff12m)

summary(pol_pla)
summary(pol_exp)

# redo in combo placebo exp data

polyps$diff3m <- (polyps$number3m-polyps$baseline)


polyps$diff12m <- (polyps$number12m-polyps$baseline)



# there are three types of inputs for the model: x1 (demographic), x2 (outcome), labels (test/control)

# change the labels to 1 (test) or 0 (control)

polyps$treatment <- as.character(polyps$treatment)

polyps$treatment[polyps$treatment=="sulindac"] <- "1"
polyps$treatment[polyps$treatment=="placebo"] <- "0"

# one hot encode demographic info, then merge into a single ordered value
# all we have here is sex and age, so that is what we will start with
# can change later
# numeric values need to be categorized/binned to be one hot encoded

# since age will be used often, bins should be roughly the same for all datasets
# bin by 5
polyps <- polyps %>% mutate(age = cut(age, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)))

# sex is already categorized

# one hot encoding here

library(caret)

#subset to avoid one hot encoding other variables
polyps_drop = subset(polyps, select = c("sex", "age"))

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=polyps_drop)

#perform one-hot encoding on data frame
polyps_hot <- data.frame(predict(dummy, newdata=polyps_drop))

# combining all demo values into single string of code

polyps_hot_one <- unite(polyps_hot, col='demo', sep='')

# yes - just need to make a standardized key for reading it 

polyps$demo <- polyps_hot_one$demo

# percentage difference (relative change)

polyps$rel_3 <- ((polyps$number3m) - (polyps$baseline))/(polyps$baseline)

polyps$rel_12 <- ((polyps$number12m) - (polyps$baseline))/(polyps$baseline)


# drop unnecessary columns

polyps = subset(polyps, select = c("demo", "treatment", "rel_3", "rel_12"))



# export data

write.csv(polyps, file= "C:/Users/nicol/Downloads/polyps.csv", row.names=FALSE)


