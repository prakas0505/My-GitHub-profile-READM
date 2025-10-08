#### Day1 task 3 - BHD and Height ####
#install.packages("nortest")
#install.packages('dgof')
library('dgof')
library('nortest')
#install.packages('DT')
library('DT')

setwd("/Users/Silicon14/Documents/EDA")
data <- read.csv("bhd_height.csv", header = T, sep=";", dec = ".")
library(data.table)
data <- as.data.table(data)
str(data)
#View(data)
head(data)
data$bhd_value <- as.numeric(data$bhd_value)
data$height_value <- as.numeric(data$height_value)

## subtask b - Characterize all three variables ##
# bhd (Breast height diameter in cm / quantitative / continuous )
# height (Height in m / quantitative / continuous)
# location (qualitative / nominal / categorical)

## subtask c - descriptive statistics for the quantitative variables ##
# statisics BHD NOBRB
#install.packages("ggplot2")
library(ggplot2)
data.NOBRB <- data[location=="NOBRB"]
summary(data.NOBRB$bhd_value)
boxplot(data.NOBRB$bhd_value, ylab="BHD (cm)", main="Breast Height Diameter at NOBRB",col = rainbow(ncol(trees)))

# statistics HEIGHT NOBRB
summary(data.NOBRB$height_value)
boxplot(data.NOBRB$height_value, ylab="Height (m)", main="Tree Height NOBRB",col = rainbow(ncol(trees)))
# statistics BHD OVP
data.OVP <- data[location=="OVP"]
summary(data.OVP$bhd_value)
boxplot(data.OVP$bhd_value, ylab="BHD (cm)", main="Breast Height Diameter at OVP",col = rainbow(ncol(trees)))
# statistics HEIGHT OVP
summary(data.OVP$height_value)
boxplot(data.OVP$height_value, ylab="Height (m)", main="Tree Height at OVP",col = rainbow(ncol(trees)))
plot(data.OVP)

## subtask d - normality test ##
shapiro.test(data.NOBRB$bhd_value) # normal
shapiro.test(data.NOBRB$height_value) # not normal (significantly different from normal distribution)
shapiro.test(data.OVP$bhd_value) # not normal
shapiro.test(data.OVP$height_value) # not normal
##p-value > 0.05
#we can assume the normality. 
library(nortest)
lillie.test(data.NOBRB$bhd_value)
lillie.test(data.NOBRB$height_value)
lillie.test(data.OVP$bhd_value)
lillie.test(data.OVP$height_value)

## subtask e -  If the data can be considered normally distributed, check for differences between growing areas. ##
# the data can not be considered normally distributed (task is skipped)




#### Day1 task 4 - ANOVA ####

data <- read.csv("temp_90481.csv", header = T, sep=",", dec = ".",stringsAsFactors = F)
data <- as.data.table(data)
str(data)
head(data)
tail(data)

## sub task b- run plot time ##
x <- as.vector(data$T_min)
y <- as.vector(data$T_max)
plot(data$T_min, xlab = "Days", ylab = "Temperature in C")
plot(data$T_max, xlab = "Days", ylab = "Temperature in C")

## subtask c -select specific periods ##
# in both cases a trend of increasing temperatues is visible
data_1900 <- data[1:680,] # data before 1900
data_1945_1955 <- data[2068:2408,] # data between 45 and 55
data_2000 <- data[3773:4247,] # data after 2000

## subtask d - Plot the probability density function ##
# for t_min
plot(density(data_1900$T_min), xlim=c(-2,18), ylim=c(0,0.3),
     main = "Daily Minimum Temperature in January for Mexican Weather Station 9048",
     xlab = "Minimum daily Temperature T_min in With C",
     ylab = "probability density")
class(data_1900)
x <- list(data_1900)
x<- as.numeric(unlist(data_1900))
hist(x)

lines(density(data_1945_1955$T_min), col="red")
lines(density(data_2000$T_min), col="green")
legend( "topright",                                # Add legend to density
       legend = c("< 1900", "1945 - 1955", "> 2000"),
       col = c("black", "red", "green"),
       lty = 1)
# for t_max
plot(density(data_1900$T_max), xlim=c(12,35), ylim=c(0,0.3),
     main = "Daily Maximum Temperature in January for Mexican Weather Station 9048",
     xlab = "Maximum daily Temperature T_min in ?C",
     ylab = "probability density")
lines(density(data_1945_1955$T_max), col="red")
lines(density(data_2000$T_max), col="green")
legend( "topright",                                # Add legend to density
        legend = c("< 1900", "1945 - 1955", "> 2000"),
        col = c("black", "red", "green"),
        lty = 1)

## subtask e - perform an ANOVA test to verify if there are differences between the three groups of measurements.##
# create data structure for doing anova
data_1900$Group <- "A"
data_1945_1955$Group <- "B"
data_2000$Group <-"C"
data_all <- rbind(data_1900,data_1945_1955,data_2000)
data_all <- data_all[ ,`:=`(Date = NULL)] 
# anova for t_min
# Compute the analysis of variance
anova_t_min <- aov(T_min ~ Group, data = data_all)
summary(anova_t_min) # p < 0.05, therfore a least one group mean is significantly different from others
# find out with post-hoc test between cwhich groups differences are
TukeyHSD(anova_t_min)
# Since each of the adjusted p-values is less than .05, we can conclude that there is a significant difference in mean minimum temperature between each group.
# anova for t_max
anova_t_max <- aov(T_max ~ Group, data = data_all)
summary(anova_t_max) 
TukeyHSD(anova_t_max) # same as for tmin

## subtask f - use average mothly value and repeat above analysis ##
n <- 31
mean_1900 <- data_1900[, mean(data_1900$T_min), by= (seq(nrow(data_1900)) - 1) %/% n]
plot(mean_1900)
mean_1900 <- data_1900[,(mean1878 = mean(T_min[startsWith("1878") = Date]))]
# install.packages("tidyverse")
library(tidyverse)
mean_1900 <- data_1900 %>% filter(str_detect(Date, "^1878"))

data_2000$Date <- as.Date(data_2000$Date) # dates are not clear for algorithm, hence no easy way to answer the task...


