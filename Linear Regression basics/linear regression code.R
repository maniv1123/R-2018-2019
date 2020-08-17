library(vcd)
library(ggplot2)
library(openintro)

#--- the following code makes sure that R has the correct data types
#--- this is important for all R commands to work properly
convert <- function(obj,types){
    for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                                   numeric = as.numeric, 
                                   factor = as.factor, date = as.Date, time = time)
        obj[,i] <- FUN(obj[,i])
    }
    obj
}


rm(list = ls())
data(hsb2)

# Question 2
# Part 1 Do a scatter plot of social studies (socst) scores vs math scores(math).
# Report the correlation.

par(mfrow = c(1,1))
plot(hsb2$socst ~ hsb2$math)
cor(hsb2$socst, hsb2$math)

# Part 2 Fit a linear regression model that can be used to predict social
# studies scores based on math scores. Write down the model equation that R
# gives you. Overlay this regression line on top of the scatterplot.

hsb2.model <- lm(socst ~ math, data = hsb2)
summary(hsb2.model)
#summary: best line is Y = 19.56 + 0.62*X
#let's visualize the line of best fit on the scatterplot
b0 = hsb2.model$coefficients[1] #intercept
b1 = hsb2.model$coefficients[2] #slope
#plot line on scatterplot
abline(a = b0, b = b1, col = "red", lwd = 2)
summary(hsb2$socst)

# Part 5 Do a diagnostic plot for your model. Say which, if any, of the (a) no
# mean trend, (b) normal distribution and (c) constant variance assumptions
# are violated. Based on your answer, is the model a good fit? That
# is, is there a linear relationship between social studies scores and math
# scores?

# diagnostics
par(mfrow = c(2,2))
plot(hsb2.model)

# Part 6 Report the R2 value of your model. What is the meaning of this value?
r <- cor(hsb2$socst, hsb2$math)
r2 <- r ** 2
r2

# Question 3
# Part 1: Do a plot of qualified vs weight, qualified vs relate, and qualified vs
# weight:relate. For each plot, briefly describe what you see. Based on
# these plots alone, do you think sitting next to an obese woman has an
# adverse effect on the applicant’s qualification ratings?

data <- read.table("weight.txt", header = T)
summary(data)
par(mfrow = c(1,1))
data$weight <- as.factor(data$weight)
data$relate <- as.factor(data$relate)
plot(qualified ~ weight, data = data)
plot(qualified ~ relate, data = data)
plot(qualified ~ I(weight:relate), data = data)

# Part 2 Run three regression models with the following definitions: 
#	• weights.model1: qualified vs weight + relate.
#	• weights.model2: qualified vs weight:relate
#	• weights.model3: qualified vs weight + relate + weight:relate

weights.model1 <- lm(qualified ~ weight + relate, data = data)
summary(weights.model1)

weights.model2 <- lm(qualified ~ weight:relate, data = data)
summary(weights.model2)

weights.model3 <- lm(qualified ~ weight + relate + weight:relate, data = data)
summary(weights.model3)

# Part 3 Do a diagnostic plot for each of your models. Say which, if any, of
# the (a) independence (no mean trend) (b) normal distribution and (c)
# constant variance assumptions are violated.
par(mfrow = c(2,2))
plot(weights.model1)
plot(weights.model2)
plot(weights.model3)