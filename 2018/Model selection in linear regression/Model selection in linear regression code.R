library(openintro)
data(mammals)
mammals2 <- mammals[rowSums(is.na(mammals)) == 0,]
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

# Question 1
# Part 1: We want to fit a linear regression model that can be used to predict
# TotalSleep. Explain why Dreaming, NonDreaming and Species are BAD variables
# to include in this regression model.
mammals
head(mammals2$Species)
head(mammals2$Dreaming)
head(mammals2$NonDreaming)

# Part 2:Treat Predation, Exposure and Danger as numericals. Run model1, the
# linear regression model with TotalSleep vs BodyWt, BrainWt, LifeSpan, Gestation,
# Predation, Exposure and Danger. Clearly show the R command that you use, and
# include the R’s model summary
mammals2$Predation <- as.numeric(mammals2$Predation)
mammals2$Exposure <- as.numeric(mammals2$Exposure)
mammals2$Danger <- as.numeric(mammals2$Danger)
model1 <- lm(TotalSleep ~  BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger,data=mammals2)
summary(model1)
par(mfrow = c(2,2))
plot(model1)

# Question 2
# Part 1: Treat Predation, Exposure and Danger as categoricals. Run model2, the
# linear regression model with TotalSleep vs BodyWt, BrainWt, LifeSpan, Gestation,
# Predation, Exposure and Danger. Clearly show the R command that you use, and
# include the R’s model summary.

mammals2$Predation <- as.factor(mammals2$Predation)
mammals2$Exposure <- as.factor(mammals2$Exposure)
mammals2$Danger <- as.factor(mammals2$Danger)
model2 <- lm(TotalSleep ~  BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger,data=mammals2)
summary(model2)

# Part 2: Compare model1 and model 2: comment on the coefficients and the
# diagnostic plots. Say which, if any, of the (a) independence (b) normal
# distribution and (c) constant variance assumptions are violated.
plot(model1)
plot(model2)

# Question 3
# Part 1: Do variable selection with the stepAIC command, starting with model1.
# Call this model1.AIC. Compare model1.AIC against model1: comment on the
# coefficients and the diagnostic plots.
library(MASS)
mammals2$Predation <- as.numeric(mammals2$Predation)
mammals2$Exposure <- as.numeric(mammals2$Exposure)
mammals2$Danger <- as.numeric(mammals2$Danger)
model1.aic <- stepAIC(model1)
summary(model1.aic)
plot(model1.aic)

# Part 2: Do variable selection with the stepAIC command, starting with model2.
# Call this model2.AIC. Compare model2.AIC against model2: comment on the coefficients
mammals2$Predation <- as.factor(mammals2$Predation)
mammals2$Exposure <- as.factor(mammals2$Exposure)
mammals2$Danger <- as.factor(mammals2$Danger)
model2.aic <- stepAIC(model2)
summary(model2.aic)
plot(model2.aic)
