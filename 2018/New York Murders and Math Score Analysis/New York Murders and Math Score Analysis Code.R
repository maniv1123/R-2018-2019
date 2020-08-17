library(vcd)
library(ggplot2)

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

# Question 1 Part 1Describe in graphs and numbers the distribution of math
# scores between male and female

library(openintro)
data(hsb2)

summary(hsb2)

#-- math vs gender table and graph
table(hsb2$gender, hsb2$math)
hsb2$math.recode[hsb2$math >= 20 & hsb2$math < 40] <- "20-39"
hsb2$math.recode[hsb2$math >= 40 & hsb2$math < 60] <- "40-59"
hsb2$math.recode[hsb2$math >= 60 & hsb2$math < 70] <- "60-69"
hsb2$math.recode[hsb2$math >= 70] <- "70 above"

table(hsb2$gender, hsb2$math.recode)
ggplot(data=hsb2, aes(x=math)) + geom_histogram(binwidth=10)+facet_grid(gender ~ .)
ggplot(data=hsb2, aes(x=math, fill = gender)) + geom_histogram(binwidth=20)

# Question 1 part 2: Is there a significant difference in the median score between these two
# groups? Use a permutation test to find out. Remember to set seed so
# that the grader can reproduce your result. ?set.seed

# score between males and females
male.score <- c(rep(hsb2$math[hsb2$gender == "male"]))
female.score <- c(rep(hsb2$math[hsb2$gender == "female"]))

# median score of male vs female
male.median <- median(c(rep(hsb2$math[hsb2$gender == "male"])))
female.median <- median(c(rep(hsb2$math[hsb2$gender == "female"])))

#rearrange into one big data set
data.male.score <- cbind(male.score, "male")
data.female.score <- cbind(female.score, "female")
data <- rbind(data.male.score,data.female.score)
head(data)
dim(data)
data <- as.data.frame(data)
names(data) <- c("combined","score")

#make sure that combined is numeric
data$combined <- as.numeric(as.character(data$combined))

#sampling code: shuffle the score label
shuffle.score <- function(data){
  score.shuffle <- sample(data$score)
  return(cbind(data,score.shuffle)[,c("combined","score.shuffle")])
}

#compute S from the shuffled labels
get.S <- function(data){
  new.data <- shuffle.score(data)
  select.male <- new.data$score.shuffle=="male"
  select.female <-  new.data$score.shuffle=="female"
  med.male <- median(new.data$combined[select.male])
  med.female <- median(new.data$combined[select.female])
  
  return (abs(med.female - med.male))
}

get.S(data)
#replicate n times
n = 10**4
Svector = replicate(n,get.S(data))

# Test statistic, S
s = abs(female.median - male.median)
s
# p value from mean of the sample
oneS = sum(Svector >= 1)/10000
oneS

#plot distribution
set.seed(10000)
hist(Svector)
plot(density(Svector))
table(Svector)
# This is an approximation of the distribution of S under the null. 

#Part 3: Is there a significant difference between male and female in the
#	   proportion of those who math score is 65 or more? Use a test of 
#	   your choice.

hsb2$math.recode2[hsb2$math >= 65] <- "Greater than 65"
table(hsb2$gender, hsb2$math.recode2)

# Using permutation test
male.high<- c(rep(hsb2$math[hsb2$gender == "male"]))
female.high<- c(rep(hsb2$math[hsb2$gender == "female"]))

#rearrange into one big data set
data.male.high <- cbind(male.high, "male")
data.female.high <- cbind(female.high, "female")
data<- rbind(data.male.high,data.female.high)
head(data)
dim(data)
data <- as.data.frame(data)
names(data) <- c("score65","gender")

#make sure that score65 is numeric
data$score65 <- as.numeric(as.character(data$score65))

#sampling code: shuffle the gender label
shuffle.gender <- function(data){
  gender.shuffle <- sample(data$gender)
  return(cbind(data,gender.shuffle)[,c("score65","gender.shuffle")])
}

#compute NewS from the shuffled labels
get.NewS <- function(data){
  new.data <- shuffle.gender(data)
  select.male <- new.data$gender.shuffle=="male"
  select.female <- new.data$gender.shuffle=="female"
  phat.female <- sum((new.data$score65[select.female]) >= 65)/sum(select.female)
  phat.male <- sum((new.data$score65[select.male]) >= 65)/sum(select.male)
  return (abs(phat.female - phat.male))
}

# Single p value
get.NewS(data)
#replicate n times
n = 10**4
Svector2 = replicate(n,get.NewS(data))

# p value from mean of the sample
oneS = sum(Svector2 >= 0.0094)/10000
oneS

#plot distribution
set.seed(100)
hist(Svector2)
plot(density(Svector2))


# Question 2
install.packages("OIdata")
library(OIdata)
data(murders)

# Part 1: Produce a map to visualize the murders by borough
 summary(murders)

install.packages("RColorBrewer")
library(RColorBrewer)
data(london_boroughs)
LB          <- london_boroughs
mtab        <- table(murders$borough)
LB$nmurders <- rep(mtab, rle(as.character(LB$name))$lengths)
p           <- ggplot()
p +
  geom_polygon(data=LB, aes(x=x, y=y, group = name, fill = nmurders),
               colour="white" ) +
  scale_fill_gradientn(colours = brewer.pal(7, "Blues"), limits=range(LB$nmurders))

# Part 2. Produce a table that counts the number of murders by borough.
table(murders$borough)

# Part 3  Is the count itself meaningful? What other statistics are we missing to
#	compute the murder rate? Go online to find them and compute the
#	murder rate by borough.

# Population obtained from greater london authority's website
borough.population <- c(187029,357538,232774,312245,310554,220087,364815,339314,313935
,255483,247182,182445,255540,240499,237927,275499,254927,206285,158251,160436,304481
,276938,200543,310460,281395,187527,288717,191123,256012,259742,307710,219582)

borough.names <- table(murders$borough)
borough.rate <- c((borough.names/borough.population)*100000)
borough.rate

# Question 3
# Part 1:Is there a significant difference in the rate of murders between
# boroughs? Answer this question by performing a permutation test.

# Compute S
Sample = abs(borough.names - (838 * ((borough.population)/8196995)))
S = sum(Sample) 
S

# Murders in every borough
boroughMnumbers <- c(murders$borough)
boroughMnumbers

population <- c(187029,357538,232774,312245,310554,220087,364815,339314,313935
,255483,247182,182445,255540,240499,237927,275499,254927,206285,158251,160436,304481
,276938,200543,310460,281395,187527,288717,191123,256012,259742,307710,219582)

places <- {c("Barking", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croyden",
 "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith", "Haringey",
 "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington",
 "Kingston", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond",
 "Southwark", "Sutton", "Tower", "Waltham", "Wordsworth", "Westminster")
}

numbers <- c(20,24,14,32,19,24,38,34,31,33,42,17,36,12,14,21,15,31,2,4,79,36,13,56,14,1
,49,14,32,32,26,23)

boroughData = cbind(places,population,numbers)

true.order <- rep(c(numbers))
#d shuffle function, get n samples from S.
one.sample <- function(){
  sam <- sample(c(true.order[]),length(true.order), TRUE)
  S = sam
  return(S)
}
dshuffle <- function(n){
  return(replicate(n, one.sample()))
}
#compute p-value from 10,000 draws
sim.S <- dshuffle(10000)
p.value <- abs(sim.S - (838 * (population/8196995)))

#write a function to take n (number of draws as argument), and returns the p-value
p.val <- function(n){
  sim.S <- dshuffle(n)
  return (sum(abs(sim.S - (838 * (population/8196995))))/n)
}
sum(p.val(10000))/10000

#simple plot. Set seed to get the same value
#you can play with the seed change the plot
set.seed(100)
nseq <- 2**(4:13)
pseq <- sapply(nseq,p.val)
plot(pseq ~ nseq)
lines(pseq ~ nseq)
#add in the true line
abline(a = 1/70,b=0,col="red")
