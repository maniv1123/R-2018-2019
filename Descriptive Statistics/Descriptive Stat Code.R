#Homework 1

setwd("C:\\Users\\Admin\\Downloads")
library(vcd)
library(ggplot2)
library(chron)
rm(list = ls())
data <- read.csv("email.csv")


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

#set the target types of the various columns
numcol = dim(data)[2]
type.target = rep("factor", numcol)
type.target[c(1:numcol)[names(data) == "cc"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "image"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "attach"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "dollar"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "inherit"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "viagra"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "password"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "num_char"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "line_breaks"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "exclaim_mess"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "time"]] <- "character"
type.target[c(1:numcol)[names(data) == "to_multiple"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "from"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "sent_email"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "winner"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "format"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "re_subj"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "exclaim_subj"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "urgent_subj"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "number"]] <- "numeric"

#check: names and target types
cbind(names(data), type.target)
#convert
data <- convert(data, type.target)
#------- Start analysis. 


#--- spam vs exclaim_mess
#dotchart exclaim_mess vs spam
par(mfrow = c(1,2))
dotchart(data$exclaim_mess[data$spam == 0], main = "not spam")
dotchart(data$exclaim_mess[data$spam == 1], main = "spam")
par(mfrow = c(1,1))

#-histogram 
#histogram exclaim_mess vs spam, same plot
ggplot(data, aes(x=exclaim_mess, fill = spam)) + geom_histogram(binwidth=20)

#boxplot 
ggplot(data, aes(x=1,y = line_breaks)) + geom_boxplot()
#boxplot exclaim_mess vs spam
ggplot(data, aes(x=spam,y = exclaim_mess)) + geom_boxplot()

#violin plot exclaim_mess vs spam
ggplot(data, aes(x=spam,y=exclaim_mess)) + geom_violin()

# Question2
# Recode into 0, 1, 2, >=3
data$exclaim_mess.recode[data$exclaim_mess==0] <- "0"
data$exclaim_mess.recode[data$exclaim_mess==1] <- "1"
data$exclaim_mess.recode[data$exclaim_mess==2] <- "2"
data$exclaim_mess.recode[data$exclaim_mess>=3] <- "3 or more"


# Check type
class(data$exclaim_mess.recode)

# table and mosaic plot for spam vs exclaim_mess.recode
table(data$spam, data$exclaim_mess.recode)
mosaic(spam ~ exclaim_mess.recode, data = data)

# Question 3

#--- spam vs to_multiple
table(data$spam, data$to_multiple)
mosaic(spam ~ to_multiple, data = data)


#--- spam vs from
table(data$spam, data$from)
par(mfrow = c(1,2))
dotchart(data$from[data$spam == 0], main = "not spam")
dotchart(data$from[data$spam == 1], main = "spam")
par(mfrow = c(1,1))


#-- spam vs cc
table(data$spam, data$cc)
# Recode into 0, 1, 2,3, >=4
data$cc.recode[data$cc == 0] <- "0"
data$cc.recode[data$cc==1] <- "1"
data$cc.recode[data$cc==2] <- "2"
data$cc.recode[data$cc==3] <- "3"
data$cc.recode[data$cc>=4] <- "4 or more"
# table and mosaic plot for spam vs cc.recode
table(data$spam, data$cc.recode)
mosaic(spam ~ cc.recode, data = data)

# spam vs sent_email
table(data$spam, data$sent_email)
par(mfrow = c(1,2))
dotchart(data$sent_email[data$spam == 0], main = "not spam")
dotchart(data$sent_email[data$spam == 1], main = "spam")
par(mfrow = c(1,1))
# mosaic plot for spam vs sent_email
mosaic(spam ~ sent_email, data = data)

#spam vs image
table(data$spam, data$image)


# Recode into 0, 1, 2, >=3
data$image.recode[data$image == 0] <- "0"
data$image.recode[data$image ==1] <- "1"
data$image.recode[data$image ==2] <- "2"
data$image.recode[data$image >=3] <- "3 or more"
# table and mosaic plot for spam vs image.recode
table(data$spam, data$image.recode)
# mosaic
mosaic(spam ~ image.recode, data = data)

#spam vs attach
table(data$spam, data$attach)
# Recode into 0, 1, 2, >=3
data$attach.recode[data$attach == 0] <- "0"
data$attach.recode[data$attach ==1] <- "1"
data$attach.recode[data$attach ==2] <- "2"
data$attach.recode[data$attach >=3] <- "3 or more"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$attach.recode)
mosaic(spam ~ attach.recode, data = data)

#spam vs dollar
table(data$spam, data$dollar)
# Recode into 0, 1, 2,3, >=4
data$dollar.recode[data$dollar == 0] <- "0"
data$dollar.recode[data$dollar==1] <- "1"
data$dollar.recode[data$dollar==2] <- "2"
data$dollar.recode[data$dollar==3] <- "3"
data$dollar.recode[data$dollar>=4] <- "4 or more"
# table and mosaic plot for spam vs dollar
table(data$spam, data$dollar.recode)
mosaic(spam ~ dollar.recode, data = data)
#dotchart
par(mfrow = c(1,2))
dotchart(data$dollar[data$spam == 0], main = "not spam")
dotchart(data$dollar[data$spam == 1], main = "spam")
par(mfrow = c(1,1))

#spam vs winner
table(data$spam, data$winner)
mosaic(spam ~ winner, data = data)

#spam vs inherit
table(data$spam, data$inherit)
mosaic(spam ~ inherit, data = data)

#spam vs viagra
table(data$spam, data$viagra)
#dotchart
par(mfrow = c(1,2))
dotchart(data$viagra[data$spam == 0], main = "not spam")
dotchart(data$viagra[data$spam == 1], main = "spam")
par(mfrow = c(1,1)

#spam vs password 
table(data$spam, data$password)
# Recode into 0, 1, 2, >=3
data$password.recode[data$password == 0] <- "0"
data$password.recode[data$password ==1] <- "1"
data$password.recode[data$password ==2] <- "2"
data$password.recode[data$password >=3] <- "3 or more"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$password.recode)
mosaic(spam ~ attach.recode, data = data)

#spam vs num_char 
table(data$spam, data$num_char)
# Recode into <=5k, <25k, <50k, <100k, >=150
data$num_char.recode[data$num_char < 5] <- "<=5K"
data$num_char.recode[data$num_char >= 5 & data$num_char < 25] <- "<25k"
data$num_char.recode[data$num_char >= 25 & data$num_char < 50] <- "<50k"
data$num_char.recode[data$num_char >= 50 & data$num_char < 100] <- "<100k"
data$num_char.recode[data$num_char >= 25] <- ">100K"
# table and mosaic plot for spam vs num_char.recode
table(data$spam, data$num_char.recode)
mosaic(spam ~ num_char.recode, data = data)

#spam vs line_breaks
table(data$spam, data$line_breaks)
# Recode into <50, <100, <500, <1000, >=1000
data$line_breaks.recode[data$line_breaks< 50] <- "<50"
data$line_breaks.recode[data$line_breaks>= 50 & data$line_breaks < 100] <- "<100"
data$line_breaks.recode[data$line_breaks >= 100 & data$line_breaks < 500] <- "<500"
data$line_breaks.recode[data$line_breaks >= 500 & data$line_breaks < 1000] <- "<1000"
data$line_breaks.recode[data$line_breaks >= 1000] <- ">=1000"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$line_breaks.recode)
mosaic(spam ~ line_breaks.recode, data = data)

#spam vs format 
#table and mosaic plot
table(data$spam, data$format)
mosaic(spam ~ format, data = data)

#spam vs re_subj 
#table and box plot
table(data$spam, data$re_subj)
ggplot(data, aes(x=re_subj, fill=spam)) + geom_bar()

#spam vs exclaim_subj
#table and box plot
table(data$spam, data$exclaim_subj)
mosaic(spam ~ format, data = data)
#Homework 1

setwd("C:\\Users\\Admin\\Downloads")
library(vcd)
library(ggplot2)
rm(list = ls())
data <- read.csv("email.csv")


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

#set the target types of the various columns
numcol = dim(data)[2]
type.target = rep("factor", numcol)
type.target[c(1:numcol)[names(data) == "cc"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "image"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "attach"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "dollar"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "inherit"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "viagra"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "password"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "num_char"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "line_breaks"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "exclaim_mess"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "time"]] <- "date"
type.target[c(1:numcol)[names(data) == "to_multiple"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "from"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "sent_email"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "winner"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "format"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "re_subj"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "exclaim_subj"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "urgent_subj"]] <- "numeric"
type.target[c(1:numcol)[names(data) == "num_char"]] <- "numeric"

#check: names and target types
cbind(names(data), type.target)
#convert
data <- convert(data, type.target)
#------- Start analysis. 



#--- spam vs exclaim_mess
#dotchart exclaim_mess vs spam
par(mfrow = c(1,2))
dotchart(data$exclaim_mess[data$spam == 0], main = "not spam")
dotchart(data$exclaim_mess[data$spam == 1], main = "spam")
par(mfrow = c(1,1))

#-histogram 
#histogram exclaim_mess vs spam, same plot
ggplot(data, aes(x=exclaim_mess, fill = spam)) + geom_histogram(binwidth=20)

#boxplot 
ggplot(data, aes(x=1,y = line_breaks)) + geom_boxplot()
#boxplot exclaim_mess vs spam
ggplot(data, aes(x=spam,y = exclaim_mess)) + geom_boxplot()

#violin plot exclaim_mess vs spam
ggplot(data, aes(x=spam,y=exclaim_mess)) + geom_violin()

# Question2
# Recode into 0, 1, 2, >=3
data$exclaim_mess.recode[data$exclaim_mess==0] <- "0"
data$exclaim_mess.recode[data$exclaim_mess==1] <- "1"
data$exclaim_mess.recode[data$exclaim_mess==2] <- "2"
data$exclaim_mess.recode[data$exclaim_mess<=3] <- "3 or more"


# Check type
class(data$exclaim_mess.recode)

# table and mosaic plot for spam vs exclaim_mess.recode
table(data$spam, data$exclaim_mess.recode)
mosaic(spam ~ exclaim_mess.recode, data = data)

# Question 3

#--- spam vs to_multiple
table(data$spam, data$to_multiple)
mosaic(spam ~ to_multiple, data = data)


#--- spam vs from
table(data$spam, data$from)
par(mfrow = c(1,2))
dotchart(data$from[data$spam == 0], main = "not spam")
dotchart(data$from[data$spam == 1], main = "spam")
par(mfrow = c(1,1))


#-- spam vs cc
table(data$spam, data$cc)
# Recode into 0, 1, 2,3, >=4
data$cc.recode[data$cc == 0] <- "0"
data$cc.recode[data$cc==1] <- "1"
data$cc.recode[data$cc==2] <- "2"
data$cc.recode[data$cc==3] <- "3"
data$cc.recode[data$cc>=4] <- "4 or more"
# table and mosaic plot for spam vs cc.recode
table(data$spam, data$cc.recode)
mosaic(spam ~ cc.recode, data = data)

# spam vs sent_email
table(data$spam, data$sent_email)
par(mfrow = c(1,2))
dotchart(data$sent_email[data$spam == 0], main = "not spam")
dotchart(data$sent_email[data$spam == 1], main = "spam")
par(mfrow = c(1,1))
# mosaic plot for spam vs sent_email
mosaic(spam ~ sent_email, data = data)

#spam vs image
table(data$spam, data$image)


# Recode into 0, 1, 2, >=3
data$image.recode[data$image == 0] <- "0"
data$image.recode[data$image ==1] <- "1"
data$image.recode[data$image ==2] <- "2"
data$image.recode[data$image >=3] <- "3 or more"
# table and mosaic plot for spam vs image.recode
table(data$spam, data$image.recode)
# mosaic
mosaic(spam ~ image.recode, data = data)

#spam vs attach
table(data$spam, data$attach)
# Recode into 0, 1, 2, >=3
data$attach.recode[data$attach == 0] <- "0"
data$attach.recode[data$attach ==1] <- "1"
data$attach.recode[data$attach ==2] <- "2"
data$attach.recode[data$attach >=3] <- "3 or more"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$attach.recode)
mosaic(spam ~ attach.recode, data = data)

#spam vs dollar
table(data$spam, data$dollar)
# Recode into 0, 1, 2,3, >=4
data$dollar.recode[data$dollar == 0] <- "0"
data$dollar.recode[data$dollar==1] <- "1"
data$dollar.recode[data$dollar==2] <- "2"
data$dollar.recode[data$dollar==3] <- "3"
data$dollar.recode[data$dollar>=4] <- "4 or more"
# table and mosaic plot for spam vs dollar
table(data$spam, data$dollar.recode)
mosaic(spam ~ dollar.recode, data = data)
#dotchart
par(mfrow = c(1,2))
dotchart(data$dollar[data$spam == 0], main = "not spam")
dotchart(data$dollar[data$spam == 1], main = "spam")
par(mfrow = c(1,1))

#spam vs winner
table(data$spam, data$winner)
mosaic(spam ~ winner, data = data)

#spam vs inherit
table(data$spam, data$inherit)
mosaic(spam ~ inherit, data = data)

#spam vs viagra
table(data$spam, data$viagra)
#dotchart
par(mfrow = c(1,2))
dotchart(data$viagra[data$spam == 0], main = "not spam")
dotchart(data$viagra[data$spam == 1], main = "spam")
par(mfrow = c(1,1)

#spam vs password 
table(data$spam, data$password)
# Recode into 0, 1, 2, >=3
data$password.recode[data$password == 0] <- "0"
data$password.recode[data$password ==1] <- "1"
data$password.recode[data$password ==2] <- "2"
data$password.recode[data$password >=3] <- "3 or more"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$password.recode)
mosaic(spam ~ attach.recode, data = data)

#spam vs num_char 
table(data$spam, data$num_char)
# Recode into <=5k, <25k, <50k, <100k, >=150
data$num_char.recode[data$num_char < 5] <- "<=5K"
data$num_char.recode[data$num_char >= 5 & data$num_char < 25] <- "<25k"
data$num_char.recode[data$num_char >= 25 & data$num_char < 50] <- "<50k"
data$num_char.recode[data$num_char >= 50 & data$num_char < 100] <- "<100k"
data$num_char.recode[data$num_char >= 25] <- ">100K"
# table and mosaic plot for spam vs num_char.recode
table(data$spam, data$num_char.recode)
mosaic(spam ~ num_char.recode, data = data)

#spam vs line_breaks
table(data$spam, data$line_breaks)
# Recode into <50, <100, <500, <1000, >=1000
data$line_breaks.recode[data$line_breaks< 50] <- "<50"
data$line_breaks.recode[data$line_breaks>= 50 & data$line_breaks < 100] <- "<100"
data$line_breaks.recode[data$line_breaks >= 100 & data$line_breaks < 500] <- "<500"
data$line_breaks.recode[data$line_breaks >= 500 & data$line_breaks < 1000] <- "<1000"
data$line_breaks.recode[data$line_breaks >= 1000] <- ">=1000"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$line_breaks.recode)
mosaic(spam ~ line_breaks.recode, data = data)

#spam vs format 
#table and mosaic plot
table(data$spam, data$format)
mosaic(spam ~ format, data = data)

#spam vs re_subj 
#table and violin plot
table(data$spam, data$re_subj)
ggplot(data, aes(x=spam, y=re_subj)) + geom_violin()

#spam vs exclaim_subj
#table and mosaic plot
table(data$spam, data$exclaim_subj)
mosaic(spam ~ exclaim_subj, data = data)

#spam vs urgent_subj
#table
table(data$spam, data$urgent_subj)
#dotchart urgent_subj vs spam
par(mfrow = c(1,2))
dotchart(data$urgent_subj[data$spam == 0], main = "not spam")
dotchart(data$urgent_subj[data$spam == 1], main = "spam")
par(mfrow = c(1,1))

#spam vs exclaim_mess
#refering back to question number 1 and 2 upon reading instructions on piazza

#spam vs number
#table
table(data$spam, data$number)
mosaic(spam ~ number, data = data)

#spam vs time
table(data$time, data$spam)
import_month <- substr(data$time,6,7)
import_month
# Recode into Dec, feb, mar
data$time.recode[import_month == "12"] <- "Dec"
data$time.recode[import_month == "01"] <- "Jan"
data$time.recode[import_month == "02"] <- "Feb"
data$time.recode[import_month == "03"] <- "Mar"
# table and mosaic plot for spam vs attach.recode
table(data$spam, data$time.recode)
mosaic(spam ~ time.recode, data = data)