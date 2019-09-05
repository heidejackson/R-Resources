
rm(list = ls())
getwd()

setwd("C:/Users/mprc/Documents/")
dir.create("Assorted_Topics_in_R")
setwd("Assorted_Topics_in_R")

#RUN ME ONCE
#install.packages("tidyverse")
#install.packages("mice")
#install.packages("xtable")
#install.packages("sm")
library(tibble)
library(readr)
library(haven)
library(readr)
library(dplyr)
library(mice)
library(xtable)
library(sm)
#R is really great at creating fake data--here's how easy it is to make up a dataset
#before we get to far, let's set a seed so findings are reproducible
set.seed(12722017)
#now let's set our id variable, the sequence command, can be used to specify a series of sequential numbers
id<-seq(1:100)
#seq with the by option can specify sequence of numbers in multiples, e.g. 
seq(1,50, by=7)
#below I show examples for simulating variables with various distributions
#for a normal distribution
error<-rnorm(n=100, mean=0, sd=1)
x1<-rnorm(n=100, mean=0, sd=1)
#for a binomial distribution
x2<-rbinom(n=100, 1, .5)
missing<-rbinom(n=100,1,.3)
#there are similar functions for other distributions such as uniform, poisson, or negative binomials
#W, for more info check out help(runif) help(rpois) or help(rnbinom)

#the sample function can also be useful for ordinal variables
x3<-sample(x=0:3, size=100, replace=TRUE, prob=c(.2,.1,.4,.3))
#a variant of the sample code could also be used if we want to create a seemingly random id for each case
# the replace option specifies whether we are sampling with or without replacement
sample(x=10000:99999, size=100, replace=FALSE)
#let's consolidate these variables into a dataframe
fakedata<-tibble(x1,x2,x3,error,id,missing)
#now i want to create a twp categpru variable where the probability of a 1 response varies by whether missing is
#1 or not
fakedata$x4<-rbinom(100,1,ifelse(fakedata$missing==1,.7,.3))

#now let's set up our outcome to be a function of some random error, our observed xs and the likelihood of being missing
#in practice the relationship between y and missings would be unobserved
fakedata$y<-.5*fakedata$x1+-.1*fakedata$x2+error+.2*fakedata$missing+-.5*fakedata$x4
fakedata$y2<-.5*fakedata$x1+-.1*fakedata$x2+error




#now I'm going to set a subset of data to be missing and drop the missing and error terms which wouldn't appear in 
#the real life data

fakedatam<-mutate(fakedata,y=ifelse(missing==1,NA,y), y2=ifelse(missing==1,NA,y2))
 fakedatam<- select(fakedatam,-missing,-error)

#now we can impute this missing data.  I default to running my imputation in Stata, but R does have several
#packages for performing multiple imputation
#mice is one package that has a lot of functionality, mice defaults to predictive mean matching
imp1<-mice(fakedatam, m=10, id=id)

#the mice package offers several diagnostics for evaluating the structure of missing data and the quality
#of the imputation
#since we made up this data, I'd like to compare the distribution of imputed values to the "truth"
#we can do this for each of our imputed data sets, I'll show this for one data set as an example
d1<-complete(imp1, 1)
newcolnames<-paste("i",colnames(d1), sep="")
colnames(d1)<- newcolnames
d1$id<-d1$iid
merged<-merge(d1, fakedata, by="id")
dt<-density(merged$y[merged$missing==1])
di<-density(merged$iy[merged$missing==1])
plot(dt, ylim=c(0,1))
lines(di, col="red")
dt2<-density(merged$y2)
di2<-density(merged$iy2)
plot(dt2, ylim=c(0,1))
lines(di2, col="red")


#mice also offers options for running analyses with multiply imputed data
fitm <- with(imp1, lm(y ~ x1 + x2 + as.factor(x3)+x4))
#and for pooling inferences using rubin's rules
fitml<- summary(pool(fitm))

#now let's take a look at the output from this regression
#we can clean up the details of the regression output using the colname and rowname commands,
#just a bit of starting cleanup
colnames(fitml)<-c("Estimates", "Standard Errors", "T-Statistic","Degrees of Freedom", "P-Value")
rownames(fitml)<- c("Intercept", "Predictor 1", "Predictor 2", "Predictor 3--Level 1 \n (0 as Base)","Predictor 3--Level 2 \n (0 as Base)", "Predictor 3--Level 3 \n (0 as Base)","Predictor 4")
#now let's look at the formatted table, there are a couple of ways to export the data, here's an example of moving
#the data into LaTeX via the xtable package
table1<- xtable(fitml)
print(table1, file = "table1.tex")
#for non TeX users, tables can also be exported using just base R
#we can write a csv that actually doesn't look half bad
write.csv(fitml, file="table1.csv")


#mice also offers options for running analyses with multiply imputed data
fitm <- with(imp1, lm(y ~ x1 + x2 + x3+x4))
#and for pooling inferences using rubin's rules
fitml<- summary(pool(fitm))


#now let's switch gears.  Sometimes you'll need to run pieces of code frequently.  Functions can help streamline
#that process, here's a function that adds a prefix to all variables in a dataset
# a function sometimes includes arguments that get referenced in the function, in this case we have a dataset
#and a character function which will get called in this function
#functions don't have to have these arguments defined and in some cases a function can allow an argument value 
#to be missing
addprefix<- function(data,prefix){
  tnames<-colnames(data)
  nnames<-paste(prefix,tnames,sep="")
  colnames(data)<-nnames
}
#let's run this function and see what happens
addprefix(data=fakedata,prefix="n")
#our syntax is right, but this function doesn't seem to do anything...In order for this function to take effect
#we have to have it return the output we are interested in
addprefix<- function(data,prefix){
  
  tnames<-colnames(data)
  nnames<-paste(prefix,tnames,sep="")
  colnames(data)<-nnames
  print(paste("All Variables Renamed with Prefix", prefix, sep=" "))
  return(data)
  
}
#now the addprefix function is working properly
addprefix(data=fakedata,prefix="n")


#functions can streamline coding, but they also can sometimes mask what R is doing and the functions may be
#performing tasks not intended
#let's make a new function called addsuffix to add a suffix to variables in a dataset
addsuffix<- function(data,suffix){
  
  tnames<-colnames(data)
  nnames<-paste(tnames,suffix,sep="")
  colnames(data)<-nnames
  print(paste("All Variables Renamed with Suffix", suffix, sep=" "))
  data<-sample_n(data, 5)
  return(data)
  
}
addsuffix(data=fakedata,suffix="n")
#given the structure of this function, it may not be as clear what this function does, to find more information
#about this function or any user, base, or package provided function in R, we can use the getAnywhere function
getAnywhere("addsuffix")
getAnywhere("getAnywhere")
#now let's find out more information about the imputation package we just ran
getAnywhere("mice")