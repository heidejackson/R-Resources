#remove objects from our session
rm(list = ls())
# get current working directory
getwd()
# set random seed, very important for this class
set.seed(101419)
#set directory
setwd("C:/Users/heidej/Documents/")
#create new directory and put workflow in it
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
#library(stargazer)
#library(rmarkdown)

#R is great for simulating fake data here are some useful functions for doing this

# generat a sequence of ids
id<-seq(1:100)
#this can also be generated in some specified interval
seq(1,50, by=7)
#generate variables that have a normal distribution, 100 cases, mean 0 standard deviation of 1
error<-rnorm(n=100, mean=0, sd=1)
x1<-rnorm(n=100, mean=0, sd=1)
#generate a variable that has a binomial distribution, 100 cases, probability of selection .5
x2<-rbinom(n=100, 1, .5)
#same as above but probability of success .3
missing<-rbinom(n=100,1,.3)
# we can also do this for categorical variables
x3<-sample(x=0:3, size=100, replace=TRUE, prob=c(.2,.1,.4,.3))
#finally R let's us take a sample of a series of numbers, we can set this up to sample with or without replacement
sample(x=10000:99999, size=100, replace=FALSE)
# I am going to wrap up some of the variables we just made into a dataset
fakedata<-data.frame(x1,x2,x3,error,id,missing)
#we can continue to add variables to this data set
fakedata$x4<-rbinom(100,1,ifelse(fakedata$missing==1,.7,.3))
#now let's specify our outcome y as a function of random error and some of these predictors
fakedata$y<-.5*fakedata$x1+-.1*fakedata$x2+error+.2*fakedata$missing+-.5*fakedata$x4
#and let's specify a y2 where a subset of data is missing
fakedata$y2<- ifelse(missing==1,NA,fakedata$y)


#now let's see how linear regression estimates compare with the expected values we just created
m1<-summary(lm(y~x1+x2+as.factor(x3)+x4+missing, data=fakedata))
#and a regression run for the outcome with missing data
m2<- summary(lm(y2~x1+x2+as.factor(x3)+x4, data=fakedata))
#add some colnames
colnames(m1$coefficients)<-c("Estimates", "Standard Errors", "T-Statistic","P-Value")
#add rownames
rownames(m1$coefficients)<- c("Intercept", "Predictor 1", "Predictor 2", "Predictor 3--Level 1 \n (0 as Base)","Predictor 3--Level 2 \n (0 as Base)", "Predictor 3--Level 3 \n (0 as Base)","Predictor 4", "Missing Flag")
#sent this to a latex table
table1<- xtable(m1)
#make the table
print(table1, file = "table1.tex")
#alternative version saves this as a csv
write.csv(m1$coefficients, file="table1.csv")


#an example using stargazer which I think is prettier but not as easy to export
#m1a<-lm(y~x1+x2+as.factor(x3)+x4+missing, data=fakedata)
#m2a<- lm(y2~x1+x2+as.factor(x3)+x4, data=fakedata)

#write.table(stargazer(m1a, m2a, title="Regression Results", type="latex", align=TRUE, dep.var.labels=c("Y True","Y with Missingness"),covariate.labels=c("X1","X2","X3-1","X3-2","X3-3"),omit.stat=c("LL","ser","f"), no.space=TRUE), file="test.tex",  row.names = FALSE)


# we can simplify the creation of the regression model and output via the use of a function, let's see how
modelobject<-y~x1+x2+as.factor(x3)+x4+missing
colnamesf<- c("Estimates", "Standard Errors", "T-Statistic","P-Value")
rownamesf<- c("Intercept", "Predictor 1", "Predictor 2", "Predictor 3--Level 1 \n (0 as Base)","Predictor 3--Level 2 \n (0 as Base)", "Predictor 3--Level 3 \n (0 as Base)","Predictor 4", "Missing Flag")

#here's what this function might look like
myregfunction<- function(data, model, colnamesn, rownamesn, filename){
  #create an object that reads from the specified model and looks at the specified data set
  object<-summary(lm(model, data=data))
#let's say we want our function to warn us if we are about to do something that doesn't make sense,
# we can program this with some conditional logic and an if statement
  if (ncol(object$coefficients)!=length(colnamesn)){
    print("Warning Invalid Colnames Specified")}
  else {
    #assigns colnames
  colnames(object$coefficients)<-colnamesn
  }
  if (nrow(object$coefficients)!=length(rownamesn)){
    print("Warning Invalid Rownames Specified")}
  else{
  rownames(object$coefficients)<-rownamesn
  }
  write.csv(object$coefficients, file=filename)
  }
#setting up running a function I know to have the wrong number of row and colnames
myregfunction(fakedata, modelobject, colnamesf[1:3], rownamesf[1:3], "testoutputbad.csv")

#now setting a function where I know the colnames and rownames is correct
myregfunction(fakedata, modelobject, colnamesf, rownamesf, "testoutputgood.csv")


#functions can also be used for data manipulation--say we perhaps wanted to do a power calculation
#and examine how inferences change depending on our sample size, this function creates the data
#we made above but varies the sample
myfakedatafunction<-function(samplesize){
  id<-seq(1:length(samplesize))
error<-rnorm(n=samplesize, mean=0, sd=1)
x1<-rnorm(n=samplesize, mean=0, sd=1)
x2<-rbinom(n=samplesize, 1, .5)
missing<-rbinom(n=samplesize,1,.3)
x3<-sample(x=0:3, size=samplesize, replace=TRUE, prob=c(.2,.1,.4,.3))
fakedata<-data.frame(x1,x2,x3,error,id,missing)
fakedata$x4<-rbinom(samplesize,1,ifelse(fakedata$missing==1,.7,.3))
fakedata$y<-.5*fakedata$x1+-.1*fakedata$x2+error+.2*fakedata$missing+-.5*fakedata$x4
fakedata$y2<- ifelse(missing==1,NA,fakedata$y)
#note in the context of functions return is important, without it, this function would run but 
#nothing would be stored
return(fakedata)
}

#let's show this function would perform in the context of a loop

# i want to run the fake data simulation across these sample sizes
ns<- c(50,100,150,200, 250, 300, 400)
# start with a loop that runs from the first object in my ns through the length of it
for (i in 1:length(ns)){
#create an object a that contains the value fd"somenumber"
a<- paste("fd",ns[i], sep="")
# the assign function is also very helpful here, assign create an object that takes on the value of a
#and gives it the value of the myfakedatafunction
assign(a, myfakedatafunction(ns[i]))
}
#functions can also be called in other functions
myregfunction(myfakedatafunction(1000), modelobject, colnamesf, rownamesf, "testoutput1000.csv")


#sometimes other little toy functions can be helpful for various data cleaning tasks
#here's an example of adding a prefix
addprefix<- function(data,prefix){
  
  tnames<-colnames(data)
  nnames<-paste(prefix,tnames,sep="")
  colnames(data)<-nnames
  print(paste("All Variables Renamed with Prefix", prefix, sep=" "))
  return(data)
  
}
addprefix(data=fakedata,prefix="n")

#but be careful when you inherit a function, sometime a function's description and error messages
#may not correspond to what the function actually does
addsuffix<- function(data,suffix){
  
  tnames<-colnames(data)
  nnames<-paste(tnames,suffix,sep="")
  colnames(data)<-nnames
  print(paste("All Variables Renamed with Suffix", suffix, sep=" "))
  data<-sample_n(data, 5)
  return(data)
  
}
addsuffix(data=fakedata,suffix="n")

#last but not least if we want to load in functions from other scripts, we can do this with a source statement
#source("myfunction.R")
