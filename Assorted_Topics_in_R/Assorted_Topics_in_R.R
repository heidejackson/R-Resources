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
#install.packages("xtable")
library(tibble)
library(readr)
library(haven)
library(readr)
library(dplyr)
library(mice)
library(xtable)
#library(stargazer)
#library(Rmarkdown)


id<-seq(1:100)
seq(1,50, by=7)
error<-rnorm(n=100, mean=0, sd=1)
x1<-rnorm(n=100, mean=0, sd=1)
x2<-rbinom(n=100, 1, .5)
missing<-rbinom(n=100,1,.3)
x3<-sample(x=0:3, size=100, replace=TRUE, prob=c(.2,.1,.4,.3))
sample(x=10000:99999, size=100, replace=FALSE)
fakedata<-data.frame(x1,x2,x3,error,id,missing)
fakedata$x4<-rbinom(100,1,ifelse(fakedata$missing==1,.7,.3))
fakedata$y<-.5*fakedata$x1+-.1*fakedata$x2+error+.2*fakedata$missing+-.5*fakedata$x4
fakedata$y2<- ifelse(missing==1,NA,fakedata$y)


m1<-summary(lm(y~x1+x2+as.factor(x3)+x4+missing, data=fakedata))
m2<- summary(lm(y2~x1+x2+as.factor(x3)+x4, data=fakedata))
colnames(m1$coefficients)<-c("Estimates", "Standard Errors", "T-Statistic","P-Value")
rownames(m1$coefficients)<- c("Intercept", "Predictor 1", "Predictor 2", "Predictor 3--Level 1 \n (0 as Base)","Predictor 3--Level 2 \n (0 as Base)", "Predictor 3--Level 3 \n (0 as Base)","Predictor 4", "Missing Flag")
table1<- xtable(m1)
print(table1, file = "table1.tex")
write.csv(m1$coefficients, file="table1.csv")


#m1a<-lm(y~x1+x2+as.factor(x3)+x4+missing, data=fakedata)
#m2a<- lm(y2~x1+x2+as.factor(x3)+x4, data=fakedata)

#write.table(stargazer(m1a, m2a, title="Regression Results", type="latex", align=TRUE, dep.var.labels=c("Y True","Y with Missingness"),covariate.labels=c("X1","X2","X3-1","X3-2","X3-3"),omit.stat=c("LL","ser","f"), no.space=TRUE), file="test.tex",  row.names = FALSE)


modelobject<-y~x1+x2+as.factor(x3)+x4+missing
colnamesf<- c("Estimates", "Standard Errors", "T-Statistic","P-Value")
rownamesf<- c("Intercept", "Predictor 1", "Predictor 2", "Predictor 3--Level 1 \n (0 as Base)","Predictor 3--Level 2 \n (0 as Base)", "Predictor 3--Level 3 \n (0 as Base)","Predictor 4", "Missing Flag")

myregfunction<- function(data, model, colnamesn, rownamesn, filename){
  object<-summary(lm(model, data=data))

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
myregfunction(fakedata, modelobject, colnamesf[1:3], rownamesf[1:3], "testoutputbad.csv")

myregfunction(fakedata, modelobject, colnamesf, rownamesf, "testoutputgood.csv")



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
return(fakedata)
}


ns<- c(50,100,150,200, 250, 300, 400)
for (i in 1:length(ns)){
a<- paste("fd",ns[i], sep="")
assign(a, myfakedatafunction(ns[i]))
}
myregfunction(myfakedatafunction(1000), modelobject, colnamesf, rownamesf, "testoutput1000.csv")


addprefix<- function(data,prefix){
  
  tnames<-colnames(data)
  nnames<-paste(prefix,tnames,sep="")
  colnames(data)<-nnames
  print(paste("All Variables Renamed with Prefix", prefix, sep=" "))
  return(data)
  
}
addprefix(data=fakedata,prefix="n")

addsuffix<- function(data,suffix){
  
  tnames<-colnames(data)
  nnames<-paste(tnames,suffix,sep="")
  colnames(data)<-nnames
  print(paste("All Variables Renamed with Suffix", suffix, sep=" "))
  data<-sample_n(data, 5)
  return(data)
  
}
addsuffix(data=fakedata,suffix="n")

#source("myfunction.R")
