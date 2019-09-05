
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

set.seed(12722017)
id<-seq(1:100)
seq(1,50, by=7)
error<-rnorm(n=100, mean=0, sd=1)
x1<-rnorm(n=100, mean=0, sd=1)
x2<-rbinom(n=100, 1, .5)
missing<-rbinom(n=100,1,.3)
x3<-sample(x=0:3, size=100, replace=TRUE, prob=c(.2,.1,.4,.3))
sample(x=10000:99999, size=100, replace=FALSE)
fakedata<-tibble(x1,x2,x3,error,id,missing)
fakedata$x4<-rbinom(100,1,ifelse(fakedata$missing==1,.7,.3))
fakedata$y<-.5*fakedata$x1+-.1*fakedata$x2+error+.2*fakedata$missing+-.5*fakedata$x4
fakedata$y2<-.5*fakedata$x1+-.1*fakedata$x2+error

fakedatam<-mutate(fakedata,y=ifelse(missing==1,NA,y), y2=ifelse(missing==1,NA,y2))
fakedatam<- select(fakedatam,-missing,-error)

imp1<-mice(fakedatam, m=10, id=id)

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


fitm <- with(imp1, lm(y ~ x1 + x2 + as.factor(x3)+x4))
fitml<- summary(pool(fitm))


colnames(fitml)<-c("Estimates", "Standard Errors", "T-Statistic","Degrees of Freedom", "P-Value")
rownames(fitml)<- c("Intercept", "Predictor 1", "Predictor 2", "Predictor 3--Level 1 \n (0 as Base)","Predictor 3--Level 2 \n (0 as Base)", "Predictor 3--Level 3 \n (0 as Base)","Predictor 4")
table1<- xtable(fitml)
print(table1, file = "table1.tex")
write.csv(fitml, file="table1.csv")


fitm <- with(imp1, lm(y ~ x1 + x2 + x3+x4))
fitml<- summary(pool(fitm))


addprefix<- function(data,prefix){
  tnames<-colnames(data)
  nnames<-paste(prefix,tnames,sep="")
  colnames(data)<-nnames
}

addprefix(data=fakedata,prefix="n")
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

getAnywhere("addsuffix")
getAnywhere("getAnywhere")
getAnywhere("mice")