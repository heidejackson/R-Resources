
rm(list = ls())

getwd()

setwd("C:/Users/mprc/Documents/")

dir.create("Intro_to_R")

setwd("Intro_to_R")

#RUN FIRST TIME ONLY
# install.packages("Rtools")
# install.packages("packrat")
# install.packages("tidyverse")
# 
# 
# packrat::init("~/Intro_to_R")
# packrat::snapshot()

library("dplyr")
library("readr")
library("gapminder")
library("haven")
library("tibble")

?readr
gmcsv<- read_csv("gapmindermiss.csv")

gmcsv2=read_csv("gapmindermiss.csv")


gmdta<- read_dta("gapmindermiss.dta")
gmsas<-read_sas("gapmindermiss.sas7bdat")

gmcsv 
view(gmcsv) 
summary(gmcsv)


summary(gmcsv$lifeExp) 
summary(gmcsv[,4])


lifeexp<-gmcsv$lifeExp
lifeExp<-gmcsv$lifeExp

mean(gmcsv$lifeExp)

mean(lifeExp, na.rm=TRUE)





gmcsvm<- mutate(gmcsv, dummy=ifelse(year==2007, 1,0))


dummy2<-ifelse(gmcsv$year==2007, 1,0)
gmcsvm2<- cbind(gmcsv, dummy2)


dummy3<-NA
for(i in 1:length(gmcsv$year)) {
if (gmcsv$year[i]==2007) {
  dummy3[i]<-1
} else {
  dummy3[i]<-0
}
}

gmcsv$dummy3<-dummy3


identical(gmcsvm$dummy, gmcsvm2$dummy2, dummy3)

gmcsvm %>% filter(year==2007)
gmcsvm %>% filter(year!=2007)


gmcsvm[gmcsvm$year==2007,]
gmcsvm[gmcsvm$year!=2007,]



table(gmcsvm$dummy)
table(gmcsvm$dummy[gmcsvm$year==2007])
table(gmcsvm$dummy[gmcsvm$year!=2007])


lm(lifeExp~gdpPercap, data=gmcsvm)
glm(lifeExp~gdpPercap, data=gmcsvm)


m1<- lm(lifeExp~gdpPercap, data=gmcsvm)

summary(m1)
plot(m1)

objects(m1)



save.image("fulldata.RData")


save(gmcsvm, file="gmcsvm.Rda")
rm(list = ls())

load("fulldata.RData")
rm(list = ls())

load("gmcsvm.Rda")



missing<-rbinom(1704, 1, .1)
gapminder2<-add_column(gapminder, missing)
gapmindermiss<-mutate(gapminder2, lifeExp=ifelse(missing==0, lifeExp, NA))

write_csv(gapmindermiss, "gapmindermiss.csv")
write_dta(gapmindermiss, "gapmindermiss.dta", version=15)
write_sas(gapmindermiss, "gapmindermiss.sas7bdat")


