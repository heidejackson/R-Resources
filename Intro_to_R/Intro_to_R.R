
rm(list = ls())

getwd()

set.seed(10012019)

setwd("C:/Users/heidej/Documents/")

dir.create("Intro_to_R")

setwd("Intro_to_R")

# install.packages("packrat")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("gapminder")
# install.packages("haven")


library("dplyr")
library("readr")
library("gapminder")
library("haven")
library("tibble")
library("packrat")

# packrat::init("~/Intro_to_R")
# packrat::snapshot()
?readr
help(readr)

gmcsv<- read_csv("gapmindermiss.csv")
gmcsv2=read_csv("gapmindermiss.csv")


gmcsvb<- read.csv("gapmindermiss.csv")

class(gmcsv)
class(gmcsvb)

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

quantile(gmcsv$lifeExp, c(.02, .95), na.rm=TRUE)


q<- quantile(gmcsv$lifeExp, c(.02, .95), na.rm=TRUE)



exceptional<- filter(gmcsv, lifeExp<=q[1] | lifeExp>=q[2])

exceptional2<- gmcsv[(lifeExp<=q[1] | lifeExp>=q[2]),]

exceptional3<- gmcsv[(lifeExp<=q[1] | lifeExp>=q[2]) & is.na(lifeExp)==FALSE,]



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

identical(gmcsvm$dummy, gmcsvm2$dummy2, gmcsv$dummy3)

gmcsvm %>% filter(year==2007)
gmcsvm %>% filter(year!=2007)

gmcsvm[gmcsvm$year==2007,]
gmcsvm[gmcsvm$year!=2007,]


table(gmcsvm$dummy)
table(gmcsvm$dummy[gmcsvm$year==2007])
table(gmcsvm$dummy[gmcsvm$year!=2007])

prop.table(table(gmcsvm$dummy))



t.test(gmcsvm$lifeExp, gmcsvm$dummy)

?t.test
t.test(gmcsvm$lifeExp~gmcsvm$dummy)

lm(lifeExp~gdpPercap, data=gmcsvm)

glm(lifeExp~gdpPercap, data=gmcsvm)

glm(lifeExp~gdpPercap, data=gmcsvm, subset=dummy==1)

glm(lifeExp~gdpPercap+country, data=gmcsvm, subset=dummy==1)


m1<- lm(lifeExp~gdpPercap, data=gmcsvm)
m1 
m<- summary(m1)
out<- coef(m)
write.csv(out, "regressionresults.csv")

rownames(out)<- c("Intercept", "GDP per Capita")
colnames(out)<- c("Coefficients", "Standard Error", "T Statistic", "P Value")
write.csv(out, "regressionresults.csv")

plot(m1)
objects(m1)


save.image("fulldata.RData")

save(gmcsvm, file="gmcsvm.Rda")
rm(list = ls())
load("fulldata.RData")
rm(list = ls())
load("gmcsvm.Rda")


#earlier I mentioned that we were loading a variant of the gapminder dataset
# the data set we loaded in differs from the original gapminder in that i set a subset of observations
#to have missing life expectancy data using the code below
#creating the missing dataset
#missing<-rbinom(1704, 1, .1)
#gapminder2<-add_column(gapminder, missing)
#gapmindermiss<-mutate(gapminder2, lifeExp=ifelse(missing==0, lifeExp, NA))

#I wrote this updated dataset to the various formats we loaded earlier in class
#write_csv(gapmindermiss, "gapmindermiss.csv")
#write_dta(gapmindermiss, "gapmindermiss.dta", version=15)
#write_sas(gapmindermiss, "gapmindermiss.sas7bdat")
                      

