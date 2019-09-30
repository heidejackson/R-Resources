#clear out any objects stored in R
rm(list = ls())
#current location
getwd()

#set directory to a default location
setwd("C:/Users/heidej/Documents/")
#create a subdirectory for this class materials, titled intro to R
dir.create("Intro_to_R")
#now we are going to work from that location we just created intro to R
setwd("Intro_to_R")

# R is a flexible program based on the S programming language.  By default base R can do many things
#However, R's functionality is greatly enhanced by the use of packages.  As of August 2019, there
# are over 10,000 R packages
# Packages generally contain functions that enhance R's functionality.  Using a package requires two
#steps.
#First, the package needs to be installed, for this introduction, we are going to install a few
#packages
# install.packages("packrat")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("gapminder")
# install.packages("haven")


#next, we are going to load the package, this is accomplished through the library function
library("dplyr")
library("readr")
library("gapminder")
library("haven")
library("tibble")
library("packrat")

# packrat::init("~/Intro_to_R")
# packrat::snapshot()
#what do these packages do?  We can find out in a couple of ways
?readr
help(readr)
# the '?' and 'help()' accomplish the same thing
#if we are looking for specific functions that might be referenced in a package, we can also use
#'??'

#Now let's read in a version of the gapminder data set. Gapminder is a data set that shows
#life expectancy by gdp by country

#R can read in datasets from a variety of formats.  In this code, I am using readr to read in a 
#comma separated file and haven to read in a version of gapminder from sas or stata

# as an introduction to R syntax, running this line, uses the function read_csv() to load in 
# a dataset into the R environment.  We are calling this dataset gmcsv
gmcsv<- read_csv("gapmindermiss.csv")
# we can also define an object in R by using '='.  This yields the same thing as the <- syntax
gmcsv2=read_csv("gapmindermiss.csv")

#this method of loading in data requires the readr package.  We can read in csv files using base 
#R, here's an example

gmcsvb<- read.csv("gapmindermiss.csv")
#what's the difference between this and the read_csv function?  Well in practice they are very similar
#however, read_csv provides more information on potential errors on what is being loaded and saves 
#data by default as a tibble, a special type of data frame, whereas read.csv saves data as a data.frame
# we can see this by using something called a class function
class(gmcsv)
class(gmcsvb)

#the same dataset can be read in from stata or sas via the read_dta and read_sas functions respectively
gmdta<- read_dta("gapmindermiss.dta")
gmsas<-read_sas("gapmindermiss.sas7bdat")

#how do we see the contents of this data set?  Let us count the ways
#1 
gmcsv  #this will show us an object called a tibble with dimensions 1,704x7 (1704 rows, 7 columns)
#the columns names are shown on the next line
#then the format of each column is shown.  this data set contains a combination of character 
#and numeric variables
#it then shows the first ten rows of this data set

# you might be wondering why is this data set called a tibble?
# a tibble is just a dataframe with some nice formatting properties that is commonly used by 
#tidyverse packages

#another way to view this dataset
view(gmcsv) # this will let you see the full data set and look at all of it's values
# we can also look at the dataset in the environment mode of R Studio

#finally another useful way of looking at this dataset is the summary function
summary(gmcsv)

#view and summary are both functions, if you are interested in learning more information about them help or ? will give you more information

#the summary function can also provide us more information about particular variables
#let's start by looking at the variable lifeExp which provides the average life expectancy at birth
#for each year for each country

# We can do this in a couple of different ways
summary(gmcsv$lifeExp) 
summary(gmcsv[,4]) #this will give us the summary of the fourth column of gmcsv which in this case is life expectancy


#We could also define an object and call it as follows
lifeexp<-gmcsv$lifeExp
lifeExp<-gmcsv$lifeExp
#to note, if we create the object lifeexp this will be distinct from an object lifeExp.  R is case sensitive

#now let's use a different function for summarizing this variable, from now on, I am just going to reference gmcsv$lifeExp
mean(gmcsv$lifeExp)
#that was unexpected....by default many R functions will return NA (missing) if any value is set to missing.  We can override this by modifying
#the settings of the mean function
mean(lifeExp, na.rm=TRUE)

#that's better and the results from the mean function match what we saw from summary.

#let's say we care about the extreme ends of the distribution, the quantile function can help us view
#values at particular parts of the distribution
quantile(gmcsv$lifeExp, c(.02, .95), na.rm=TRUE)


#one thing to note, so far we haven't saved our output anywhere, if we want to call these results
#later we can
q<- quantile(gmcsv$lifeExp, c(.02, .95), na.rm=TRUE)


#these results can be useful if we want to look at these exceptional cases in the data.
#here's how

exceptional<- filter(gmcsv, lifeExp<=q[1] | lifeExp>=q[2])

# again there are many ways to do this

#just this won't return exactly the same thing, this is because of the differential handling of missing cases
exceptional2<- gmcsv[(lifeExp<=q[1] | lifeExp>=q[2]),]

#this will give the same thing as the filter command
exceptional3<- gmcsv[(lifeExp<=q[1] | lifeExp>=q[2]) & is.na(lifeExp)==FALSE,]



#now let's say we want to create a dummy variable indicating if the measure if from 2007 or some other year.  
#again there are a number of different ways of doing this.
#The mutate function can help us do this
gmcsvm<- mutate(gmcsv, dummy=ifelse(year==2007, 1,0))


#another option
dummy2<-ifelse(gmcsv$year==2007, 1,0)
gmcsvm2<- cbind(gmcsv, dummy2)

#yet a third option
dummy3<-NA
for(i in 1:length(gmcsv$year)) {
if (gmcsv$year[i]==2007) {
  dummy3[i]<-1
} else {
  dummy3[i]<-0
}
}

gmcsv$dummy3<-dummy3

#did these three options really do the same thing, there's a function for that too.  The identical function
# can tell you if your objects are the same
identical(gmcsvm$dummy, gmcsvm2$dummy2, gmcsv$dummy3)

#now we've confirmed three ways of doing the same thing, but did we do what we intended?
# let's check.  again there are many ways to verify
#let's unpack what this function is doing.  The mutate function is acting on the dataset gmcsv and is creating a dummy variable =1 if the year
#is 2007 and 0 otherwise
#again there are a number of different ways to do this
#one option is to use how can we check if this function worked properly? let's take a look at the dataset if year is equal to 2007
gmcsvm %>% filter(year==2007)
#now if the year isn't equal to 2007
gmcsvm %>% filter(year!=2007)

#another way of accomplishing the same thing
gmcsvm[gmcsvm$year==2007,]
gmcsvm[gmcsvm$year!=2007,]


#ok, that looks right.  We can also do this more formally by using the table function
table(gmcsvm$dummy)
table(gmcsvm$dummy[gmcsvm$year==2007])
table(gmcsvm$dummy[gmcsvm$year!=2007])

# a bit more data review, it is often of interest to see the percent of cases in a table category
#prop.table can help with this
prop.table(table(gmcsvm$dummy))


#does life expectancy in 2007 differ from other years
#let's do a simple t-test

t.test(gmcsvm$lifeExp, gmcsvm$dummy)

#woah look at those results, that doesn't look right

#let's find out more information about the t.test function
?t.test
#ok, it looks like we need to denote that we expect a relationship between life expectancy
#and our dummy variable, let's retry that 
t.test(gmcsvm$lifeExp~gmcsvm$dummy)

#now let's say we want to look at the relationship between gdp and life expectency
#R's linear regression function can get us started
lm(lifeExp~gdpPercap, data=gmcsvm)
#Just like other things we've seen so far.  There are many functions capable of running a linear regression
#By default the glm function will give us the same thing, glm is a useful function to know as a variation of this
# can be used to run several other types of models including logistic regression
#the functionality of glm is similar to proc glm (for folks who have familiarity with sas)
glm(lifeExp~gdpPercap, data=gmcsvm)

#these functions have different options, one I find very useful is the subset function
glm(lifeExp~gdpPercap, data=gmcsvm, subset=dummy==1)

#if we care about this adjusting for country level fixed effects we can do that
glm(lifeExp~gdpPercap+country, data=gmcsvm, subset=dummy==1)

#running the lm function will run a linear regression model where life expectancy is regressed on gdp per capita.   
#this will not save the regression results and by default running this command will just give us the regression coefficents.
#given that we may want to reference these results later, let's save this model as an object, called m1

m1<- lm(lifeExp~gdpPercap, data=gmcsvm)
#m1 contains the results of this regression model, if we just type
m1 # this will give the same output as lm(lifeExp~gdpPercap, data=gmcsvm) 
#to see more, we can use the summary function with the regression output
m<- summary(m1)
out<- coef(m)
write.csv(out, "regressionresults.csv")
#we could make this output a little prettier by modifying the rownames and colnames of the out object

rownames(out)<- c("Intercept", "GDP per Capita")
colnames(out)<- c("Coefficients", "Standard Error", "T Statistic", "P Value")
write.csv(out, "regressionresults.csv")

plot(m1)
#summarizing m1 shows us that the linear regression cotains a lot more information than just the regression
#coefficients seen earlier.  One way to see what's in this data set is to use the objects function
objects(m1)


#now we've covered a fair bit today, let's wrap up

#if we want to save everything we've done today
save.image("fulldata.RData")

#if we just want to save the final dataset we were working from, we could do this as
save(gmcsvm, file="gmcsvm.Rda")
rm(list = ls())
# later on we can load our full session by
load("fulldata.RData")
rm(list = ls())
# or just the final data set by doing
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
                      

