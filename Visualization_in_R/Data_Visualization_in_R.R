#clear out any objects stored in R
rm(list = ls())
#current location
getwd()

#set directory to a default location
setwd("C:/Users/mprc/Documents/")
#create a subdirectory for this class materials, titled Data_Visualization_in_R
dir.create("Data_Visualization_in_R")
#now we are going to work from that location we just created 
setwd("Data_Visualization_in_R")

# R is a flexible program based on the S programming language.  By default base R can do many things
#However, R's functionality is greatly enhanced by the use of packages.  As of August 2019, there
# are over 10,000 R packages
# Packages generally contain functions that enhance R's functionality.  Using a package requires two
#steps.
#First, the package needs to be installed, for this course we are only going to need a few packages
#install.packages("packrat")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("gapminder")
#install.packages("haven")
#install.packages("extrafont")
#install.packages("RColorBrewer")
#install.packages("survival")
#install.packages("car")
#packrat::init("~/Data_Visualization_in_R")

#next, we are going to load the package, this is accomplished through the library function
library("ggplot2") 
library("readr")
library("gapminder")
library("haven")
library("tibble")
library("splines")
library("dplyr")
library("grDevices")
library("extrafont")
library("RColorBrewer")
library("car")
#font_import()
#today we are going to introduce a couple of different options for graphing using base R and ggplot2

#as an example, we are going to use the gapminder data set gapminder shows life expectancy over time across countries

#let's say at first we are interested in understanding trends in average life expectancy over time, how could we do it?
#using base R we could create a data set that contains the average life expectancy 
yearlevel<-gapminder %>%
  group_by(year) %>%
  summarise_at(vars(lifeExp), funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)))
plot(yearlevel$year, yearlevel$mean)
#that's a start, but let's add a line, the type function can help us do that, specifying type=b shows both points
#and a line.  If we specified type=l, it would just show a line graph
plot(yearlevel$year, yearlevel$mean, type="b")
#now we can customize our settings to make this a little prettier
#what are some key settings that we are modifying here, xlab, ylab and main control the labels for the x axis,
#y axis, and main chart respectively. In this chart I also show an example for how we can have a multiple line label
#the bty function controls the lines surrounding the graph for line graphs i often use an L shaped border
#family specifies the font type used in the graph, the extrafont function allows you to customize this further
#color specifies the color of the point or line created
#pch specifies the character of the point, this can handle all kinds of crazy customizations
#lwd also gives us the ability to control the width of lines in our line graph
#lty gives us the option to control our line type
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="darkgreen", pch=2, lwd=4)
#this next graph shows the pch with a letter instead of a type and a lty of 2 (a dashed line)
#this blog post shows some of the options https://www.statmethods.net/advgraphs/parameters.html
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col="darkgreen", pch="l", lty=2)

#the xlim and ylim functions give you the ability to select the range of values plotted
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col="darkgreen", pch=2, xlim=c(1980,2000), ylim=c(0,100))
#R also has a cool feature that makes it easy to specify custom colors.  Let's look at the same graph but in terrapin gold and with a bit of clean up

tred<-rgb(224,58,62, max = 255)
tgold<-rgb(255,213,32, max = 255)
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col=tred, pch=2, xlim=c(1980,2000), ylim=c(0,100))

#We can also add objects onto this plot, some examples, but first, let's modify our test dataset slightly
yearlevelc<-gapminder %>%
  group_by(year, continent) %>%
  summarise_at(vars(lifeExp), funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)))

#this first plot shows overall life expectancy
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col=tred, pch=2, xlim=c(1980,2000), ylim=c(0,100))
#now let's show life expectancy overall, for the continent of Europe, and for a made up hypothetical case
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col=tred, xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b", lwd=2)
points(1990, 100, pch=8, cex=4, col="blue")
legend("topleft", c("Overall Life Expectancy", "Life Expectancy in Europe", "Exceptional Case"), pch=c(NA,1, 8), lty=c(1,1,NA), col=c("red", "black", "blue"), bty="n")
#one thing to note, any added lines or points added to the initial graph inherit the range set by plot so if we were to add
points(1970, 50) #won't show up on the already plotted graph

#as you can see, even base R graphing functionality offers a lot of flexibility and will generate what I think is a pretty enough graph
# the syntax for other types of graphs is very similar
#let's say you want to do a bar graph showing this information, the process is very similar, 
#we can use the same syntax for labeling our x and y axes and barchart, the main difference,
#we make this a barplot and we add a name function to label each bar with the corresponding year
barplot(yearlevel$mean,  xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", family="serif", col="darkgreen", names=factor(yearlevel$year))

# the process for creating bar graphs with error bars is a bit more tedious, this is one place where ggplot can be very useful.  Here's how you could do this just using base R
t<- barplot(yearlevel$mean,  xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", family="serif", col="darkgreen", names=factor(yearlevel$year), cex.names=.5, ylim=c(0,100))
segments(t,yearlevel$mean-yearlevel$sd,t,yearlevel$mean+yearlevel$sd)
arrows(t, yearlevel$mean - yearlevel$sd ,t,
       yearlevel$mean + yearlevel$sd, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

#here's an example of a grouped bar plot
barplot(yearlevelc$mean,  xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", family="serif", col=c("red", "orange", "yellow", "green","blue"), names=factor(yearlevelc$year), cex.names=.5, ylim=c(0,100))
legend("topleft",fill=c("red", "orange", "yellow", "green","blue"), c("Africa", "Americas", "Asia", "Europe", "Oceania"), bty="n")

#what if we are interested in looking at the distribution of life expectancy over time?  Let's look at this via a series of boxplots 
boxplot(gapminder$lifeExp~gapminder$year, col=tgold, xlab="Year", ylab="Life Expectancy in Years \n (At Birth)")

#finally if we just want to look at the distribution of life expectancies we can just use the histogram function
hist(gapminder$lifeExp, col=tred, family="mono")

#alternatively we can look at the distribution of life expectancy via the density function
d<-density(gapminder$lifeExp)
plot(d, main="Distribution of Life Expectancy \n At Birth")


#now let's give an example of saving graphs
# the first line specifies the type of image to be saved and the image saved
#R gives the option to save many types of images.  I often save things as a png or a pdf if I'm working in TeX
png("firstpass.png")
# the par command controls aspects of the window graphed, in this case  the mar option specifies wider margins
#in order to accomodate the multi line label we made earlier
par(mar = c(5, 5, 5, 5))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
#the dev.off command ends the image being graphed
dev.off()
#here's the same graph but saved as a pdf
pdf("firstpass.pdf", family="Garamond")
par(mar = c(5, 5, 5, 5))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
dev.off()

#by default the png image we made earlier isn't high resolution and for a lot of applications it really doesn't need to be
#however, if you need a high resolution image for a journal submission or a poster, R allows you to customize the image size and quality
#for high resolution images, I'll sometimes use tiff files
#eps is the preferred format for very large images
#note when making custom images for posters, other customization is sometimes needed, sometimes you may need to 
#do things like adjust the margins and the font size (via the cex options) depending on the image type selected

tiff("firstpass.tiff",width = 8, height = 8, pointsize = 1/300, units = 'in', res = 300)
par(mar = c(5, 5, 5, 5))
#now let's show life expectancy overall, for the continent of Europe, and for a made up hypothetical case
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
legend("topleft", c("Overall Life Expectancy", "Life Expectancy in Europe", "Exceptional Case"), pch=c(NA,1, 8), lty=c(1,1,NA), col=c("red", "black", "blue"), bty="n")
#the dev.off command ends the image being graphed
dev.off()

#some fonts are also not supported depending on the image type.  If we were to attempt to use a serif font and make an eps file
#R would throw an error

postscript("firstpass.eps",width = 8, height = 8, pointsize = 1/300)
par(mar = c(5, 5, 5, 5))
#now let's show life expectancy overall, for the continent of Europe, and for a made up hypothetical case
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
legend("topleft", c("Overall Life Expectancy", "Life Expectancy in Europe", "Exceptional Case"), pch=c(NA,1, 8), lty=c(1,1,NA), col=c("red", "black", "blue"), bty="n")
#the dev.off command ends the image being graphed
dev.off()


#now let's look at how some of this same functionality might play out in ggplot2 
#ggplot2 offers a lot more customizability than basic R graphs it is based on an underlying grammar of graphics
#in general ggplot graphs start with a basic data statement and an aesthetic statement (aes) which specifies the variables to be graphed
#the aes statement offers a number of options such as fill and group statements to allow more complex images
#we save this inital graph structure as an object, b, and call it and see....
b<- ggplot(data=yearlevel, aes(x=year, y=mean))
b #an empty graph, that's helpful
# now let's add some objects to the graphs, one of the nice features of ggplot is that it retains this essential
#structure of the graph, while looking at different ways of visualizing this underlying data
b+geom_point()
#specifying error bars and confidence errors is substantially easier in ggplot2 versus base R
b+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
#ggplot also has it's own options for making pretty graphs
b+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))+ylab("Life Expectancy in Years")+xlab("Year")+ggtitle("Life Expectancy in GGPLOT")+ theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
#as a style preference, i often build my plots item by item when adding ggplot, so my final code will look like this:
b+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))+
  ylab("Life Expectancy in Years")+
  xlab("Year")+ #similar to base R, we can use xlab and ylab to set x and y axis labels
  scale_x_continuous(limits=c(1900,2010), breaks=seq(1900,2000,by=10))+ #this is kind of like the xlim function we saw earlier but has different functionality
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,by=10))+ #again similar to y function, one option not shown here, is the option to customize our labels
  ggtitle("Life Expectancy in GGPLOT")+ 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) #in ggplot a lot of the graph's features can be customized through the use of various themes

#now let's look at some bar graphs
#start by specifying the underlying data and structured to be graphed
#now we are specifying that this will be a bar graph by continent
gf<- ggplot(data=yearlevelc, aes(x=year, y=mean, fill=continent))
  #one of the differences between ggplot and plot is that ggplot allows for statistical transformations of the
#data during the process of graphing.  this is actually one of the features of ggplot I really don't care for
# I like to know my data and know exactly what I am graphing prior to making the graph
#in this case, with the stat="identity" option we are telling ggplot to graph the numbers as provided from 
#the initial data set(these are the average life expectancies by continent).  if we were to omit this option
#the function would throw us an error
#the position=dodge option tells us that we want groups to be placed side by side, by default, ggplot will show 
# a stacked graph
  gf+geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=mean-sd*.25, ymax=mean+sd*.25), position="dodge")+
  xlab("Year")+
  ylab("Life Expectancy")+
  ggtitle("Life Expectancy in GGPLOT")+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme_bw()+
    #themes also give us the option to specify our legends and legend labels
  theme(legend.position="left")+labs(fill = "Continent")+
  scale_fill_manual(values=c(tred, tgold, "black", "lightgray", "darkgray"))
  
#here's the same thing but a stacked graph
  gf+geom_bar(stat="identity")+
    xlab("Year")+
    ylab("Life Expectancy")+
    ggtitle("Life Expectancy in GGPLOT")+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
    theme_bw()+
    theme(legend.position="left")+labs(fill = "Continent")+
    scale_fill_manual(values=c(tred, tgold, "black", "lightgray", "darkgray"))
  
  
  
  #ggplot also has some nice default gray scales for graphs
  gf+geom_bar(stat="identity")+
    xlab("Year")+
    ylab("Life Expectancy")+
    ggtitle("Life Expectancy in GGPLOT")+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
    theme_bw()+
    theme(legend.position="left")+labs(fill = "Continent")+
    scale_fill_grey(start = 0.8, end = 0.2) 
  
#finally a quick look at boxplots in ggplot
  bf<- ggplot(data=gapminder, aes(x=year, y=lifeExp))
  #the group function tells us to show the distribution by year, if we omitted this, we'd just get the total distribution
  # of life expectancy
bf+geom_boxplot(aes(group=year))


#we can also use the facet command to show separate graphs for particular groups
bf+geom_boxplot(aes(group=year))+facet_grid(continent ~.)
bf+geom_boxplot(aes(group=year))+facet_grid(.~ continent)
#we can even look at distributions for particular combinations of variables in this case 

# we would see the distribution of life expectancies within a particular continent for a particular year
bf+geom_boxplot()+facet_grid(year~ continent)
#ggplot has it's own save system, to use it we save one of our plots as an object
saveme<- bf+
  geom_boxplot(aes(group=year))+
  facet_grid(.~ continent)+
  scale_x_continuous(limits=c(1950,2012), breaks=seq(1950,2010, by=10))

#now let's save, this gives much of the same functionality we saw earlier but can sometimes be a bit nearer
ggsave("ggsave.pdf", saveme)


#up until now we've talked about using R graphs in the context of showing results; however, we can also easily
#call them when assessing model fit

fit1<- lm(lifeExp~gdpPercap, data=gapminder)
plot(fit1)  #ouch that is a poorly specified model, residuals do not appear to be random
qqPlot(fit1)
fit2<-lm(lifeExp~log(gdpPercap), data=gapminder)
plot(fit2)
qqPlot(fit2)


