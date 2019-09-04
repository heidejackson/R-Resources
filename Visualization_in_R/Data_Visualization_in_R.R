
rm(list = ls())
getwd()

setwd("C:/Users/mprc/Documents/")
dir.create("Data_Visualization_in_R")
setwd("Data_Visualization_in_R")

#RUN ME ONCE
# install.packages("packrat")
# install.packages("Rtools")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("gapminder")
# install.packages("haven")
# install.packages("extrafont")
# install.packages("RColorBrewer")
# install.packages("survival")
# install.packages("car")
# packrat::init("~/Data_Visualization_in_R")

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
packrat::snapshot()
#font_import()

yearlevel<-gapminder %>%
  group_by(year) %>%
  summarise_at(vars(lifeExp), funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)))
plot(yearlevel$year, yearlevel$mean)
plot(yearlevel$year, yearlevel$mean, type="b")
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="darkgreen", pch=2, lwd=4)
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col="darkgreen", pch="l", lty=2)
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col="darkgreen", pch=2, xlim=c(1980,2000), ylim=c(0,100))

tred<-rgb(224,58,62, max = 255)
tgold<-rgb(255,213,32, max = 255)
plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col=tred, pch=2, xlim=c(1980,2000), ylim=c(0,100))

yearlevelc<-gapminder %>%
  group_by(year, continent) %>%
  summarise_at(vars(lifeExp), funs(mean(., na.rm=TRUE), sd(.,na.rm=TRUE)))

plot(yearlevel$year, yearlevel$mean, type="b", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col=tred, pch=2, xlim=c(1980,2000), ylim=c(0,100))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col=tred, xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b", lwd=2)
points(1990, 100, pch=8, cex=4, col="blue")
legend("topleft", c("Overall Life Expectancy", "Life Expectancy in Europe", "Exceptional Case"), pch=c(NA,1, 8), lty=c(1,1,NA), col=c("red", "black", "blue"), bty="n")
points(1970, 50) 

barplot(yearlevel$mean,  xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", family="serif", col="darkgreen", names=factor(yearlevel$year))

t<- barplot(yearlevel$mean,  xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", family="serif", col="darkgreen", names=factor(yearlevel$year), cex.names=.5, ylim=c(0,100))
segments(t,yearlevel$mean-yearlevel$sd,t,yearlevel$mean+yearlevel$sd)
arrows(t, yearlevel$mean - yearlevel$sd ,t,
       yearlevel$mean + yearlevel$sd, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

barplot(yearlevelc$mean,  xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", family="serif", col=c("red", "orange", "yellow", "green","blue"), names=factor(yearlevelc$year), cex.names=.5, ylim=c(0,100))
legend("topleft",fill=c("red", "orange", "yellow", "green","blue"), c("Africa", "Americas", "Asia", "Europe", "Oceania"), bty="n")

boxplot(gapminder$lifeExp~gapminder$year, col=tgold, xlab="Year", ylab="Life Expectancy in Years \n (At Birth)")

hist(gapminder$lifeExp, col=tred, family="mono")

d<-density(gapminder$lifeExp)
plot(d, main="Distribution of Life Expectancy \n At Birth")



png("firstpass.png")
par(mar = c(5, 5, 5, 5))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", family="serif", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
dev.off()

pdf("firstpass.pdf", family="Garamond")
par(mar = c(5, 5, 5, 5))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
dev.off()


tiff("firstpass.tiff",width = 8, height = 8, pointsize = 1/300, units = 'in', res = 300)
par(mar = c(5, 5, 5, 5))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
legend("topleft", c("Overall Life Expectancy", "Life Expectancy in Europe", "Exceptional Case"), pch=c(NA,1, 8), lty=c(1,1,NA), col=c("red", "black", "blue"), bty="n")
dev.off()


postscript("firstpass.eps",width = 8, height = 8, pointsize = 1/300)
par(mar = c(5, 5, 5, 5))
plot(yearlevel$year, yearlevel$mean, type="l", xlab="Year", ylab="Life Expectancy in Years \n (At Birth)", main="Life Expectancy at Birth Over Time", bty="l", col="red", xlim=c(1980,2000), ylim=c(0,100), lwd=5)
lines(yearlevelc$year[yearlevelc$continent=="Europe"], yearlevelc$mean[yearlevelc$continent=="Europe"], type="b")
points(1990, 100, pch="*", cex=5)
legend("topleft", c("Overall Life Expectancy", "Life Expectancy in Europe", "Exceptional Case"), pch=c(NA,1, 8), lty=c(1,1,NA), col=c("red", "black", "blue"), bty="n")
dev.off()



b<- ggplot(data=yearlevel, aes(x=year, y=mean))
b 
b+geom_point()
b+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
b+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))+ylab("Life Expectancy in Years")+xlab("Year")+ggtitle("Life Expectancy in GGPLOT")+ theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
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

gf<- ggplot(data=yearlevelc, aes(x=year, y=mean, fill=continent))

  gf+geom_bar(stat="identity", position="dodge")+
    geom_errorbar(aes(ymin=mean-sd*.25, ymax=mean+sd*.25), position="dodge")+
    xlab("Year")+
    ylab("Life Expectancy")+
    ggtitle("Life Expectancy in GGPLOT")+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
    theme_bw()+
    theme(legend.position="left")+labs(fill = "Continent")+
    scale_fill_manual(values=c(tred, tgold, "black", "lightgray", "darkgray"))
  
  gf+geom_bar(stat="identity")+
    xlab("Year")+
    ylab("Life Expectancy")+
    ggtitle("Life Expectancy in GGPLOT")+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
    theme_bw()+
    theme(legend.position="left")+labs(fill = "Continent")+
    scale_fill_manual(values=c(tred, tgold, "black", "lightgray", "darkgray"))
  
  
  
 
  gf+geom_bar(stat="identity")+
    xlab("Year")+
    ylab("Life Expectancy")+
    ggtitle("Life Expectancy in GGPLOT")+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
    theme_bw()+
    theme(legend.position="left")+labs(fill = "Continent")+
    scale_fill_grey(start = 0.8, end = 0.2) 
  
  bf<- ggplot(data=gapminder, aes(x=year, y=lifeExp))

  bf+geom_boxplot(aes(group=year))


bf+geom_boxplot(aes(group=year))+facet_grid(continent ~.)
bf+geom_boxplot(aes(group=year))+facet_grid(.~ continent)

bf+geom_boxplot()+facet_grid(year~ continent)

saveme<- bf+
  geom_boxplot(aes(group=year))+
  facet_grid(.~ continent)+
  scale_x_continuous(limits=c(1950,2012), breaks=seq(1950,2010, by=10))

ggsave("ggsave.pdf", saveme)



fit1<- lm(lifeExp~gdpPercap, data=gapminder)
plot(fit1)  
qqPlot(fit1)
fit2<-lm(lifeExp~log(gdpPercap), data=gapminder)
plot(fit2)
qqPlot(fit2)


