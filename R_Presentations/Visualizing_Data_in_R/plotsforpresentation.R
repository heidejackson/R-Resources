setwd("C:\Users\heidej\Documents\Data_Visualization_in_R")
pdf("simpleplot.pdf")
x<-1:100
y<-100:1
plot(x,y)
dev.off()

pdf("moreoptions.pdf")

x<-1:100
y<-100:1
plot(x,y,
     ylab="Fancy /n Multiline/n Label", #labels Y axis
     xlab="Simple Label", #labels X axis
     main="This is a Test", #Graph title
     xlim=c(0,100), #Range of X axis
     ylim=c(0,100), #Range of Y axis
     type="b", #Specify p for point, l for line, or b for both
     lty=2, #Type of Line
     pch=2, # Type of point, can also be a character in quotes
     lwd=3, #specify line thickness
     col="red", #Color of plotted objects
     bty="l" #Type of box around plot
)
dev.off()

pdf("addedoptions.pdf")
x1<-1:100
y<-100:1
x2<-100:1
plot(x1,y, col="red", type="l")
lines(x2, y, col="blue") #add a line that is blue
points(x1,x2) # add points with these coordinates
legend("topleft", c("Line 1", "Line 2", "Points"), 
       col=c("red", "blue", "black"), lty=c(1,1,NA), 
       pch=c(NA, NA, 1), bty="n")
dev.off()

pdf("histogram.pdf")
x1<-rnorm(100,0,1)
hist(x1)
dev.off()

pdf("bargraph.pdf")
name=c("Example 1", "Example 2")
x<- c(1,5)
barplot(x, names=name, col=c("red", "blue"))
dev.off()