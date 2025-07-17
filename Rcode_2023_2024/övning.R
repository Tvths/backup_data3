data(iris)
data(mtcars)
data(Nile)

Nile2 <- as.data.frame(Nile)
Nile2$years <- 1871:1970
Nile2$level


freqs <- table(iris$Species)
freqs

p <- ggplot(data = Nile) + 
  aes(x = years, y = level) + 
  geom_point()
print(p)


pie(freqs,labels = c("del1","del2","del3"))
pie(freqs,labels = c("ABC","IEF","XYZ"), main="Cirkeldiagram")
pie(freqs,col = c("#496187","#040b17","#cf081f"), labels = c("ABC","IEF","XYZ"), main = "Cirkeldiagram")


freqsCars <- table(mtcars$gear)
freqsCars

barplot(freqsCars)
barplot(freqsCars, xlab = "Gears", ylab = "Counts", main = "Cars", col = "#97c918")

carTable <- table(mtcars$vs, mtcars$gear)
carTable
barplot(carTable, col = c("#bd1e3b","#0fa630"), main ="Car Distribution by Gears and VS",
        xlab = "number of gears",ylab = "ABC",
        legend = rownames(carTable), beside = TRUE)

hist(iris$Sepal.Length,col="blue", main="Min titel", xlab="X-titel", ylab="Y-titel", breaks = 40)

plot(density(iris$Sepal.Length))
dens <- density(iris$Sepal.Length)

hist(iris$Sepal.Length, breaks=40, col="red",freq = TRUE)
hist(iris$Sepal.Length, breaks=15, col="red",freq = FALSE)
lines(dens)

boxplot(Sepal.Length~Species, data=iris, col=c("blue", "green", "red"))



plot(Nile$years, Nile$x,type ="s")
plot(Nile$years, Nile$x, type="s", xlim=c(1900,1945), ylim=c(600, 1200))

points(Nile$years, Nile$x, pch=20)


jpeg(filename = "minJPEG.jpeg", width = 480, height = 480)
plot(Nile$years, Nile$x, type="l")
dev.off()


iris
mtcars
Nile

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x
apply(x, 2, mean, trim = .2)
