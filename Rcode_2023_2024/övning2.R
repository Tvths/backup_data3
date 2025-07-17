library("ggplot2")

data(Nile)
Nile <- data.frame(level = as.vector(Nile))
Nile$years <- 1871:1970
p <- ggplot(data = Nile) +
  aes(x = years,y = level) + 
  geom_point() +
  theme_bw()
p + geom_line()
p <- p + xlab("Years") + ylab("Water level") + ggtitle("Nile series")
p
p1 <- ggplot(data=Nile,aes(x=years, y=level)) + geom_line() + ggtitle("linje")
p2 <- ggplot(data=Nile,aes(x=years, y=level)) + geom_point() + ggtitle("punkter")
class(p1)
mina_plottar<-list(p1=p1,p2)
for(i in 1:2){
  print(mina_plottar[[i]])
}
p

p <- ggplot(data=Nile,aes(x=years, y=level)) + 
  geom_line(color="red", linewidth=1)+
  geom_point(color="blue", size=4)
p


Nile$period <- "- 1900"
Nile$period[Nile$years >= 1900] <- "1900 - 1945"
Nile$period[Nile$years > 1945] <- "1945 + "
Nile$period <- as.factor(Nile$period)

p <- ggplot(data=Nile,aes(x=years, y=level)) + 
  geom_line(aes(color=period)) + 
  geom_point(aes(color=period, shape=period))
p


x<-scale(1:1000)
set.seed(4939)
y<-2+4*x+rnorm(n = 100,sd = 1)
A<-data.frame(x=x,y=y)

p <- ggplot(data = A, mapping = aes(x=x,y=y)) + 
  geom_point()
p
p+geom_hline(yintercept=0,linetype=3) 
p+geom_hline(yintercept=-5:10,linetype=3)
p+geom_vline(xintercept = c(-1,0,1), linetype=3)
p+geom_abline(slope = 4, intercept = 2, linetype = 3, linewidth = 1, col = "blue")+ 
  geom_vline(xintercept=0,linetype=3) + 
  geom_hline(yintercept=0,linetype=3)



x<-scale(1:100)
set.seed(4939)
y<-2+4*x+rnorm(n = 100,sd=1)
A<-data.frame(x=x,y=y)


p+geom_smooth(method = lm, se = FALSE)

ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = loess,se=TRUE)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 0.8)



library(MASS)
no_obs<-5000
set.seed(26)
X<-mvrnorm(n = no_obs,mu = c(1,1),Sigma = matrix(c(1,0.8,0.8,1),2,2)/2.5)
df<-data.frame(x1=exp(X[,1]),x2=exp(X[,2]))
ggplot(data = df,mapping = aes(x = x1,y = x2))+geom_point()

ggplot(data = df,mapping = aes(x = x1,y = x2))+geom_point(alpha = 0.1)
ggplot(data = df,mapping = aes(x = x1,y = x2))+geom_point(alpha = 0.3)

ggplot(data = df,mapping = aes(x = x1,y = x2))+geom_point(size=0.3)


data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)

p <- ggplot(data = mtcars, aes(x = cyl)) + geom_bar(fill = "pink", colour = "darkorchid4") + ggtitle("ABC")
p 
p + coord_flip() 
ggplot(data = mtcars, aes(x = cyl)) + geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = gear) +
  ylab("ABC") + scale_y_continuous(labels = scales :: percent)

p1 <- ggplot(data=mtcars) + aes(x=cyl, fill=gear) + geom_bar(position="stack")
p1
p2 <- ggplot(data=mtcars) + aes(x=cyl, fill=gear) + geom_bar(position="dodge")
p2
p1 + scale_fill_discrete(name="Testa\nDetta") + scale_fill_manual(values = c("orange","blue","red"))
p2 + scale_fill_manual(values = c("red","blue","yellow"))



set.seed(324)
df<-data.frame(var=rnorm(5000))
p <- ggplot(data = df, aes(x=var)) + geom_histogram(fill="pink", colour = "black", binwidth = 7, aes(y =..density..))
p
set.seed(4390)
y<-c(rnorm(n = 1000,mean = 7,sd = 1),
     rnorm(n = 200,mean = 4,sd = 0.1),
     rgamma(n = 2000,shape = 1,rate = 1))
Y<-data.frame(var1=y)
ggplot(data = Y,aes(x=var1,y=..density..))+geom_histogram(binwidth = 0.3)
ggplot(data = Y,aes(x=var1))+geom_density()
ggplot(data = Y,aes(x=var1))+geom_density(adjust = 0.01)
ggplot(data = Y,aes(x=var1))+geom_density(adjust = 0.01, alpha = 0.1, fill = "blue")

ggplot(data = Y,aes(x = var1, y =..density..)) + geom_density(adjust = 0.01, col = "red", linewidth = 1)

ggplot(data = chickwts, aes(y = weight, x = feed)) + geom_boxplot(fill = "yellow")
ggplot(data = iris,aes(y=Sepal.Length))+geom_boxplot(fill="yellow")
ggplot(data = iris,aes(y=Sepal.Length,x=Species))+geom_boxplot(fill="yellow")
p <- ggplot(data=mtcars,aes(x=cyl, y=mpg)) + geom_boxplot()
p + coord_flip() + xlab("XYZ") + ylab("ABC")

data = iris

ggplot(data = iris,mapping = aes(x = Sepal.Length)) + 
  geom_histogram(binwidth = 0.2,fill="green",col="black") + 
  facet_grid(.~Species)


ggplot(data = iris,mapping = aes(x = Sepal.Length)) + 
  geom_histogram(binwidth = 0.2,fill="green",col="black") + 
  facet_grid(Species~.)



ggplot(data = iris,mapping = aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(size = 3 ,col="black") +
  facet_grid(Species~.) + geom_smooth(method = lm, se = FALSE)



mpg <- as.data.frame(mpg)
head(mpg)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(size = 2) + 
  facet_grid(cyl~.)


ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(size = 2) + 
  facet_grid(.~cyl) +
  geom_smooth(method = lm, se = FALSE)


ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(size = 2) + 
  facet_grid(cyl~drv)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(size = 2) + 
  facet_grid(cyl~class)


ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(size = 2) + 
  facet_wrap(vars(trans))
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(size = 2) + 
  facet_grid(trans~.)


library(cowplot)
mpg<-as.data.frame(mpg)
head(mpg)

p0<-ggplot(data = mpg,mapping = aes(x = cty )) + 
  geom_histogram(binwidth = 2)

p0     
p1<-ggplot(data = mpg,mapping = aes(x = cty ,y=hwy)) + 
  geom_point(size=1)
p1
p2<-ggplot(data = mpg,mapping = aes(x = cty ,y=hwy)) + 
  geom_point(size=1) + 
  facet_grid( drv~.)
p2


plot_grid(p0,p1)
plot_grid(p1,p0)



iris$Petal.Length.Cat <- cut(iris$Petal.Length, breaks=3)
iris$Sepal.Length.Cat <- cut(iris$Sepal.Length, breaks=3)
table(iris$Petal.Length.Cat, iris$Species)
ftable(iris$Petal.Length.Cat, iris$Sepal.Length.Cat, iris$Species)
tab <- table(iris$Petal.Length.Cat, iris$Species)
chisq.test(tab)




z <- a %>% fun1(b) %>% fun2(c) %>% fun3()


c <- ggplot(mtcars, aes(qsec, wt))
c + stat_smooth()
c + stat_smooth() + geom_point()
c + stat_smooth(se = FALSE) + geom_point()

c + stat_smooth(span = 0.9) + geom_point()
c + stat_smooth(level = 0.99) + geom_point()
c + stat_smooth(method = "lm") + geom_point()
library(splines)
library(MASS)
c + stat_smooth(method = "lm", formula = y ~ ns(x,3)) +
  geom_point()
c + stat_smooth(method = rlm, formula= y ~ ns(x,3)) + geom_point()
