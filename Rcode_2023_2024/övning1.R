library(ggplot2)

data(Nile)
Nile <- data.frame(level = as.vector(Nile))
Nile$years <- 1871:1970
P <- ggplot(data=Nile, aes(x=years, y=level)) + geom_point()
P


p <- ggplot(data=Nile,aes(x=years, y=level, col = period)) + geom_line()
p
p <- p + xlab("Years") + ylab("Water level") + ggtitle("Nile series")
P
p1 <- ggplot(data=Nile,aes(x=years, y=level)) + geom_line() + ggtitle("linje")
p2 <- ggplot(data=Nile,aes(x=years, y=level)) + geom_point() + ggtitle("punkter")


Nile$period <- "- 1900"
Nile$period[Nile$years >= 1900] <- "1900 - 1945"
Nile$period[Nile$years > 1945] <- "1945 + "
Nile$period <- as.factor(Nile$period)


P <- ggplot(data=Nile,aes(x=years, y=level)) + geom_line(aes(color=period)) + geom_point()
P

P <- P + theme_bw()
P

P <- ggplot(data = chickwts) + aes(x  = weight)
P
P <- P + geom_histogram(binwidth = 20) + facet_grid(.~feed)
P


P <- P + geom_boxplot(aes(x = feed, y = weight)) + xlab = ("Feed")
P


ggplot(data = chickwts) + 
  aes(x=weight) +
  geom_histogram(binwidth = 30,aes(y = ..density..))+
  geom_density()


data("longley")

ggplot(data = longley, aes(x = Year, y = GNP.deflator)) +
  geom_line()
  geom_smooth()

  
ggplot(data = Nile, mapping = aes(years,level,col = period))+
  geom_line()+ 
  scale_color_manual(values = c("#9C1414", "#22B51B", "#1B37B5"))
  scale_linetype_manual(values = c(2,2,1,2,2))
  
  

 
  