##¤#tenta2####


#uppgift 1
###del a
while_calc <- function(n,type){
  i = 0
  if(type == "sum"){
    sum_of_vector = 0
    while (i < length(n)) {
      i = i + 1
      sum_of_vector = sum(n)
    }
    return(sum_of_vector)
  }
  if(type == "prod"){
    prod_of_vector = 1
    while (i < length(n)) {
      i = i + 1
      prod_of_vector = prod_of_vector * n[i]
    }
    return(prod_of_vector)
  }
}
a<-while_calc(n = 1:3,type = "sum")
a
b<-while_calc(n = 1:3,type = "prod")
b
while_calc(n = 10:9,type = "prod")
while_calc(n = 10:9,type = "sum")
while_calc(n = c(2,4,2,10,3,3),type = "sum")
while_calc(n = c(2,4,2,10,3,3),type = "prod")

###del b

data_struct <- function(type){
  type_of_data <- c("vector","matrix","list")
  if(!type %in% type_of_data){
    stop("not supported")
  }
  if(type == "vector"){
    vector <- c(rep(3,4),rep(2,4),rep(1,4))
    return(vector)
  }
  if(type == "matrix"){
    matrix1 = matrix(LETTERS[1:12], nrow = 3, ncol = 4)
    return(matrix1)
  }
  if(type == "list"){
    List = list(e1 = c(1,20,87),
                e1 = c(TRUE,FALSE,FALSE,FALSE),
                e3 = c(100,400,900,1600,2500,3600,4900,6400,8100,10000))
    return(List)
  }
}
A<-data_struct(type="vector")
A
data_struct(type="matrix")
data_struct(type="list")
data_struct(type="hej")
data_struct(type = "data.frame")
data_struct(type = "xyz")


####uppgift 2

my_spec<- function(f,sigma2,phi){
  if(is.null(phi)){
    s = rep(sigma2, times = length(f))
  }else if(length(phi) == 1){
    s = sigma2/(1+phi[1]^2-2*phi[1]*cos(2*pi*f))
  }else if(length(phi) == 2 ){
    s = sigma2/(1+phi[1]^2+phi[2]^2-2*phi[1]*(1-phi[2])*cos(2*pi*f)-2*phi[2]*cos(4*pi*f))
  }else{
    stop("phi is too long")
  }
  return(s)
}
freq1<-seq(0,0.5,length=5)
my_spec(f = freq1,sigma2 = 2,phi = c(1,2,3))
my_spec(f = freq1,sigma2 = 2,phi = NULL)
my_spec(f = freq1,sigma2 = 2,phi = 0.7)
my_spec(f = freq1,sigma2 = 2,phi = c(0.7,-0.1))
freq2<-seq(0,0.5,length=200)
s1<-my_spec(f = freq2,sigma2 = 1,phi = c(0.9,-0.8))
plot(freq2,s1,t="l")
s2<-my_spec(f = freq2,sigma2 = 0.5,phi = c(0.9))
plot(freq2,s2,t="l")
s3<-my_spec(f = freq2,sigma2 = 0.5,phi = c(-0.6))
plot(freq2,s3,t="l")
s4<-my_spec(f = freq2,sigma2 = 10,phi = c(0.3,0.3))
plot(freq2,s4,t="l")
s5<-my_spec(f = freq2,sigma2 = 4,phi = NULL)
plot(freq2,s5,t="l")


####uppgift 3
library("stringr")

####ALT 1

ploy_derivative<-function(f){
  if(!is.character(f)) stop("f is not a string!")
  index1<-str_locate( f,pattern = "[0-9]+")   
  a<-as.numeric(str_sub(f,start = index1[1],end = index1[2]))
  index2<-str_locate(f,pattern = "\\^") +1
  n<-as.numeric(str_sub(f,start = index2[1],end = index2[2])) 
  an<-a*n   
  new_n<-n-1   
  der<-str_c("dy/dx=",an,"x^",new_n)   
  return(der) 
}
?str_sub
?str_locate
fruit <- c("apple", "banana", "pear", "pineapple")
str_locate(fruit, "$")
str_locate(fruit, "a")
str_locate(fruit, "e")
str_locate(fruit, c("a", "b", "p", "p"))

str_locate_all(fruit, "a")
str_locate_all(fruit, "e")
str_locate_all(fruit, c("a", "b", "p", "p"))

ploy_derivative(f = 33)
a<-ploy_derivative(f = "3x^1")
str(a)
a

ploy_derivative(f = "2031x^2")

ploy_derivative(f = "25x^3")


ploy_derivative(f = "1234x^6")



ploy_derivative<-function(f){
  if(!is.character(f)) stop("f is not a string!")
  split_text <- strsplit(f, split = 'x^', fixed = TRUE)
  unlist_text <- unlist(split_text)
  unlist_text <- as.numeric(unlist_text)
  a <- unlist_text[1]
  n <- unlist_text[2]
  an <- a*n
  n_new <- n -1
  der<-str_c("dy/dx=",an,"x^",n_new)
  return(der)
}
f = "1234x^6"
class(strsplit(f, split = 'x^', fixed = TRUE))

####uppgift 4

list_index <- function(X, index_list){
  if(!is.list(index_list)){
    stop("index_list is not a list")
  }
  if(!is.list(X)){
    stop("X is not a list")
  }
  if(length(X) != length(index_list)){
    stop("list length mismatch")
  }
  y <- vector(mode = "list", length(X))
  names(y) <- names(X)
  for (i in 1:length(X)){
    y[[i]] <- X[[i]][index_list[[i]]]
  }
  return(y)
}
a1<-list((1:10)*3)
index1<-list(c(10,2,3))

list_index(X = a1,index_list = TRUE)
list_index(X = "abc",index_list = index1)
list_index(X = a1,index_list = list(1,2,3,4))
list_index(X = list(c(1,5,7)),index_list = list(c(1,3)))
b1<-list_index(X = a1,index_list = index1)
b1
data("trees")
a2<-as.list(trees)
index2<-list(1:3,31:28,c(3,4))
b2<-list_index(X = a2,index_list = index2)
str(b2)
data("AirPassengers")
a3<-list(a=month.name,b=1:5,c=as.vector(AirPassengers)[1:20],sqrt(1:10))
index3<-list(c(12,9),5:3,c(1,3,5,20),c(2,5,9))
b3<-list_index(X = a3,index_list = index3)
str(b3)

####uppgift 5
library(lubridate)
library(ggplot2)
path <- "C:/Users/timce/OneDrive/Dokument/coffee_data.csv"
coffee <- read.csv(file = path, stringsAsFactors = FALSE)

coffee[,1]<-as.Date(coffee[,1])
colnames(coffee)
g1 <- ggplot(data = coffee) + aes(x = date, y = coffee) + geom_line(col = "red")
g1

month <- month(coffee$date)
g2 <- ggplot(data = coffee) + aes(x = month, y = coffee) + geom_boxplot(fill = "blue")
g2

####exam20
####uppgift1

while_list <- function(x){
  List <- vector(mode = "list", 3)
  index <- 1
  a <- TRUE
  while(a){
    y <- x[index]
    if(y %% 2 == 0){
      List[[1]] <- c(List[[1]], y)
    }
    if(y %% 3 == 0){
      List[[2]] <- c(List[[2]], y)
    }
    if(y %% 5 == 0){
      List[[3]] <- c(List[[3]], y)
    }
    index <- index +1
    if(index > length(x)){
      a <- FALSE
    }
  }
  return(List)
}
while_list(x = 13)
a<-while_list(x = 3)
a
str(a)
while_list(x = c(2,4))
while_list(x = c(5,25,35))
while_list(x = 1:12)
while_list(x = 25:35)


###del b

mat_func <- function(n,m){
  mat <- matrix(0,n,m)
  for(i in 1:n){
    for (j in 1:m){
      mat[i,j] = ((-1)^(i +1))/(j +1)
    }
  }
  return(round(mat,3))
}
mat_func(2,3)


###uppgift 2

my_conf <- function(x, alpha){
  n <- length(x)
  v <- 1-alpha/2
  z <- qnorm(v)
  Mean_x <- mean(x)
  if(all(x==1 | x==0)){
    SE <- sqrt((Mean_x*(1-Mean_x))/n)
    Lower <- Mean_x - z*SE
    Upper <- Mean_x + z*SE
  }else{
    SE <- sd(x)/sqrt(n)
    Lower <- Mean_x - z*SE
    Upper <- Mean_x + z*SE
  }
  vect <- c(Lower,Upper)
  return(vect)
}

set.seed(322)
a1<-rnorm(100,3,4)
b1<-rbinom(n = 75,size = 1,prob = 0.8)
a2<-c(19,12,32,42,53,22,43,11,12,45)
b2<-c(0,1,1,1,1,0,0,0,1,1,0,1,0,0,1,0,0,0,0)
ci1<-my_conf(x = a1,alpha = 0.05)
ci1
my_conf(x = a2,alpha = 0.01)
my_conf(x = a1,alpha = 0.001)
my_conf(x = b1,alpha = 0.1)
my_conf(x = b2,alpha = 0.05)

####uppgift 3

library("stringr")
library("lubridate")

a <- ymd("2020-08-03")
b <- ymd("2020-08-17")
c <- interval(a,b)
d <- as.duration(c)
h <- as.numeric(d)
h/(7*24*60*60)


date_count <- function(date1,date2,unit){
  if(!is.character(date1)) stop("date1 is not character")
  if(!is.character(date2)) stop("date2 is not character")
  date1 <- ymd(date1)
  date2 <- ymd(date2)
  int <- interval(start =date1, end = date2)
  dur <- as.duration(int)
  if(unit == "week"){
    seconds <- as.numeric(dur)
    weeks <- int%/%weeks(1)
    if(weeks == 0){
      seconds = 0
    }
    a<-str_c("date1: ",date1," date2: ",date2," weeks: ",weeks," second: ",seconds)
  }else if(unit == "month"){
    months <- int%/%months(1)
    a<-str_c("date1: ",date1," date2: ",date2," month: ",months)
  }else{
    stop("wrong format of unit")
  }
  b<- as.character(a)
  return(b)
}

date_count(date1 = TRUE,date2 = "2020-08-31",unit = "week")
date_count(date1 = "2020-08-3",date2 = 1:10,unit = "week")
date_count(date1 = "2020-08-3",date2 = "2020-08-31",unit = "day")
date_count(date1 = "2020-08-3",date2 = "2020-08-31",unit = 233.1)

D<-date_count(date1 = "2020-08-3",date2 = "2020-08-7",unit = "week")
class(D)
D
date_count(date1 = "2020-08-3",date2 = "2020-08-10",unit = "week")
date_count(date1 = "2020-08-3",date2 = "2020-08-17",unit = "week")
date_count(date1 = "2020-04-14",date2 = "2020-09-29",unit = "week")
date_count(date1 = "2020-08-01",date2 = "2020-08-31",unit = "month")
date_count(date1 = "2020-08-01",date2 = "2020-09-01",unit = "month")
date_count(date1 = "1930-02-18",date2 = "1999-09-10",unit = "month")


####uppgift 4


my_binom <- function(x,n,p,cdf){
  if(x < 0) stop("x is negative")
  if(n < 0) stop("n is negative")
  if(x > n) stop() 
  if(cdf == TRUE){
    p_xnp = 0
    for (i in 0:x) {
      p_xnp <- p_xnp + (factorial(n)/(factorial(i)*factorial(n-i))) * (p^i) * (1-p)^(n-i)
    }
    return(p_xnp)
  }else{
    p_xnp <- (factorial(n)/(factorial(x)*factorial(n-x))) * (p^x) * (1-p)^(n-x)
    return(p_xnp)
  }
}
my_binom(x = -3,n = 10,p = 0.6,cdf = TRUE)
my_binom(x = 3,n = -2,p = 0.6,cdf = TRUE)
B<-my_binom(x = 3,n = 10,p = 0.6,cdf = FALSE)
class(B)
B
dbinom(x = 3,size = 10,prob = 0.6)
my_binom(x = 9,n = 29,p = 0.2,cdf = FALSE)
my_binom(x = 0,n = 15,p = 0.1,cdf = FALSE)
my_binom(x = 3,n = 10,p = 0.6,cdf = TRUE)
pbinom(q = 3,size = 10,prob = 0.6) 
my_binom(x = 3,n = 12,p = 0.2,cdf = TRUE)
my_binom(x = 11,n = 17,p = 0.7,cdf = TRUE)
my_binom(x = 17,n = 17,p = 0.7,cdf = TRUE)

my_binom<-function(x,n,p,cdf){
  if(n<0) stop("n is negative")
  if(x<0) stop("x is negative")
  
  binom_pdf <- function(x,n,p){
    if(x < 0 | x > n) stop()
    comb <- (factorial(n) / (factorial(x)*factorial(n-x)))
    res <- comb * p^x * (1-p)^(n-x)
    return(res)
  }
  binom_cdf <- function(x,n,p){
    res <- 0
    for(k in 0:x){
      res <- res + binom_pdf(k,n,p)
    }
    return(res)
  }
  if(cdf==FALSE){
    val<-binom_pdf(x = x,n = n,p = p)
  }else{
    val<-binom_cdf(x = x,n = n,p = p)
  } 
  return(val)
}
my_binom(x = -3,n = 10,p = 0.6,cdf = TRUE)
my_binom(x = 3,n = -2,p = 0.6,cdf = TRUE)
B<-my_binom(x = 3,n = 10,p = 0.6,cdf = FALSE)
class(B)
B
dbinom(x = 3,size = 10,prob = 0.6)
my_binom(x = 9,n = 29,p = 0.2,cdf = FALSE)
my_binom(x = 0,n = 15,p = 0.1,cdf = FALSE)
my_binom(x = 3,n = 10,p = 0.6,cdf = TRUE)
pbinom(q = 3,size = 10,prob = 0.6) 
my_binom(x = 3,n = 12,p = 0.2,cdf = TRUE)
my_binom(x = 11,n = 17,p = 0.7,cdf = TRUE)
my_binom(x = 17,n = 17,p = 0.7,cdf = TRUE)


###uppgift 5
library(ggplot2)
library(cowplot)
data = trees

g1 <- ggplot(data = trees) + aes(x = Height) + geom_histogram(bins = 10)
g1
g2 <- ggplot(data = trees) + aes(Girth) + geom_boxplot() + coord_flip()
g2
g3 <- ggplot(data = trees) + aes( x = Height, y = Volume) + geom_point()
g3
my_plots <- plot_grid(g1,g2,g3, nrow = 3)
my_plots

a <- rbinom(100, size = 2, prob = 1)
a
