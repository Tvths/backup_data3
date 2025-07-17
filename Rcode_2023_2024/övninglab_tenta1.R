a <- sqrt(abs(-3)^2-3)
a

b <- sin(log(4^3))
b


1 + 1i
z <- (1+1i)^3
z
Re(z) # Reell del
Im(z) # Imaginar del
Arg(z) # Argument
Conj(z) # Komplex konjugat
sqrt(-1)
sqrt(-1 + 0i)


a_int<-3L
a <- 1
b <- "Text"
c <- TRUE
z <- (1+1i)^3
typeof(a_int)
typeof(a)
typeof(b)
typeof(c)
typeof(z)


a <- c(1,5,2)
ls()
rm(a)
ls()



####lab1-extraproblem
#1,
z = exp(1 + 3/13) -1
z
y = log(pi/17 , bas = exp(1))
y
x = sqrt(z^2 + abs(y))
x

#2,
#a,
sqrt(pi) + abs(sin(exp(1)))
#b,
cos(pi/7) + abs(log(1/exp(1), bas = 3)) + 2^(1/2)
#c, 
5^3
#d
5^(pi)


#3,
x = pi-exp(1)
cat("sin(0) is", sin(0),"\n",
    "The difference between pi(3.1416) and e (2.7183) is:","\n", x)
#4,
x = rnorm(10,mean = 1, sd = 1)
cat("x is an element of type double with value",x)


s = c(rep(3,times = 3),rep(6, times = 3))
s

###
i <- 1:100
j <- 1:10
isp <- i^2
jsp <- i^3
sum(isp) - sum(jsp)
###
myz <- c(5, -1, 2, 10, 0)
quantile(myz, probs=c(0.25,0.5,0.75))
###
myText <- c(rep("Gris", times=10) ,rep("Lamm", times=3))
table(myText)
#andelar
table(myText)/sum(table(myText))
###
myx <- rep(x = 7:12, times = 10)
myx <- c(NA, myx)
mean(myx)
mean(myx, na.rm=TRUE)
###
k = c(12,pi,1,7)
l = c(2*sqrt(1),2*sqrt(2), 2*sqrt(3))
m = c(exp(1), log(2+exp(1), base = exp(1)))
p = c(log(3, base = exp(1)), exp(pi +1), sin(pi/3))

q = c(k,l,m,p)
q[5:7] = c(sqrt(5),sqrt(6),sqrt(7))
q
###
seq(from = 10, to = 3, by = -1)
seq(from = 3, to = 17, by = 2)
###
a <- -2 : 2
b <- c("Text1", "Text2", "TRUE", "FALSE")
as.logical(a)
as.logical(b)
###
d <- c(TRUE, FALSE, NA)
as.numeric(d)
as.character(d)
###gamla tenta 1

###uppgift1 estimate gamma
estimate_gamma <- function(x, na.rm){
#argument x och na.rm

#se till alla värder är positiva
  if(any(x <= 0)) stop("all x must be positiva")
#na.rm(ta bort NA värden)
  index_of_na <- is.na(x)
  if(na.rm){
    x <- x[!index_of_na]
  }else{
    if(any(index_of_na)){
      return(NA)
    }
  }
#beräkna K och theta  

  N <- length(x)

  #k_bar
  k_bar_t <- N * sum(x)

  k_bar_n1 <- N * sum(x*log(x))

  k_bar_n2 <- sum(log(x))

  k_bar_n3 <- sum(x)

  k_bar <- k_bar_t/(k_bar_n1-k_bar_n2*k_bar_n3)

  # k_hat
  k_hat = k_bar - 1/N*(3*k_bar - 2/3*(k_bar/1 + k_bar) -4/5*(k_bar/(1+k_bar)^2))

  #theta
  theta_1 <- N * sum(x*log(x))
  theta_2 <- sum(log(x))
  theta_3 <- sum(x)

  theta_hat <- (N/(N-1))*(1/N^2)*(theta_1 - theta_2*theta_3)
  output <- list(N = N,
                 k_hat = k_hat,
                 theta_hat = theta_hat,
                 x = x)
  return(output)
}
set.seed(33)
x1<-rgamma(n = 100,shape = 4,scale = 10)
a1<-estimate_gamma(x = x1,na.rm = TRUE)
str(a1)
  
set.seed(44)
x2<-rgamma(n = 500,shape = 2,scale = 5)
a2<-estimate_gamma(x = x2,na.rm = TRUE)
a2[2:3]


estimate_gamma(x = -(1:10),na.rm = TRUE)
estimate_gamma(x = c(NA,x1,NA),na.rm = FALSE)

a3<-estimate_gamma(x = c(NA,x1,NA),na.rm = TRUE)
a3[2:3]


###uppgift2
library(stringr)

text <- "hej"
#alternativ 1
#skapa en text med bara stora boksstäver
#byt ut om det behövs
change_letters <- function(text, first = TRUE, last = TRUE){
  text_upper <- str_to_upper(text)
  #första bokstaven
  if(first){
    first <- str_sub(text_upper, start = 1, end = 1)
    text <- str_replace(string = text, pattern = "^(.)", replacement = first)
  }
  #sista bokstaven
  if(last){
    last <- str_sub(text_upper, start = -1, end = -1)
    text <- str_replace(string = text, pattern = "(.)$", replacement = last)
  }
  return(text)
}
?str_replace
#alternativ2
#skapa text med stor bokstav om det behövs
change_letters <- function(text, first = TRUE, last = TRUE){
  if(first){
    str_sub(text, start = 1, end = 1) <- str_to_upper(str_sub(text, start = 1, end = 1))
  }
  
  if(last){
    str_sub(text, start = -1, end = -1) <- str_to_upper(str_sub(text, start = -1, end = -1))
  }
  return(text)
}
change_letters(text = "hej",first = FALSE,last = FALSE)
change_letters(text = "hej",first = TRUE,last = FALSE)

first <- str_sub(text,start = 1,end = 1)
first
text <- str_replace(string = text, pattern = "^(.)", replacement = first)
text
?str_replace
###uppgift3

tol = 0.001
l = 1
w = 1
while_func <- function(tol,l,w){
  t <- 0
  y_old <- 1e10
  y_new <- exp(-l*t) *cos(w*t)
  while (abs(y_new - y_old) >= 0) {
    t <- t+1
    y_old <- y_new
    y_new <- exp(-l*t) * cos(w*t)
  }
  
  res <- list(iter = t-1,
              y_last = y_new,
              y_second_last = y_old
              )
  return(res)
}
a<-while_func(tol = 0.001,l = 1,w = 1)
a
while_func(tol = 0.1,l = 1,w = 0.1)


###del B


matrix_func <- function(n,m){
  #skapa en tom matris
  mat <- matrix(data = NA, nrow = n, ncol = m)
  #loppa igenom matrisen
  
  for (i in 1:n) {
    for (j in 1:m) {
      #beräkna (i*j)^2
      mat[i,j] <- (i*j)^2
      #testa om det är j'mt delbart med 4
      if(mat[i,j] %% 4 != 0 ){
        mat[i,j] <- 0
      }
    }
  }
  return(mat)
}
matrix_func(n = 1,m = 1)
matrix_func(n = 4,m = 5)

###uppgift 4
data("iris")
data("mtcars")

library("ggplot2")
#skapa funktion factor_plot(my_data,title)
factor_plot <- function(my_data, title){
#funktion ska ta ett data.frame och skapa barpot.
  if(ncol(my_data) > 2){
      stop("too many variables")
    }
  if(ncol(my_data) == 1){
    if(!is.factor(my_data[,1])){
      stop("non factor data")
    }
    p1 = ggplot(data = my_data) + aes(x = my_data[,1]) + geom_bar()
  }
  if(ncol(my_data) == 2){
    if(!is.factor(my_data[,1]) | !is.factor(my_data[,2])){
      stop("non factor data")
    }
    p1 = ggplot(data = my_data) + aes(x = my_data[,1],fill = my_data[,2]) + 
      geom_bar(position = "dodge") + 
      xlab(label = names(my_data)[1]) + labs(fill = names(my_data)[2])
  }
  p1 = p1 + ggtitle(title)
  return(p1)
}
B1<-data.frame(cyl=as.factor(mtcars$cyl))
B2<-data.frame(cyl=as.factor(mtcars$cyl),gear=as.factor(mtcars$gear))
B3<-data.frame(gear=as.factor(mtcars$gear),cyl=as.factor(mtcars$cyl))
B4<-data.frame(Species=iris$Species)
factor_plot(my_data = iris,title = "IRIS")
factor_plot(my_data = iris[,1:2],title = "IRIS")
C1<-factor_plot(my_data = B1,title = "hej")
class(C1)
print(C1)
factor_plot(my_data = B2,title = "AB")
factor_plot(my_data = B3,title = "reverse")
factor_plot(my_data = B4,title = "IRIS")

