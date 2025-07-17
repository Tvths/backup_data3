x <- c(1,2,3,4,5,6)

min_matris1 <- matrix(x,nrow=3,ncol=2)
min_matris1

mean(min_matris1)
median(min_matris1)
sum(min_matris1)
sd(min_matris1)
var(min_matris1)
min(min_matris1)
max(min_matris1)
which.min(min_matris1)
which.max(min_matris1)
range(min_matris1)


min_matris2 <- matrix(x,nrow = 2)
min_matris2

a <- matrix(c(1,2,3,4), ncol = 2)
b <- matrix(1:4, ncol = 2)

c <- a*b
c

z <- rep(c(1,2,3,4,5),10)
z
x <- 1:50
x
c <- (1:50)^2
c
d <- log(1:50)
d
stor_matris <- cbind(z,x,c,d)
stor_matris
class(stor_matris)

x<-c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
A<-matrix(x, nrow=3)
A
y<-c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
B<-matrix(y, nrow=3)
B

A&B
A|B
!A
X <- matrix(1:6, nrow=3)
Y <- matrix(6:1, nrow=3)
X > Y
X <= Y
X == Y


x <-c(1,2,3,4,5,6)
min_matris <- matrix(x, nrow=3,ncol=2)
min_matris

min_matris[1,1,drop=FALSE]
y<-seq(4,11)
z<-c(rep(2,4),rep(9,4))
rad_mat<-rbind(y,z)
rad_mat
kol_mat<-cbind(y,z)
kol_mat



hist(iris$Sepal.Length)





library("lubridate")
en_tid <- ymd_h("2024-12-12 4 PM")
en_tid
wday(en_tid, label = TRUE, abbr = FALSE, week_start = 1, locale = "ru_RU")



