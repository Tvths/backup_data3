#### Programnering i R ####
#### Datorlaboration [3] ####

Namn <- c("Viet Tien Trinh", "Vinh Phat Thong ")
LiuId <- c("vietr933","vinth530")
Grupp <- "grupp13"

#' @title classroom()
#' @descripsion 
#' funktionen kommer att kolla vilken storlek av ett room och det beroende på antal sittplatser som
#' finns i roomet sedan.   
#' @param seats 
#'  antal sittplatser. 
#' @param whiteboards 
#'  antal vita tavla.
#' @return
#' en mening som innehåller antal sittplatser och antal vita tavla.
classroom <- function(seats, whiteboards){
  #Ställa 2 varning messeage med 2 olika falla.
  if(seats <= 0){
    stop("Error in classroom() : Not a positive value")
  }
  if(whiteboards < 0){
    stop("Error in classroom() : Not a non-negative value")
  }
  if(whiteboards == 0){
    whiteboards = "no"
  }
  #Skapa en variabel x och låt x vara en list med två element"seats" och "Whiteboards"
  #sent låt class av x är "classroom".
  x = list(seats = seats, whiteboards = whiteboards)
  class(x) <- "classroom"
  return(x)
}
print.classroom <- function(x){
  #Kolla och returnera 3 olika resultat beroende på antal sittplatser.
  if(x$seats <= 10){
    cat("A group room for", x$seats, "students with", x$whiteboards, "whiteboards.")
  }else if(x$seats >= 11 && x$seats <= 50){
    cat(" A classroom for", x$seats, "students with", x$whiteboards, "whiteboards.")
  }else if(x$seats > 50){
    cat("A lecture hall for", x$seats, "students with", x$whiteboards, "whiteboards.")
  }
}



#Använda paket "lubrodate".
library(lubridate)

#' @Title give_blood()
#' @describeIn 
#' Funktionen kommer att kolla och returnera vilken dag kan man får ger blod 
#' efter senaste gång.
#' @param lasttime 
#' Senaste dag som man gav blod.
#' @param holiday 
#' Det finns 2 falla är "hemma" och "inte hemma". 
#' @param sex 
#' Könen, "m" är män och "f" är kvinnor.
#' @param type_of_travel
#' Landet som man har varit finns malaria eller finns inte malaria.
#' @return
#' Vilken dag kan man får ger blod.
give_blood <- function(lasttime,holiday,sex,type_of_travel){
  #skapa 2 variabler "datum_kan_ge_blod" är dag man kan ger blod och "extra_time"
  #är tid måste man addera till efter senaste gången.
  datum_kan_ge_blod = lasttime
  extra_time = 0
  #Skapa en vector som innehåller 2 olika können.
  type_sex = c("m","f")
  #Kolla alla falla med alla agrumment för att får sist resultat.
  if(holiday == "hemma"){
    if (sex == type_sex[1]){
      extra_time = months(3)
      datum_kan_ge_blod = lasttime + extra_time
      if(wday(datum_kan_ge_blod) == 7){
        extra_time = extra_time + days(2)
        datum_kan_ge_blod = lasttime + extra_time
      }else if(wday(datum_kan_ge_blod) == 1){
        extra_time = extra_time + days(1)
        datum_kan_ge_blod = lasttime + extra_time
      }
    }else if(sex == type_sex[2]){
      extra_time = months(4)
      datum_kan_ge_blod = lasttime + extra_time
      if(wday(datum_kan_ge_blod) == 7){
        extra_time = extra_time + days(2)
        datum_kan_ge_blod = lasttime + extra_time
      }else if(wday(datum_kan_ge_blod) == 1 ){
        extra_time = extra_time + days(1)
        datum_kan_ge_blod = lasttime + extra_time
      }
    }
  }else{
    if(type_of_travel == "malaria"){
      if (sex %in% type_sex){
        extra_time = months(6) + days(1)
        datum_kan_ge_blod = int_end(holiday) + extra_time
        if(wday(datum_kan_ge_blod) == 7 ){
          extra_time = extra_time + days(2)
          datum_kan_ge_blod = int_end(holiday) + extra_time
        }else if(wday(datum_kan_ge_blod) == 1){
          extra_time = extra_time + days(1)
          datum_kan_ge_blod = int_end(holiday) + extra_time
        }
      }
    }else if(type_of_travel == "other"){
      if (sex == type_sex[1]){
        extra_time = weeks(4) + days(1)
        datum_kan_ge_blod = int_end(holiday) + extra_time
        if(wday(datum_kan_ge_blod) == 7){
          extra_time = extra_time + days(2)
          datum_kan_ge_blod = int_end(holiday) + extra_time
        }else if(wday(datum_kan_ge_blod) == 1){
          extra_time = extra_time + days(1)
          datum_kan_ge_blod = int_end(holiday) + extra_time
        }
        if(as.duration(interval(lasttime,datum_kan_ge_blod)) < months(3)){
          datum_kan_ge_blod = lasttime + months(3)
        }else{
          datum_kan_ge_blod = datum_kan_ge_blod
        }
      }else if(sex == type_sex[2]){
        extra_time = weeks(4) + days(1)
        datum_kan_ge_blod = int_end(holiday) + extra_time
        if(wday(datum_kan_ge_blod) == 7){
          extra_time = extra_time + days(2)
          datum_kan_ge_blod = int_end(holiday) + extra_time
        }else if(wday(datum_kan_ge_blod) == 1){
          extra_time = extra_time + days(1)
          datum_kan_ge_blod = int_end(holiday) + extra_time
        }
        if(as.duration(interval(lasttime,datum_kan_ge_blod)) < months(4)){
          datum_kan_ge_blod = lasttime + months(4)
        }else{
          datum_kan_ge_blod = datum_kan_ge_blod
        }
      }
    }
  }
  #returnera resultat men format som nedan.
  return(format(datum_kan_ge_blod,format = "year=%Y month=%b day=%d weekday=%A"))
}

library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl3.yml")
show_tasks()
mark_my_assignment()