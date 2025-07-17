Namn <- "Viet Tien Trinh"
LiuId <- "vietr933"



#uppgift1
#' @Title blood_match (blood_match())
#' @description
#' Baskrivening an funktion: En funktion som tala om ifall en person som kan ta emot en annan 
#' persons blod baserat på deras blodgrupp.
#' @param patients 
#' Argument av funktion är patients och argument måste vara en list som innehåller 2 listor "giver"
#' och "receiver". Inom varje listor som innehålla 2 variabler "AB0" och "rh". 
#' @return
#' Funktionen måste returnera resultat att de är match eller inte match eller visar message varning
#' om väderna av argument är orimligt eller argument är inte vara en list.
blood_match <- function(patients){
  #Kolla så att patients är en list om det är inte kommer det att visa warning "Not a list".
  if(!is.list(patients)){
    stop("Not a list")
  }
  #Skapa 2 vektorer typ är 4 olika blodtyp"A,B,AB,0" och rh_system är 2 olika tillstånd "+" eller "-".
  typ = c("A","B","AB","0")
  rh_system = c("+","-")
  #Skapa 4 variabler som peka till givers AB0, rh och receivers ABO, rh.
  AB0_giver = patients$giver$AB0
  AB0_receiver = patients$receiver$AB0
  rh_giver = patients$giver$rh
  rh_receiver = patients$receiver$rh
  #Kolla så att Ab0s och rhs värder som innehållas i 2 vektorerna typ och rh_system, om inte kommer det att visa
  #warning "Wrong bloodtype".
  if((AB0_giver %in% typ) && (AB0_receiver %in% typ) && (rh_giver %in% rh_system) && (rh_receiver %in% rh_system)){
    TRUE
    }else{
      stop("Wrong bloodtype")
    }
  #Skapa en funktion RH_system() som testa rhs värderna är stämmer.
  RH_system <- function(rh_giver, rh_receiver){
    if(rh_receiver == "+"){
      print("They are a match")
    }else if((rh_receiver == "-") && (rh_giver %in% c("-"))){
      print("They are a match")
    }else{
      print("They are not a match, Incompatible rh")
    }
  }
  #Testa AB0s värderna är stämmer samt kombinera AB0 och rh genom att anropa funktion RH_system(). 
  if((AB0_giver == "A") && (AB0_receiver %in% c("A","AB"))){
    RH_system(rh_giver, rh_receiver)
  }else if((AB0_giver == "B") && (AB0_receiver %in% c("B","AB"))){
    RH_system(rh_giver, rh_receiver)
  }else if((AB0_giver == "AB") && (AB0_receiver %in% c("AB"))){
    RH_system(rh_giver, rh_receiver)
  }else if(AB0_giver == "0"){
    RH_system(rh_giver, rh_receiver)
  }else{print("They are not a match, Incompatible AB0")}
}



#uppgift2
#' @Title hilber_matrix (hilbert_matrix())
#' @description
#' Beskrivning av funktion: en funktion som skapa godtyckligt stora hilbermatriser.
#' @param nrow 
#' Anatl rader av matrix.
#' @param ncol 
#' Antal kolumn av matrix.
#' @return
#' Funktionen kommer att visar en hilbertmatris med varje element i matrisen är definerat som Hij = 1/(i+j-1). 
hilbert_matrix <- function(nrow,ncol){
  #Skapa en noll matrix H med antal rader är nrow och antal kolumn ncol.
  H <- matrix(0,nrow,ncol)
  #Skapa en nästslad loop med 2 variabler i och j med i börja från 1 till nrow och 
  #j börja från 1 till ncol.
  for (i in 1:nrow) {
    for (j in 1:ncol){
      #Byte alla element i matrix med nytt värder med format ( 1/(i+j-1) ). 
      H[i,j] = round(1/(i+j-1),digits = 5)
    }
  }
  #returnera matrix H.
  return(H)
}



library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl2.yml")
show_tasks()
mark_my_assignment()
