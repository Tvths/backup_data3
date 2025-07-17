#### Programmering i R ####
#### Datorlaboration  [4] ####
Namn <- c("Vinh Phat Thong", "Viet Tien Trinh")
LiuId <- c("vinth530", "vietr933")
Grupp <- "grupp13"

library(downloader)
library(stringr)

wordcount <- function(text){
  tarbort_punc <- str_replace_all(text,"[[:punct:]]","")
  gemerner <- str_to_lower(tarbort_punc)
  Str_split <- str_split(gemerner,"\\s+")
  Un_list <- unlist(Str_split)
  count_frequency <- table(Un_list)
  max <- count_frequency[1]
  for(i in 1:length(count_frequency)){
    if(max < count_frequency[i]){
      max = count_frequency[i]
    }
  }
  most_common_word <- names(max)
  total_times <- as.integer(max)
  countword_df <- data.frame(word <- names(count_frequency),
                             freq <- as.integer(count_frequency))
  colnames(countword_df) <- c("word", "freq")
  message("The most common word is '", most_common_word, "' and it occurred ", total_times, " times.")
  return(countword_df)
}

library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/KursRprgm2/main/Labs/Tests/inl4.yml")
show_tasks()
mark_my_assignment()
 