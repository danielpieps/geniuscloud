library(magrittr)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(wordcloud2)

new_row = data.frame(
  word = c("chorus","verse","bridge","1","2","3","4"),
  lexicon = "PIEPER"
)

stop_words <- rbind(stop_words, new_row)

song_cloud <- function(link, back_ground){
  song_name <- strsplit(link,".com/")[[1]][2]
  genius <- read_html(link) %>%
    html_nodes("p") %>% 
    .[[1]] %>% 
    html_text()
  song <- as.data.frame(strsplit(genius, "\n")[[1]])
  song <- song %>% mutate(line = 1:nrow(.))
  colnames(song)[1] = "text"
  song$text <- as.character(song$text)
  (song_tidy <- song %>% unnest_tokens(word, text))
  song_tidy <- song_tidy %>% anti_join(stop_words)
  song_count <- song_tidy %>% count(word, sort = TRUE)
  colnames(song_count)[2] = "freq"
  wordcloud2(song_count, fontFamily = "Times",color = "random-light", backgroundColor = back_ground, size = 1, minRotation = -pi/2, maxRotation = -pi/2)
}

song_cloud("https://genius.com/Drake-passionfruit-lyrics","white")
