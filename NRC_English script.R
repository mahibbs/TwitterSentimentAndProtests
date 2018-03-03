require(foreign)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(readr)
require(scales)
require(tidytext)
require(gtools)
require(plyr)
require(lubridate)
require(tidyverse)

NRC_English <- get_sentiments("nrc")

#Cleaning the NRC English Lexicon
#Rows as columns
NRC_English$Positive <- ifelse(NRC_English$sentiment == "positive", 1, 0)
NRC_English$Negative <- ifelse(NRC_English$sentiment == "negative", 1, 0)
NRC_English$Trust <- ifelse(NRC_English$sentiment == "trust", 1, 0)
NRC_English$Fear <- ifelse(NRC_English$sentiment == "fear", 1, 0)
NRC_English$Anger <- ifelse(NRC_English$sentiment == "anger", 1, 0)
NRC_English$Sadness <- ifelse(NRC_English$sentiment == "sadness", 1, 0)
NRC_English$Surprise <- ifelse(NRC_English$sentiment == "surprise", 1, 0)
NRC_English$Disgust <- ifelse(NRC_English$sentiment == "disgust", 1, 0)
NRC_English$Joy <- ifelse(NRC_English$sentiment == "joy", 1, 0)
NRC_English$Anticipation <- ifelse(NRC_English$sentiment == "anticipation", 1, 0)

#How I created a neutral value (varies by hypotheis/country)
hyp2.libya.english <- libya.1
hyp2.libya.english <- mutate(hyp2.libya.english, tracking_ID = 1:n())
hyp2.libya.english$words <- NA
hyp2.libya.english$words <- as.character(hyp2.libya.english$words)
hyp2.libya.english <- unnest_tokens(hyp2.libya.english, words, text)
hyp2.libya.english <- inner_join(hyp2.libya.english, NRC_English, by = c("words" = "word"))
temp.hyp2 <-hyp2.libya.english %>%
  group_by(date) %>%
  group_by(tracking_ID) %>%
  mutate(wordcount = n_distinct(words))
wordcount.libya.english <- temp.hyp2[c(13,14,27)]
wordcount.libya.english <- wordcount.libya.english %>%
  group_by(date)%>%
  mutate(Count=sum(wordcount))
wordcount.libya.english$tracking_ID <- NULL
wordcount.libya.english$wordcount <- NULL
wordcount.libya.english <- distinct(wordcount.libya.english)
out.hyp2.libya.english <- left_join(out.hyp2.libya.english, wordcount.libya.english)
out.hyp2.libya.english$Neutral.English <- out.hyp2.libya.english$Count - (out.hyp2.libya.english$Negative.English + out.hyp2.libya.english$Positive.English)
out.hyp2.libya.english$Neutral.English[out.hyp2.libya.english$Neutral.English < 0] <- 0

