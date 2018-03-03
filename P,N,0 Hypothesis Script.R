require(foreign)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(readr)
require(scales)
require(tidytext)
require(gtools)
require(lubridate)
require(tidyverse)

require(plyr)

#Script for (P,N,0) Prob distribution hypotheis test 
#Libya as test case
hyp2.libya.arabic <- libya.1
hyp2.libya.arabic <- mutate(hyp2.libya.arabic, tracking_ID = 1:n())
hyp2.libya.arabic$words <- NA
hyp2.libya.arabic$words <- as.character(hyp2.libya.arabic$words)
hyp2.libya.arabic <- unnest_tokens(hyp2.libya.arabic, words, text)
hyp2.libya.arabic <- inner_join(hyp2.libya.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))

hyp2.libya.english <- libya.1
hyp2.libya.english <- mutate(hyp2.libya.english, tracking_ID = 1:n())
hyp2.libya.english$words <- NA
hyp2.libya.english$words <- as.character(hyp2.libya.english$words)
hyp2.libya.english <- unnest_tokens(hyp2.libya.english, words, text)
hyp2.libya.english <- inner_join(hyp2.libya.english, NRC_English, by = c("words" = "word"))


#Summarize the number of positive and negative word occurances each day
out.hyp2.libya <- hyp2.libya.arabic %>%
  group_by(date)%>%
  summarise(PropNegative=sum(Negative), PropPositive=sum(Positive), PropNeutral=sum(Neutral))

out.hyp2.libya.english <- hyp2.libya.english %>%
  group_by(date) %>%
  summarise(Negative=sum(Negative), Positive=sum(Positive))

#Find Neutral english word count
temp.hyp2 <-hyp2.libya.english %>%
  group_by(date) %>%
  group_by(tracking_ID) %>%
  mutate(wordcount = n_distinct(words))
wordcount.libya.english <- temp.hyp2[c(13,14,27)]
wordcount.libya.english <- distinct(wordcount.libya.english)
wordcount.libya.english <- wordcount.libya.english %>%
  group_by(date)%>%
  mutate(Count=sum(wordcount))
wordcount.libya.english$tracking_ID <- NULL
wordcount.libya.english$wordcount <- NULL
wordcount.libya.english <- distinct(wordcount.libya.english)
out.hyp2.libya.english <- left_join(out.hyp2.libya.english, wordcount.libya.english)
out.hyp2.libya.english$Neutral.English <- out.hyp2.libya.english$Count - (out.hyp2.libya.english$Negative.English + out.hyp2.libya.english$Positive.English)
out.hyp2.libya.english$Neutral.English[out.hyp2.libya.english$Neutral.English < 0] <- 0

#Join Arabic and English P/N/0
temp.hyp2 <- inner_join(out.hyp2.libya, out.hyp2.libya.english)
temp.hyp2$Positive <- temp.hyp2$PropPositive + temp.hyp2$Positive.English
temp.hyp2$Negative <- temp.hyp2$PropNegative + temp.hyp2$Negative.English
temp.hyp2$Neutral <- temp.hyp2$PropNeutral + temp.hyp2$Neutral.English
PN0.libya <- temp.hyp2
names(PN0.libya)[2:4] <- c("Negative.Arabic", "Positive.Arabic", "Neutral.Arabic")
PN0.libya$AllWords <- PN0.libya$Positive + PN0.libya$Negative + PN0.libya$Neutral
PN0.libya$PropPositive <- PN0.libya$Positive / PN0.libya$AllWords
PN0.libya$PropNegative <- PN0.libya$Negative / PN0.libya$AllWords
PN0.libya$PropNeutral <- PN0.libya$Neutral / PN0.libya$AllWords

#Graph it
ggplot(PN0.libya, aes(date)) +
  geom_line(aes(y = Positive.Arabic, col="positive")) +
  geom_line(aes(y = Negative.Arabic, col="negative")) +
  ggtitle("Positive and Negative Word Counts in Libya: Arabic Only")

ggplot(PN0.libya, aes(date)) +
  geom_line(aes(y = Positive, col="positive")) +
  geom_line(aes(y = Negative, col="negative")) +
  ggtitle("Positive and Negative Word Counts in Libya: Arabic and English")

ggplot(PN0.libya, aes(date)) +
  geom_line(aes(y = PropPositive, col="positive")) +
  geom_line(aes(y = PropNegative, col="negative")) +
  ggtitle("Proportion of Positive and Negative Words: Arabic and English")

#Syria
hyp2.syria.arabic <- syria.1
hyp2.syria.arabic <- mutate(hyp2.syria.arabic, tracking_ID = 1:n())
hyp2.syria.arabic$words <- NA
hyp2.syria.arabic$words <- as.character(hyp2.syria.arabic$words)
hyp2.syria.arabic <- unnest_tokens(hyp2.syria.arabic, words, text)
hyp2.syria.arabic <- inner_join(hyp2.syria.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))

hyp2.syria.english <- syria.1
hyp2.syria.english <- mutate(hyp2.syria.english, tracking_ID = 1:n())
hyp2.syria.english$words <- NA
hyp2.syria.english$words <- as.character(hyp2.syria.english$words)
hyp2.syria.english <- unnest_tokens(hyp2.syria.english, words, text)
hyp2.syria.english <- inner_join(hyp2.syria.english, NRC_English, by = c("words" = "word"))

out.hyp2.syria <- hyp2.syria.arabic %>%
  group_by(date)%>%
  summarise(Negative.arabic=sum(Negative), Positive.arabic=sum(Positive), Neutral.arabic=sum(Neutral))
out.hyp2.syria.english <- hyp2.syria.english %>%
  group_by(date) %>%
  summarise(Negative.english=sum(Negative), Positive.english=sum(Positive))

temp.hyp2 <-hyp2.syria.english %>%
  group_by(date) %>%
  group_by(tracking_ID) %>%
  mutate(wordcount = n_distinct(words))
wordcount.syria.english <- temp.hyp2[c(13,14,27)]
wordcount.syria.english <- distinct(wordcount.syria.english)
wordcount.syria.english <- wordcount.syria.english %>%
  group_by(date)%>%
  mutate(Count=sum(wordcount))
wordcount.syria.english$tracking_ID <- NULL
wordcount.syria.english$wordcount <- NULL
wordcount.syria.english <- distinct(wordcount.syria.english)
out.hyp2.syria.english <- left_join(out.hyp2.syria.english, wordcount.syria.english)
out.hyp2.syria.english$Neutral.english <- out.hyp2.syria.english$Count - (out.hyp2.syria.english$Negative.english + out.hyp2.syria.english$Positive.english)
out.hyp2.syria.english$Neutral.english[out.hyp2.syria.english$Neutral.english < 0] <- 0

temp.hyp2 <- inner_join(out.hyp2.syria, out.hyp2.syria.english)
temp.hyp2$Positive <- temp.hyp2$Positive.arabic + temp.hyp2$Positive.english
temp.hyp2$Negative <- temp.hyp2$Negative.arabic + temp.hyp2$Negative.english
temp.hyp2$Neutral <- temp.hyp2$Neutral.arabic + temp.hyp2$Neutral.english
PN0.syria <- temp.hyp2
PN0.syria$AllWords <- PN0.syria$Positive + PN0.syria$Negative + PN0.syria$Neutral
PN0.syria$PropPositive <- PN0.syria$Positive / PN0.syria$AllWords
PN0.syria$PropNegative <- PN0.syria$Negative / PN0.syria$AllWords
PN0.syria$PropNeutral <- PN0.syria$Neutral / PN0.syria$AllWords
PN0.syria$Count <- NULL

ggplot(PN0.syria, aes(date)) +
  geom_line(aes(y = Positive, col="positive")) +
  geom_line(aes(y = Negative, col="negative")) +
  ggtitle("Positive and Negative Word Counts in Syria: Arabic and English")

ggplot(PN0.syria, aes(date)) +
  geom_line(aes(y = PropPositive, col="positive")) +
  geom_line(aes(y = PropNegative, col="negative")) +
  ggtitle("Proportion of Positive and Negative Words in Syria: Arabic and English")

#Egypt
hyp2.egypt.arabic <- egypt.1
hyp2.egypt.arabic <- mutate(hyp2.egypt.arabic, tracking_ID = 1:n())
hyp2.egypt.arabic$words <- NA
hyp2.egypt.arabic$words <- as.character(hyp2.egypt.arabic$words)
hyp2.egypt.arabic <- unnest_tokens(hyp2.egypt.arabic, words, text)
hyp2.egypt.arabic <- inner_join(hyp2.egypt.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))

hyp2.egypt.english <- egypt.1
hyp2.egypt.english <- mutate(hyp2.egypt.english, tracking_ID = 1:n())
hyp2.egypt.english$words <- NA
hyp2.egypt.english$words <- as.character(hyp2.egypt.english$words)
hyp2.egypt.english <- unnest_tokens(hyp2.egypt.english, words, text)
hyp2.egypt.english <- inner_join(hyp2.egypt.english, NRC_English, by = c("words" = "word"))

out.hyp2.egypt <- hyp2.egypt.arabic %>%
  group_by(date)%>%
  summarise(Negative.arabic=sum(Negative), Positive.arabic=sum(Positive), Neutral.arabic=sum(Neutral))
out.hyp2.egypt.english <- hyp2.egypt.english %>%
  group_by(date) %>%
  summarise(Negative.english=sum(Negative), Positive.english=sum(Positive))

#Michael
temp.hyp2 <-hyp2.egypt.english %>% slice(1:1e5) %>%
  group_by(tracking_ID) %>%
  mutate(wordcount = n_distinct(words)) %>% ungroup()
View(temp.hyp2)

n.split <- round(nrow(hyp2.egypt.english) / 1e5)
split.labels <- rep(1:n.split, each=1e5)
split.labels <- split.labels[1:nrow(hyp2.egypt.english)]
split.hyp2.egypt.english <- split(x=hyp2.egypt.english, f=split.labels)
for (i in 1:length(split.hyp2.egypt.english)) {
  split.hyp2.egypt.english[[i]] <- split.hyp2.egypt.english[[i]] %>% group_by(tracking_ID) %>%
    mutate(wordcount = n_distinct(words)) %>% ungroup()
  print(sprintf("Done with iteration %d", i))
}

temp.hyp2.egypt.english <- do.call(rbind, split.hyp2.egypt.english)
temp3 <- temp.hyp2.egypt.english %>% group_by(date) %>% mutate(daily_wordcount=sum(wordcount)) 
wordcount.egypt.english <- subset(temp3, select = c(date, tracking_ID, daily_wordcount))
wordcount.egypt.english <- distinct(wordcount.egypt.english)
wordcount.egypt.english$tracking_ID <- NULL
wordcount.egypt.english <- distinct(wordcount.egypt.english)
out.hyp2.egypt.english <- left_join(out.hyp2.egypt.english, wordcount.egypt.english)
out.hyp2.egypt.english$Neutral.english <- out.hyp2.egypt.english$daily_wordcount - (out.hyp2.egypt.english$Negative.english + out.hyp2.egypt.english$Positive.english)

temp.hyp2 <- inner_join(out.hyp2.egypt, out.hyp2.egypt.english)
temp.hyp2$Positive <- temp.hyp2$Positive.arabic + temp.hyp2$Positive.english
temp.hyp2$Negative <- temp.hyp2$Negative.arabic + temp.hyp2$Negative.english
temp.hyp2$Neutral <- temp.hyp2$Neutral.arabic + temp.hyp2$Neutral.english
PN0.egypt <- temp.hyp2
PN0.egypt$AllWords <- PN0.egypt$Positive + PN0.egypt$Negative + PN0.egypt$Neutral
PN0.egypt$PropPositive <- PN0.egypt$Positive / PN0.egypt$AllWords
PN0.egypt$PropNegative <- PN0.egypt$Negative / PN0.egypt$AllWords
PN0.egypt$PropNeutral <- PN0.egypt$Neutral / PN0.egypt$AllWords
PN0.egypt$daily_wordcount <- NULL

ggplot(PN0.egypt, aes(date)) +
  geom_line(aes(y = Positive, col="positive")) +
  geom_line(aes(y = Negative, col="negative")) +
  ggtitle("Positive and Negative Word Counts in Egypt: Arabic and English")

ggplot(PN0.egypt, aes(date)) +
  geom_line(aes(y = PropPositive, col="positive")) +
  geom_line(aes(y = PropNegative, col="negative")) +
  ggtitle("Proportion of Positive and Negative Words in Egypt: Arabic and English")
  
#Tunisia
hyp2.tunisia.arabic <- tunisia.1
hyp2.tunisia.arabic <- mutate(hyp2.tunisia.arabic, tracking_ID = 1:n())
hyp2.tunisia.arabic$words <- NA
hyp2.tunisia.arabic$words <- as.character(hyp2.tunisia.arabic$words)
hyp2.tunisia.arabic <- unnest_tokens(hyp2.tunisia.arabic, words, text)
hyp2.tunisia.arabic <- inner_join(hyp2.tunisia.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))

hyp2.tunisia.english <- tunisia.1
hyp2.tunisia.english <- mutate(hyp2.tunisia.english, tracking_ID = 1:n())
hyp2.tunisia.english$words <- NA
hyp2.tunisia.english$words <- as.character(hyp2.tunisia.english$words)
hyp2.tunisia.english <- unnest_tokens(hyp2.tunisia.english, words, text)
hyp2.tunisia.english <- inner_join(hyp2.tunisia.english, NRC_English, by = c("words" = "word"))

hyp2.tunisia.french <- tunisia.1
hyp2.tunisia.french <- mutate(hyp2.tunisia.french, tracking_ID = 1:n())
hyp2.tunisia.french$words <- NA
hyp2.tunisia.french$words <- as.character(hyp2.tunisia.french$words)
hyp2.tunisia.french <- unnest_tokens(hyp2.tunisia.french, words, text)
hyp2.tunisia.french <- inner_join(hyp2.tunisia.french, NRC_French, by = c("words" = "French (fr)"))

out.hyp2.tunisia <- hyp2.tunisia.arabic %>%
  group_by(date)%>%
  summarise(Negative.arabic=sum(Negative), Positive.arabic=sum(Positive), Neutral.arabic=sum(Neutral))

out.hyp2.tunisia.french <- hyp2.tunisia.french %>%
  group_by(date)%>%
  summarise(Negative.french=sum(Negative), Positive.french=sum(Positive), Neutral.french=sum(Neutral))

out.hyp2.tunisia.english <- hyp2.tunisia.english %>%
  group_by(date) %>%
  summarise(Negative.english=sum(Negative), Positive.english=sum(Positive))

temp.hyp2 <-hyp2.tunisia.english %>%
  group_by(date) %>%
  group_by(tracking_ID) %>%
  mutate(wordcount = n_distinct(words))
wordcount.tunisia.english <- temp.hyp2[c(13,14,27)]
wordcount.tunisia.english <- distinct(wordcount.tunisia.english)
wordcount.tunisia.english <- wordcount.tunisia.english %>%
  group_by(date)%>%
  mutate(Count=sum(wordcount))
wordcount.tunisia.english <- distinct(wordcount.tunisia.english)
out.hyp2.tunisia.english <- left_join(out.hyp2.tunisia.english, wordcount.tunisia.english)
out.hyp2.tunisia.english$Neutral.english <- out.hyp2.tunisia.english$Count - (out.hyp2.tunisia.english$Negative.english + out.hyp2.tunisia.english$Positive.english)
out.hyp2.tunisia.english$Neutral.english[out.hyp2.tunisia.english$Neutral.english < 0] <- 0

temp.hyp2 <- inner_join(out.hyp2.tunisia, out.hyp2.tunisia.english)
temp.hyp2 <- inner_join(temp.hyp2, out.hyp2.tunisia.french)
temp.hyp2$Positive <- temp.hyp2$Positive.arabic + temp.hyp2$Positive.english + temp.hyp2$Positive.french
temp.hyp2$Negative <- temp.hyp2$Negative.arabic + temp.hyp2$Negative.english + temp.hyp2$Negative.french
temp.hyp2$Neutral <- temp.hyp2$Neutral.arabic + temp.hyp2$Neutral.english + temp.hyp2$Neutral.french
PN0.tunisia <- temp.hyp2
PN0.tunisia$AllWords <- PN0.tunisia$Positive + PN0.tunisia$Negative + PN0.tunisia$Neutral
PN0.tunisia$PropPositive <- PN0.tunisia$Positive / PN0.tunisia$AllWords
PN0.tunisia$PropNegative <- PN0.tunisia$Negative / PN0.tunisia$AllWords
PN0.tunisia$PropNeutral <- PN0.tunisia$Neutral / PN0.tunisia$AllWords

ggplot(PN0.tunisia, aes(date)) +
  geom_line(aes(y = Positive, col="positive")) +
  geom_line(aes(y = Negative, col="negative")) +
  ggtitle("Positive and Negative Word Counts in Tunisia: Arabic, English, and French")

ggplot(PN0.tunisia, aes(date)) +
  geom_line(aes(y = PropPositive, col="positive")) +
  geom_line(aes(y = PropNegative, col="negative")) +
  ggtitle("Proportion of Positive and Negative Words in Tunisia: Arabic, English, and French")


