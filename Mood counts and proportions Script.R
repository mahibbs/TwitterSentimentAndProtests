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

#Script for getting moods/day, Libya as example
mood.libya.arabic <- libya.1
mood.libya.arabic <- mutate(mood.libya.arabic, tracking_ID = 1:n())
mood.libya.arabic$words <- NA
mood.libya.arabic$words <- as.character(mood.libya.arabic$words)
mood.libya.arabic <- unnest_tokens(mood.libya.arabic, words, text)
mood.libya.arabic <- inner_join(mood.libya.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))
out.mood.libya.arabic <- mood.libya.arabic %>%
  group_by(date) %>%
  summarise(Anger.arabic=sum(Anger), Anticipation.arabic=sum(Anticipation), Disgust.arabic=sum(Disgust), Fear.arabic=sum(Fear), Joy.arabic=sum(Joy), Surprise.arabic=sum(Surprise), Sadness.arabic=sum(Sadness), Trust.arabic=sum(Trust))

mood.libya.english <- libya.1
mood.libya.english <- mutate(mood.libya.english, tracking_ID = 1:n())
mood.libya.english$words <- NA
mood.libya.english$words <- as.character(mood.libya.english$words)
mood.libya.english <- unnest_tokens(mood.libya.english, words, text)
mood.libya.english <- inner_join(mood.libya.english, NRC_English, by = c("words" = "word"))
out.mood.libya.english <- mood.libya.english %>%
  group_by(date) %>%
  summarise(Anger.english=sum(Anger), Anticipation.english=sum(Anticipation), Disgust.english=sum(Disgust), Fear.english=sum(Fear), Joy.english=sum(Joy), Surprise.english=sum(Surprise), Sadness.english=sum(Sadness), Trust.english=sum(Trust))

out.mood.libya <- left_join(out.mood.libya.english, out.mood.libya.arabic)

out.mood.libya$Word.Total <- out.mood.libya$Anger.english + out.mood.libya$Anticipation.english + out.mood.libya$Disgust.english + out.mood.libya$Fear.english + out.mood.libya$Joy.english + out.mood.libya$Surprise.english + out.mood.libya$Sadness.english + out.mood.libya$Trust.english 
out.mood.libya$Word.Total <- out.mood.libya$Word.Total + out.mood.libya$Anger.arabic + out.mood.libya$Anticipation.arabic + out.mood.libya$Disgust.arabic + out.mood.libya$Fear.arabic + out.mood.libya$Joy.arabic + out.mood.libya$Surprise.arabic + out.mood.libya$Sadness.arabic + out.mood.libya$Trust.arabic

out.mood.libya$Anger <- out.mood.libya$Anger.english + out.mood.libya$Anger.arabic
out.mood.libya$Anticipation <- out.mood.libya$Anticipation.english + out.mood.libya$Anticipation.arabic
out.mood.libya$Disgust <- out.mood.libya$Disgust.english + out.mood.libya$Disgust.arabic
out.mood.libya$Fear <- out.mood.libya$Fear.english + out.mood.libya$Fear.arabic
out.mood.libya$Joy <- out.mood.libya$Joy.english + out.mood.libya$Joy.arabic
out.mood.libya$Surprise <- out.mood.libya$Surprise.english + out.mood.libya$Surprise.arabic
out.mood.libya$Sadness <- out.mood.libya$Sadness.english + out.mood.libya$Sadness.arabic
out.mood.libya$Trust <- out.mood.libya$Trust.english + out.mood.libya$Trust.arabic

out.mood.libya$PropAnger <- out.mood.libya$Anger / out.mood.libya$Word.Total
out.mood.libya$PropAnticipation <- out.mood.libya$Anticipation / out.mood.libya$Word.Total
out.mood.libya$PropDisgust <- out.mood.libya$Disgust / out.mood.libya$Word.Total
out.mood.libya$PropFear <- out.mood.libya$Fear / out.mood.libya$Word.Total
out.mood.libya$PropJoy <- out.mood.libya$Joy / out.mood.libya$Word.Total
out.mood.libya$PropSurprise <- out.mood.libya$Surprise / out.mood.libya$Word.Total
out.mood.libya$PropSadness <- out.mood.libya$Sadness / out.mood.libya$Word.Total
out.mood.libya$PropTrust <- out.mood.libya$Trust / out.mood.libya$Word.Total

ggplot(data = out.mood.libya, aes(x=date)) +
  geom_area(aes(y=PropAnticipation, fill="Anticipation")) + 
  geom_area(aes(y=PropAnger, fill="Anger")) + 
  geom_area(aes(y=PropDisgust, fill="Disgust")) +
  geom_area(aes(y=PropTrust, fill="Trust")) +
  geom_area(aes(y=PropSadness, fill="Sadness")) +
  geom_area(aes(y=PropSurprise, fill="Surprise")) +
  geom_area(aes(y=PropJoy, fill="Joy")) +
  geom_area(aes(y=PropFear, fill="Fear")) +
  labs(title="Libya Mood Proportions", y="Proportion") +
  theme(panel.grid.minor = element_blank())

ggplot(data = out.mood.libya, aes(x=date)) +
  geom_area(aes(y=Anticipation, fill="Anticipation")) + 
  geom_area(aes(y=Anger, fill="Anger")) + 
  geom_area(aes(y=Disgust, fill="Disgust")) +
  geom_area(aes(y=Trust, fill="Trust")) +
  geom_area(aes(y=Sadness, fill="Sadness")) +
  geom_area(aes(y=Surprise, fill="Surprise")) +
  geom_area(aes(y=Joy, fill="Joy")) +
  geom_area(aes(y=Fear, fill="Fear")) +
  labs(title="Libya Mood Count", y="Wordcount") +
  theme(panel.grid.minor = element_blank())

ggplot(data = out.mood.libya, aes(x=date)) +
  geom_area(aes(y=Trust, fill="Trust")) +
  geom_area(aes(y=Fear, fill="Fear")) +
  geom_area(aes(y=Anger, fill="Anger")) + 
  geom_area(aes(y=Disgust, fill="Disgust")) +
  labs(title="Libya Mood Count", y="Wordcount") +
  theme(panel.grid.minor = element_blank())

ggplot(data = out.mood.libya, aes(x=date)) +
  geom_area(aes(y=Trust, fill="Trust")) +
  geom_area(aes(y=Anger, fill="Anger")) + 
  labs(title="Libya Mood Count", y="Wordcount") +
  theme(panel.grid.minor = element_blank())

ggplot(data = out.mood.libya, aes(x=date)) +
  geom_area(aes(y=PropFear, fill="Fear")) +
  geom_area(aes(y=PropAnger, fill="Anger")) + 
  geom_area(aes(y=PropDisgust, fill="Disgust")) +
  labs(title="Libya Mood Proportions", y="Proportion") +
  theme(panel.grid.minor = element_blank())

ggplot(data = out.mood.libya, aes(x=date)) +
  geom_area(aes(y=PropTrust, fill="Trust")) +
  geom_area(aes(y=PropAnger, fill="Anger")) + 
  labs(title="Libya Mood Proportions", y="Proportion") +
  theme(panel.grid.minor = element_blank())

#Syria
mood.syria.arabic <- syria.1
mood.syria.arabic <- mutate(mood.syria.arabic, tracking_ID = 1:n())
mood.syria.arabic$words <- NA
mood.syria.arabic$words <- as.character(mood.syria.arabic$words)
mood.syria.arabic <- unnest_tokens(mood.syria.arabic, words, text)
mood.syria.arabic <- inner_join(mood.syria.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))
out.mood.syria.arabic <- mood.syria.arabic %>%
  group_by(date) %>%
  summarise(Anger.arabic=sum(Anger), Anticipation.arabic=sum(Anticipation), Disgust.arabic=sum(Disgust), Fear.arabic=sum(Fear), Joy.arabic=sum(Joy), Surprise.arabic=sum(Surprise), Sadness.arabic=sum(Sadness), Trust.arabic=sum(Trust))

mood.syria.english <- syria.1
mood.syria.english <- mutate(mood.syria.english, tracking_ID = 1:n())
mood.syria.english$words <- NA
mood.syria.english$words <- as.character(mood.syria.english$words)
mood.syria.english <- unnest_tokens(mood.syria.english, words, text)
mood.syria.english <- inner_join(mood.syria.english, NRC_English, by = c("words" = "word"))
out.mood.syria.english <- mood.syria.english %>%
  group_by(date) %>%
  summarise(Anger.english=sum(Anger), Anticipation.english=sum(Anticipation), Disgust.english=sum(Disgust), Fear.english=sum(Fear), Joy.english=sum(Joy), Surprise.english=sum(Surprise), Sadness.english=sum(Sadness), Trust.english=sum(Trust))

out.mood.syria <- left_join(out.mood.syria.english, out.mood.syria.arabic)

out.mood.syria$Word.Total <- out.mood.syria$Anger.english + out.mood.syria$Anticipation.english + out.mood.syria$Disgust.english + out.mood.syria$Fear.english + out.mood.syria$Joy.english + out.mood.syria$Surprise.english + out.mood.syria$Sadness.english + out.mood.syria$Trust.english 
out.mood.syria$Word.Total <- out.mood.syria$Word.Total + out.mood.syria$Anger.arabic + out.mood.syria$Anticipation.arabic + out.mood.syria$Disgust.arabic + out.mood.syria$Fear.arabic + out.mood.syria$Joy.arabic + out.mood.syria$Surprise.arabic + out.mood.syria$Sadness.arabic + out.mood.syria$Trust.arabic

out.mood.syria$Anger <- out.mood.syria$Anger.english + out.mood.syria$Anger.arabic
out.mood.syria$Anticipation <- out.mood.syria$Anticipation.english + out.mood.syria$Anticipation.arabic
out.mood.syria$Disgust <- out.mood.syria$Disgust.english + out.mood.syria$Disgust.arabic
out.mood.syria$Fear <- out.mood.syria$Fear.english + out.mood.syria$Fear.arabic
out.mood.syria$Joy <- out.mood.syria$Joy.english + out.mood.syria$Joy.arabic
out.mood.syria$Surprise <- out.mood.syria$Surprise.english + out.mood.syria$Surprise.arabic
out.mood.syria$Sadness <- out.mood.syria$Sadness.english + out.mood.syria$Sadness.arabic
out.mood.syria$Trust <- out.mood.syria$Trust.english + out.mood.syria$Trust.arabic

out.mood.syria$PropAnger <-out.mood.syria$Anger / out.mood.syria$Word.Total
out.mood.syria$PropAnticipation <- out.mood.syria$Anticipation / out.mood.syria$Word.Total
out.mood.syria$PropDisgust <- out.mood.syria$Disgust / out.mood.syria$Word.Total
out.mood.syria$PropFear <- out.mood.syria$Fear / out.mood.syria$Word.Total
out.mood.syria$PropJoy <- out.mood.syria$Joy / out.mood.syria$Word.Total
out.mood.syria$PropSurprise <- out.mood.syria$Surprise / out.mood.syria$Word.Total
out.mood.syria$PropSadness <- out.mood.syria$Sadness / out.mood.syria$Word.Total
out.mood.syria$PropTrust <- out.mood.syria$Trust / out.mood.syria$Word.Total

#Tunisia
mood.tunisia.arabic <- tunisia.1
mood.tunisia.arabic <- mutate(mood.tunisia.arabic, tracking_ID = 1:n())
mood.tunisia.arabic$words <- NA
mood.tunisia.arabic$words <- as.character(mood.tunisia.arabic$words)
mood.tunisia.arabic <- unnest_tokens(mood.tunisia.arabic, words, text)
mood.tunisia.arabic <- inner_join(mood.tunisia.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))
out.mood.tunisia.arabic <- mood.tunisia.arabic %>%
  group_by(date) %>%
  summarise(Anger.arabic=sum(Anger), Anticipation.arabic=sum(Anticipation), Disgust.arabic=sum(Disgust), Fear.arabic=sum(Fear), Joy.arabic=sum(Joy), Surprise.arabic=sum(Surprise), Sadness.arabic=sum(Sadness), Trust.arabic=sum(Trust))

mood.tunisia.english <- tunisia.1
mood.tunisia.english <- mutate(mood.tunisia.english, tracking_ID = 1:n())
mood.tunisia.english$words <- NA
mood.tunisia.english$words <- as.character(mood.tunisia.english$words)
mood.tunisia.english <- unnest_tokens(mood.tunisia.english, words, text)
mood.tunisia.english <- inner_join(mood.tunisia.english, NRC_English, by = c("words" = "word"))
out.mood.tunisia.english <- mood.tunisia.english %>%
  group_by(date) %>%
  summarise(Anger.english=sum(Anger), Anticipation.english=sum(Anticipation), Disgust.english=sum(Disgust), Fear.english=sum(Fear), Joy.english=sum(Joy), Surprise.english=sum(Surprise), Sadness.english=sum(Sadness), Trust.english=sum(Trust))

mood.tunisia.french <- tunisia.1
mood.tunisia.french <- mutate(mood.tunisia.french, tracking_ID = 1:n())
mood.tunisia.french$words <- NA
mood.tunisia.french$words <- as.character(mood.tunisia.french$words)
mood.tunisia.french <- unnest_tokens(mood.tunisia.french, words, text)
mood.tunisia.french <- inner_join(mood.tunisia.french, NRC_French, by = c("words" = "French (fr)"))
out.mood.tunisia.french <- mood.tunisia.french %>%
  group_by(date) %>%
  summarise(Anger.french=sum(Anger), Anticipation.french=sum(Anticipation), Disgust.french=sum(Disgust), Fear.french=sum(Fear), Joy.french=sum(Joy), Surprise.french=sum(Surprise), Sadness.french=sum(Sadness), Trust.french=sum(Trust))

out.mood.tunisia <- left_join(out.mood.tunisia.english, out.mood.tunisia.arabic)
out.mood.tunisia <- left_join(out.mood.tunisia, out.mood.tunisia.french)

out.mood.tunisia$Word.Total <- out.mood.tunisia$Anger.english + out.mood.tunisia$Anticipation.english + out.mood.tunisia$Disgust.english + out.mood.tunisia$Fear.english + out.mood.tunisia$Joy.english + out.mood.tunisia$Surprise.english + out.mood.tunisia$Sadness.english + out.mood.tunisia$Trust.english 
out.mood.tunisia$Word.Total <- out.mood.tunisia$Word.Total + out.mood.tunisia$Anger.arabic + out.mood.tunisia$Anticipation.arabic + out.mood.tunisia$Disgust.arabic + out.mood.tunisia$Fear.arabic + out.mood.tunisia$Joy.arabic + out.mood.tunisia$Surprise.arabic + out.mood.tunisia$Sadness.arabic + out.mood.tunisia$Trust.arabic
out.mood.tunisia$Word.Total <- out.mood.tunisia$Word.Total + out.mood.tunisia$Anger.french + out.mood.tunisia$Anticipation.french + out.mood.tunisia$Disgust.french + out.mood.tunisia$Fear.french + out.mood.tunisia$Joy.french + out.mood.tunisia$Surprise.french + out.mood.tunisia$Sadness.french + out.mood.tunisia$Trust.french

out.mood.tunisia$Anger <- out.mood.tunisia$Anger.english + out.mood.tunisia$Anger.arabic + out.mood.tunisia$Anger.french
out.mood.tunisia$Anticipation <- out.mood.tunisia$Anticipation.english + out.mood.tunisia$Anticipation.arabic + out.mood.tunisia$Anticipation.french
out.mood.tunisia$Disgust <- out.mood.tunisia$Disgust.english + out.mood.tunisia$Disgust.arabic + out.mood.tunisia$Disgust.french
out.mood.tunisia$Fear <- out.mood.tunisia$Fear.english + out.mood.tunisia$Fear.arabic + out.mood.tunisia$Fear.french
out.mood.tunisia$Joy <- out.mood.tunisia$Joy.english + out.mood.tunisia$Joy.arabic + out.mood.tunisia$Joy.french
out.mood.tunisia$Surprise <- out.mood.tunisia$Surprise.english + out.mood.tunisia$Surprise.arabic + out.mood.tunisia$Surprise.french
out.mood.tunisia$Sadness <- out.mood.tunisia$Sadness.english + out.mood.tunisia$Sadness.arabic + out.mood.tunisia$Sadness.french
out.mood.tunisia$Trust <- out.mood.tunisia$Trust.english + out.mood.tunisia$Trust.arabic + out.mood.tunisia$Trust.french

out.mood.tunisia$PropAnger <-out.mood.tunisia$Anger / out.mood.tunisia$Word.Total
out.mood.tunisia$PropAnticipation <- out.mood.tunisia$Anticipation / out.mood.tunisia$Word.Total
out.mood.tunisia$PropDisgust <- out.mood.tunisia$Disgust / out.mood.tunisia$Word.Total
out.mood.tunisia$PropFear <- out.mood.tunisia$Fear / out.mood.tunisia$Word.Total
out.mood.tunisia$PropJoy <- out.mood.tunisia$Joy / out.mood.tunisia$Word.Total
out.mood.tunisia$PropSurprise <- out.mood.tunisia$Surprise / out.mood.tunisia$Word.Total
out.mood.tunisia$PropSadness <- out.mood.tunisia$Sadness / out.mood.tunisia$Word.Total
out.mood.tunisia$PropTrust <- out.mood.tunisia$Trust / out.mood.tunisia$Word.Total

#EGYPT 
mood.egypt.arabic <- egypt.1
mood.egypt.arabic <- mutate(mood.egypt.arabic, tracking_ID = 1:n())
mood.egypt.arabic$words <- NA
mood.egypt.arabic$words <- as.character(mood.egypt.arabic$words)
mood.egypt.arabic <- unnest_tokens(mood.egypt.arabic, words, text)
mood.egypt.arabic <- inner_join(mood.egypt.arabic, NRC_Arabic_Cleaned, by = c("words" = "Arabic.translation"))
out.mood.egypt.arabic <- mood.egypt.arabic %>%
  group_by(date) %>%
  summarise(Anger.arabic=sum(Anger), Anticipation.arabic=sum(Anticipation), Disgust.arabic=sum(Disgust), Fear.arabic=sum(Fear), Joy.arabic=sum(Joy), Surprise.arabic=sum(Surprise), Sadness.arabic=sum(Sadness), Trust.arabic=sum(Trust))

mood.egypt.english <- egypt.1
mood.egypt.english <- mutate(mood.egypt.english, tracking_ID = 1:n())
mood.egypt.english$words <- NA
mood.egypt.english$words <- as.character(mood.egypt.english$words)
mood.egypt.english <- unnest_tokens(mood.egypt.english, words, text)
mood.egypt.english <- inner_join(mood.egypt.english, NRC_English, by = c("words" = "word"))
out.mood.egypt.english <- mood.egypt.english %>%
  group_by(date) %>%
  summarise(Anger.english=sum(Anger), Anticipation.english=sum(Anticipation), Disgust.english=sum(Disgust), Fear.english=sum(Fear), Joy.english=sum(Joy), Surprise.english=sum(Surprise), Sadness.english=sum(Sadness), Trust.english=sum(Trust))

out.mood.egypt <- left_join(out.mood.egypt.english, out.mood.egypt.arabic)

out.mood.egypt$Word.Total <- out.mood.egypt$Anger.english + out.mood.egypt$Anticipation.english + out.mood.egypt$Disgust.english + out.mood.egypt$Fear.english + out.mood.egypt$Joy.english + out.mood.egypt$Surprise.english + out.mood.egypt$Sadness.english + out.mood.egypt$Trust.english 
out.mood.egypt$Word.Total <- out.mood.egypt$Word.Total + out.mood.egypt$Anger.arabic + out.mood.egypt$Anticipation.arabic + out.mood.egypt$Disgust.arabic + out.mood.egypt$Fear.arabic + out.mood.egypt$Joy.arabic + out.mood.egypt$Surprise.arabic + out.mood.egypt$Sadness.arabic + out.mood.egypt$Trust.arabic

out.mood.egypt$Anger <- out.mood.egypt$Anger.english + out.mood.egypt$Anger.arabic
out.mood.egypt$Anticipation <- out.mood.egypt$Anticipation.english + out.mood.egypt$Anticipation.arabic
out.mood.egypt$Disgust <- out.mood.egypt$Disgust.english + out.mood.egypt$Disgust.arabic
out.mood.egypt$Fear <- out.mood.egypt$Fear.english + out.mood.egypt$Fear.arabic
out.mood.egypt$Joy <- out.mood.egypt$Joy.english + out.mood.egypt$Joy.arabic
out.mood.egypt$Surprise <- out.mood.egypt$Surprise.english + out.mood.egypt$Surprise.arabic
out.mood.egypt$Sadness <- out.mood.egypt$Sadness.english + out.mood.egypt$Sadness.arabic
out.mood.egypt$Trust <- out.mood.egypt$Trust.english + out.mood.egypt$Trust.arabic 

out.mood.egypt$PropAnger <-out.mood.egypt$Anger / out.mood.egypt$Word.Total
out.mood.egypt$PropAnticipation <- out.mood.egypt$Anticipation / out.mood.egypt$Word.Total
out.mood.egypt$PropDisgust <- out.mood.egypt$Disgust / out.mood.egypt$Word.Total
out.mood.egypt$PropFear <- out.mood.egypt$Fear / out.mood.egypt$Word.Total
out.mood.egypt$PropJoy <- out.mood.egypt$Joy / out.mood.egypt$Word.Total
out.mood.egypt$PropSurprise <- out.mood.egypt$Surprise / out.mood.egypt$Word.Total
out.mood.egypt$PropSadness <- out.mood.egypt$Sadness / out.mood.egypt$Word.Total
out.mood.egypt$PropTrust <- out.mood.egypt$Trust / out.mood.egypt$Word.Total
