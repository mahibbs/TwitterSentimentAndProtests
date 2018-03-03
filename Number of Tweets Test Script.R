#Script for # of Tweets hypothesis, Libya as sample
tweets.by.date <- all %>%
      group_by(date, country) %>%
      summarize(Tweets = length(country))
libya.by.date <- libya.1 %>%
  group_by(date) %>%
  summarize(Tweets = length(country))
#Graph it
ggplot(libya.by.date, aes(x = date, y = Tweets)) +
  geom_line()
ggplot(protests.libya, aes(x = Date, y = Protest)) +
    geom_line()
ggplot(tweets.protest.libya, aes(x = Date)) +
 geom_line(aes(y=Protest, fill="Protests")) +
 geom_line(aes(y=Tweets, fill="Tweets"))
 
#Merge with protests, watch for funky date classification
protests.libya$Date <- mdy(protests.libya$Date)
names(libya.by.date)[1] <- c("Date")
tweets.protest.libya <- inner_join(libya.by.date, protests.libya)

#Summary Statistics
cor(tweets.protest.libya$Tweets, tweets.protest.libya$Protest)
twe.pro.sub <- subset(tweets.protest.libya, select = c("Tweets", "Protest"))
summary(twe.pro.sub)
lm(Protest ~ Tweets, data = tweets.protest.libya)%>%
  summary


#Syria
syria.by.date <- syria.1 %>%
  + group_by(date)%>%
  + summarize(Tweets = length(country))
ggplot(syria.by.date, aes(x = date, y = Tweets)) +
  + geom_line()
protests.syria$Date <- mdy(protests.syria$Date)
names(syria.by.date)[1] <- c("Date")
tweets.protest.syria <- inner_join(syria.by.date, protests.syria)
ggplot(tweets.protest.syria, aes(Date)) + 
    geom_line(aes(y = Tweets)) +
    geom_line(aes(y = Protest))
ggplot(protests.syria, aes(x = Date, y = Protest)) +
  geom_line()
lm(Protest ~ Tweets, data = tweets.protest.syria)%>%
  summary

#Egypy
egypt.by.date <- egypt.1 %>%
  + group_by(date)%>%
  + summarize(Tweets = length(country))
protests.egypt$Date <- mdy(protests.egypt$Date)
names(egypt.by.date)[1] <- c("Date")
ggplot(egypt.by.date, aes(x = Date, y = Tweets)) +
  geom_line()
tweets.protest.egypt <- inner_join(egypt.by.date, protests.egypt)
ggplot(tweets.protest.egypt, aes(Date)) + 
  geom_line(aes(y = Tweets)) +
  geom_line(aes(y = Protest))
ggplot(protests.egypt, aes(x = Date, y = Protest)) +
   geom_line()
lm(Protest ~ Tweets, data = tweets.protest.egypt)%>%
  summary

#Tunisia
tunisia.by.date <- tunisia.1 %>%
  group_by(date) %>%
  summarize(Tweets = length(country))
protests.tunisia$Date <- mdy(protests.tunisia$Date)
names(tunisia.by.date)[1] <- c("Date")
ggplot(tunisia.by.date, aes(x = Date, y = Tweets)) +
  geom_line()
tweets.protest.tunisia <- inner_join(tunisia.by.date, protests.tunisia)
ggplot(tweets.protest.tunisia, aes(Date)) + 
  geom_line(aes(y = Tweets)) +
  geom_line(aes(y = Protest))
lm(Protest ~ Tweets, data = tweets.protest.tunisia)%>%
  summary
