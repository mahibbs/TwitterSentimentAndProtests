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
require(zoo)

require(plyr)

#testing proportions of PN0 sentiment vs protest magnitude 

#Libya
load("protests.PN0.Libya.RData")

ggplot(protests.PN0.libya, aes(x = PropNegative)) +
  geom_line(aes(y=Protest)) +
  labs(title="Libya: Prop Negative vs Protest Magnitude", x="Proportion", y="Number of Protests")

#Tunisia
load("protests.tunisia.RData")
load("tunisia.1.RData")
tunisia.by.date <- tunisia.1 %>%
  group_by(date) %>%
  summarize(Tweets = length(country))
names(protests.tunisia)[1] <- c("date")
protests.tunisia$date <- mdy(protests.tunisia$date)
tweets.protest.tunisia <- inner_join(tunisia.by.date, protests.tunisia)
load("PN0.tunisia.RData")
protests.PN0.tunisia <- inner_join(tweets.protest.tunisia, PN0.tunisia)

ggplot(protests.PN0.tunisia, aes(x = PropNegative)) +
  geom_line(aes(y=Protest)) +
  labs(title="Tunisia: Prop Negative vs Protest Magnitude", x="Proportion", y="Number of Protests")

ggplot(protests.PN0.tunisia, aes(x = Protest)) +
  geom_line(aes(y=PropNegative)) +
  labs(title="Tunisia: Protest Magnitude vs Prop Negative", x="Number of Protests", y="Proportion") #Nonsensicle 

#Syria
load("protests.syria.RData")
load("syria.1.RData")
syria.by.date <- syria.1 %>%
  group_by(date) %>%
  summarize(Tweets = length(country))
names(protests.syria)[1] <- c("date")
protests.syria$date <- mdy(protests.syria$date)
tweets.protest.syria <- inner_join(syria.by.date, protests.syria)
load("PN0.syria.RData")
protests.PN0.syria <- inner_join(tweets.protest.syria, PN0.syria)

ggplot(protests.PN0.syria, aes(x = PropNegative)) +
  geom_line(aes(y=Protest)) +
  labs(title="Syria: Prop Negative vs Protest Magnitude", x="Proportion", y="Number of Protests")

ggplot(protests.PN0.syria, aes(x = Protest)) +
  geom_line(aes(y=PropNegative)) +
  labs(title="Syria: Protest Magnitude vs Prop Negative", x="Number of Protests", y="Proportion") #Nonsensicle 

#Egypt 
load("protests.egypt.RData")
load("egypt.1.RData")
load("PN0.egypt.RData")
egypt.by.date <- egypt.1 %>%
  group_by(date) %>%
  summarize(Tweets = length(country))
names(protests.egypt)[1] <- c("date")
protests.egypt$date <- mdy(protests.egypt$date)
tweets.protest.egypt <- inner_join(egypt.by.date, protests.egypt)
protests.PN0.egypt <- inner_join(tweets.protest.egypt, PN0.egypt)

ggplot(protests.PN0.egypt, aes(x = PropNegative)) +
  geom_line(aes(y=Protest)) +
  labs(title="Egypt: Prop Negative vs Protest Magnitude", x="Proportion", y="Number of Protests")

#Testing mean prop of neg tweets for pre/during/post protest regime
#Libya
#Pre-protest mean prop of neg tweets
with(protests.PN0.libya, mean(PropNegative[day <= 107]))

#Protest mean prop of neg tweets
with(protests.PN0.libya, mean(PropNegative[day > 107 & day <= 179]))

#Post-protest mean prop of neg tweets
with(protests.PN0.libya, mean(PropNegative[day > 180]))

#Split evenly into quadrants
with(protests.PN0.libya, mean(PropNegative[day <= 103]))
with(protests.PN0.libya, mean(PropNegative[day > 103 & day < 206]))
with(protests.PN0.libya, mean(PropNegative[day > 207 & day < 310]))
with(protests.PN0.libya, mean(PropNegative[day > 311]))

#Descriptive statistics 
describe(protests.PN0.libya$PropNegative)
describe(protests.PN0.libya$Negative)

#Pre/During/Post protest mean PropPos
with(protests.PN0.libya, mean(PropPositive[day <= 107]))
with(protests.PN0.libya, mean(PropPositive[day > 107 & day <= 179]))
with(protests.PN0.libya, mean(PropPositive[day > 179]))

with(protests.PN0.libya, mean(Positive[day <= 107]))
with(protests.PN0.libya, mean(Positive[day > 107 & day <= 179]))
with(protests.PN0.libya, mean(Positive[day > 179]))

#Descriptive statistics
describe(protests.PN0.libya$PropPositive)
describe(protests.PN0.libya$Positive)


#Tunisia
#Pre-protest mean prop of neg and just neg tweets
with(protests.PN0.tunisia, mean(PropNegative[day <= 44]))
with(protests.PN0.tunisia, mean(Negative[day <= 44]))

#Protest mean prop of neg tweets
with(protests.PN0.tunisia, mean(PropNegative[day > 44 & day <= 145]))
with(protests.PN0.tunisia, mean(Negative[day > 44 & day <= 145]))

#Post-protest mean prop of neg tweets
with(protests.PN0.tunisia, mean(PropNegative[day > 145]))
with(protests.PN0.tunisia, mean(Negative[day > 145]))

#Split evenly into quadrants
with(protests.PN0.tunisia, mean(PropNegative[day <= 104]))
with(protests.PN0.tunisia, mean(PropNegative[day > 104 & day < 207]))
with(protests.PN0.tunisia, mean(PropNegative[day > 208 & day < 311]))
with(protests.PN0.tunisia, mean(PropNegative[day > 312]))

#Descriptive statistics 
describe(protests.PN0.tunisia$PropNegative)
describe(protests.PN0.tunisia$Negative)

#Pre/During/Post protest mean PropPos
with(protests.PN0.tunisia, mean(PropPositive[day <= 44]))
with(protests.PN0.tunisia, mean(PropPositive[day > 44 & day <= 145]))
with(protests.PN0.tunisia, mean(PropPositive[day > 145]))

with(protests.PN0.tunisia, mean(Positive[day <= 44]))
with(protests.PN0.tunisia, mean(Positive[day > 44 & day <= 145]))
with(protests.PN0.tunisia, mean(Positive[day > 145]))

#Descriptive statistics
describe(protests.PN0.tunisia$PropPositive)
describe(protests.PN0.tunisia$Positive)


#Syria
#Pre-protest mean prop of neg and just neg tweets
with(protests.PN0.syria, mean(PropNegative[day <= 129]))
with(protests.PN0.syria, mean(Negative[day <= 129]))

#Protest mean prop of neg tweets
with(protests.PN0.syria, mean(PropNegative[day > 129 & day <= 299]))
with(protests.PN0.syria, mean(Negative[day > 129 & day <= 299]))

#Post-protest mean prop of neg tweets
with(protests.PN0.syria, mean(PropNegative[day >= 300]))
with(protests.PN0.syria, mean(Negative[day >= 300]))

#Split evenly into quadrants
with(protests.PN0.syria, mean(PropNegative[day <= 104]))
with(protests.PN0.syria, mean(PropNegative[day > 104 & day <= 207]))
with(protests.PN0.syria, mean(PropNegative[day >= 208 & day <= 311]))
with(protests.PN0.syria, mean(PropNegative[day >= 312]))

#Descriptive statistics 
describe(protests.PN0.syria$PropNegative)
describe(protests.PN0.syria$Negative)

#Pre/protest/post mean prop of pos 
with(protests.PN0.syria, mean(PropPositive[day <= 129]))
with(protests.PN0.syria, mean(PropPositive[day > 129 & day <= 299]))
with(protests.PN0.syria, mean(PropPositive[day >= 300]))

#Pre/protest/post neg wordcount
with(protests.PN0.syria, mean(Positive[day <= 129]))
with(protests.PN0.syria, mean(Positive[day > 129 & day <= 299]))
with(protests.PN0.syria, mean(Positive[day >= 300]))


#Egypt (divided into periods)
#Pre-protest 
with(protests.PN0.egypt, mean(PropNegative[day <= 80]))
with(protests.PN0.egypt, mean(Negative[day <= 80]))
with(protests.PN0.egypt, mean(PropPositive[day <= 80]))
with(protests.PN0.egypt, mean(Positive[day <= 80]))

#Protest 1
with(protests.PN0.egypt, mean(PropNegative[day >= 81 & day<= 122]))
with(protests.PN0.egypt, mean(Negative[day >= 81 & day<= 122]))
with(protests.PN0.egypt, mean(PropPositive[day >= 81 & day<= 122]))
with(protests.PN0.egypt, mean(Positive[day >= 81 & day<= 122]))

#Intermediate 1 
with(protests.PN0.egypt, mean(PropNegative[day >=123  & day<= 154]))
with(protests.PN0.egypt, mean(Negative[day >= 123 & day<= 154]))
with(protests.PN0.egypt, mean(PropPositive[day >= 123 & day<= 154]))
with(protests.PN0.egypt, mean(Positive[day >= 123 & day<= 154]))

#Protest 2
with(protests.PN0.egypt, mean(PropNegative[day >=155  & day<= 157]))
with(protests.PN0.egypt, mean(Negative[day >=155  & day<= 157]))
with(protests.PN0.egypt, mean(PropPositive[day >=155  & day<= 157]))
with(protests.PN0.egypt, mean(Positive[day >=155  & day<= 157]))

#Intermediate 2
with(protests.PN0.egypt, mean(PropNegative[day >=158  & day<= 235]))
with(protests.PN0.egypt, mean(Negative[day >=158  & day<= 235]))
with(protests.PN0.egypt, mean(PropPositive[day >=158  & day<= 235]))
with(protests.PN0.egypt, mean(Positive[day >=158  & day<= 235]))

#Protests 3
with(protests.PN0.egypt, mean(PropNegative[day >=236  & day<= 237]))
with(protests.PN0.egypt, mean(Negative[day >=236  & day<= 237]))
with(protests.PN0.egypt, mean(PropPositive[day >=236  & day<= 237]))
with(protests.PN0.egypt, mean(Positive[day >=236  & day<= 237]))

#Intermediate 3
with(protests.PN0.egypt, mean(PropNegative[day >=238  & day<= 307]))
with(protests.PN0.egypt, mean(Negative[day >=238  & day<= 307]))
with(protests.PN0.egypt, mean(PropPositive[day >=238  & day<= 307]))
with(protests.PN0.egypt, mean(Positive[day >=238  & day<= 307]))

#Protests 4
with(protests.PN0.egypt, mean(PropNegative[day >=308  & day<= 310]))
with(protests.PN0.egypt, mean(Negative[day >=308  & day<= 310]))
with(protests.PN0.egypt, mean(PropPositive[day >=308  & day<= 310]))
with(protests.PN0.egypt, mean(Positive[day >=308  & day<= 310]))

#Intermediate 4
with(protests.PN0.egypt, mean(PropNegative[day >=311  & day<= 373]))
with(protests.PN0.egypt, mean(Negative[day >=311  & day<= 373]))
with(protests.PN0.egypt, mean(PropPositive[day >=311  & day<= 373]))
with(protests.PN0.egypt, mean(Positive[day >=311  & day<= 373]))

#Protest 5
with(protests.PN0.egypt, mean(PropNegative[day >=374  & day<= 416]))
with(protests.PN0.egypt, mean(Negative[day >=374  & day<= 416]))
with(protests.PN0.egypt, mean(PropPositive[day >=374  & day<= 416]))
with(protests.PN0.egypt, mean(Positive[day >=374  & day<= 416]))

describe(protests.PN0.egypt$PropNegative)
describe(protests.PN0.egypt$Negative)

#Running a T-test for PropNeg
PreProtestLibya <- filter(protests.PN0.libya, day <= 107)
ProtestLibya <- filter(protests.PN0.libya, day > 107 & day <= 179)
PostProtestLibya <- filter(protests.PN0.libya, day >= 180)

t.test(PreProtestLibya$PropNegative, protests.PN0.libya$PropNegative)
t.test(ProtestLibya$PropNegative, protests.PN0.libya$PropNegative)
t.test(PostProtestLibya$PropNegative, protests.PN0.libya$PropNegative)

PreProtestTunisia <- filter(protests.PN0.tunisia, day <= 44)
ProtestTunisia <- filter(protests.PN0.tunisia, day > 44 & day <= 145)
PostProtestTunisia <- filter(protests.PN0.tunisia, day >= 146)

t.test(PreProtestTunisia$PropNegative, protests.PN0.tunisia$PropNegative)
t.test(ProtestTunisia$PropNegative, protests.PN0.tunisia$PropNegative)
t.test(PostProtestTunisia$PropNegative, protests.PN0.tunisia$PropNegative)

PreProtestSyria <- filter(protests.PN0.syria, day <= 129)
ProtestSyria <- filter(protests.PN0.syria,day > 129 & day <= 299)
PostProtestSyria <- filter(protests.PN0.syria,day >= 300)

t.test(PreProtestSyria$PropNegative, protests.PN0.syria$PropNegative)
t.test(ProtestSyria$PropNegative, protests.PN0.syria$PropNegative)
t.test(PostProtestSyria$PropNegative, protests.PN0.syria$PropNegative)

PreProtestEgypt <- filter(protests.PN0.egypt,day <= 80)
Protest1Egypt <- filter(protests.PN0.egypt, day >= 81 & day<= 122)
Intermediate1Egypt <- filter(protests.PN0.egypt, day >=123  & day<= 154)
Protest2Egypt <- filter(protests.PN0.egypt, day >=155  & day<= 157)
Intermediate2Egypt <- filter(protests.PN0.egypt, day >=158  & day<= 235)
Protest3Egypt <- filter(protests.PN0.egypt, day >=236  & day<= 237)
Intermediate3Egypt <- filter(protests.PN0.egypt, day >=238  & day<= 307)
Protest4Egypt <- filter(protests.PN0.egypt, day >=308  & day<= 310)
Intermediate4Egypt <- filter(protests.PN0.egypt, day >=311  & day<= 373)
Protest5Egypt <- filter(protests.PN0.egypt, day >=374  & day<= 416)

t.test(PreProtestEgypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Protest1Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Intermediate1Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Protest2Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Intermediate2Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Protest3Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Intermediate3Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Protest4Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Intermediate4Egypt$PropNegative, protests.PN0.egypt$PropNegative)
t.test(Protest5Egypt$PropNegative, protests.PN0.egypt$PropNegative)

#Running a t-test for PropPos
t.test(PreProtestLibya$PropPositive, protests.PN0.libya$PropPositive)
t.test(ProtestLibya$PropPositive, protests.PN0.libya$PropPositive)
t.test(PostProtestLibya$PropPositive, protests.PN0.libya$PropPositive)

t.test(PreProtestTunisia$PropPositive, protests.PN0.tunisia$PropPositive)
t.test(ProtestTunisia$PropPositive, protests.PN0.tunisia$PropPositive)
t.test(PostProtestTunisia$PropPositive, protests.PN0.tunisia$PropPositive)

t.test(PreProtestSyria$PropPositive, protests.PN0.syria$PropPositive)
t.test(ProtestSyria$PropPositive, protests.PN0.syria$PropPositive)
t.test(PostProtestSyria$PropPositive, protests.PN0.syria$PropPositive)

t.test(PreProtestEgypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Protest1Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Intermediate1Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Protest2Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Intermediate2Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Protest3Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Intermediate3Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Protest4Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Intermediate4Egypt$PropPositive, protests.PN0.egypt$PropPositive)
t.test(Protest5Egypt$PropPositive, protests.PN0.egypt$PropPositive)

#Running t-tests comparing Pre/During and During/Post PropNeg and PropPos
#Libya
t.test(PreProtestLibya$PropNegative, ProtestLibya$PropNegative)
t.test(PostProtestLibya$PropNegative, ProtestLibya$PropNegative)
t.test(PreProtestLibya$PropPositive, ProtestLibya$PropPositive)
t.test(PostProtestLibya$PropPositive, ProtestLibya$PropPositive)

#Syria
t.test(PreProtestSyria$PropNegative, ProtestSyria$PropNegative)
t.test(PostProtestSyria$PropNegative, ProtestSyria$PropNegative)
t.test(PreProtestSyria$PropPositive, ProtestSyria$PropPositive)
t.test(PostProtestSyria$PropPositive, ProtestSyria$PropPositive)

#Tunisia
t.test(PreProtestTunisia$PropNegative, ProtestTunisia$PropNegative)
t.test(PostProtestTunisia$PropNegative, ProtestTunisia$PropNegative)
t.test(PreProtestTunisia$PropPositive, ProtestTunisia$PropPositive)
t.test(PostProtestTunisia$PropPositive, ProtestTunisia$PropPositive)

#Egypt
t.test(PreProtestEgypt$PropNegative, Protest1Egypt$PropNegative)
t.test(Intermediate1Egypt$PropNegative, Protest1Egypt$PropNegative)
t.test(PreProtestEgypt$PropPositive, Protest1Egypt$PropPositive)
t.test(Intermediate1Egypt$PropPositive, Protest1Egypt$PropPositive)

#Running t-tests comparing Pre/During and During/Post Tweet count
#Libya
t.test(PreProtestLibya$Tweets, ProtestLibya$Tweets)
t.test(PostProtestLibya$Tweets, ProtestLibya$Tweets)
#Syria
t.test(PreProtestSyria$Tweets, ProtestSyria$Tweets)
t.test(PostProtestSyria$Tweets, ProtestSyria$Tweets)
#Tunisia
t.test(PreProtestTunisia$Tweets, ProtestTunisia$Tweets)
t.test(PostProtestTunisia$Tweets, ProtestTunisia$Tweets)
#Egypt
t.test(PreProtestEgypt$Tweets, Protest1Egypt$Tweets)
t.test(Intermediate1Egypt$Tweets, Protest1Egypt$Tweets)

#Regress PropNeg and Protest Magnitude
lm(Protest ~ PropNegative, data = protests.PN0.libya) %>%
  summary()
lm(PropNegative ~ Protest, data = protests.PN0.libya) %>%
  summary()
lm(Protest ~ PropNegative + Tweets, data = protests.PN0.libya) %>%
  summary()
lm(PropNegative ~ Protest + Tweets, data = protests.PN0.libya) %>%
  summary()

lm(Protest ~ PropNegative, data = protests.PN0.syria) %>%
  summary()
lm(PropNegative ~ Protest, data = protests.PN0.syria) %>%
  summary()
lm(Protest ~ PropNegative + Tweets, data = protests.PN0.syria) %>%
  summary()
lm(PropNegative ~ Protest + Tweets, data = protests.PN0.syria) %>%
  summary()

lm(Protest ~ PropNegative, data = protests.PN0.tunisia) %>%
  summary()
lm(PropNegative ~ Protest, data = protests.PN0.tunisia) %>%
  summary()
lm(Protest ~ PropNegative + Tweets, data = protests.PN0.tunisia) %>%
  summary()
lm(PropNegative ~ Protest + Tweets, data = protests.PN0.tunisia) %>%
  summary()

lm(Protest ~ PropNegative, data = protests.PN0.egypt) %>%
  summary()
lm(PropNegative ~ Protest, data = protests.PN0.egypt) %>%
  summary()
lm(Protest ~ PropNegative + Tweets, data = protests.PN0.egypt) %>%
  summary()
lm(PropNegative ~ Protest + Tweets, data = protests.PN0.egypt) %>%
  summary()

#Scatter plot with fitted regression 
ggplot(protests.PN0.egypt, aes(x = PropNegative, y = Protest)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Egypt: Protests vs Negative Sentiment with Fitted Regression")
ggplot(protests.PN0.egypt, aes(x = Protest, y = PropNegative)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Egypt: Negative Sentiment vs Protests with Fitted Regression")

ggplot(protests.PN0.tunisia, aes(x = PropNegative, y = Protest)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Tunisia: Protests vs Negative Sentiment with Fitted Regression")
ggplot(protests.PN0.tunisia, aes(x = Protest, y = PropNegative)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Tunisia: Negative Sentiment vs Protests with Fitted Regression")

ggplot(protests.PN0.libya, aes(x = PropNegative, y = Protest)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Libya: Protests vs Negative Sentiment with Fitted Regression")
ggplot(protests.PN0.libya, aes(x = Protest, y = PropNegative)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Libya: Negative Sentiment vs Protests with Fitted Regression")

ggplot(protests.PN0.syria, aes(x = PropNegative, y = Protest)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Syria: Protests vs Negative Sentiment with Fitted Regression")
ggplot(protests.PN0.syria, aes(x = Protest, y = PropNegative)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Syria: Negative Sentiment vs Protests with Fitted Regression")

#Making month categorical (would have to fix the overlap of November10/11 and December 10/11)
library(lubridate)
protests.PN0.egypt$date <- as.POSIXct(protests.PN0.egypt$date)
protests.PN0.egypt$month <- month(as.POSIXct(protests.PN0.egypt$date, format = "%Y-%m-%d"))

ggplot(protests.PN0.egypt, aes(x = as.factor(month), y = PropNegative)) +
  geom_boxplot(aes(fill = as.factor(month)))

#Box plots
protests.PN0.libya$Period <- NA
for (i in 1:411){
if (protests.PN0.libya$day[i] <= 107){
  protests.PN0.libya$Period[i] = "PreProtest"
}else if (protests.PN0.libya$day[i]<= 179){
  protests.PN0.libya$Period[i] = "Protest"
}else {
  protests.PN0.libya$Period[i] = "PostProtest"}}

ggplot(protests.PN0.libya, aes(x = factor(Period, levels = c("PreProtest", "Protest", "PostProtest")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("lightblue", "yellow", "yellow")) +
  labs(title = "Libya: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

protests.PN0.tunisia$Period <- NA
for (i in 1:415){
  if (protests.PN0.tunisia$day[i] <= 44){
    protests.PN0.tunisia$Period[i] = "PreProtest"
  }else if (protests.PN0.tunisia$day[i]<= 145){
    protests.PN0.tunisia$Period[i] = "Protest"
  }else {
    protests.PN0.tunisia$Period[i] = "PostProtest"}}

ggplot(protests.PN0.tunisia, aes(x = factor(Period, levels = c("PreProtest", "Protest", "PostProtest")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("lightblue", "yellow", "yellow")) +
  labs(title = "Tunisia: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

protests.PN0.syria$Period <- NA
for (i in 1:416){
  if (protests.PN0.syria$day[i] <= 129){
    protests.PN0.syria$Period[i] = "PreProtest"
  }else if (protests.PN0.syria$day[i]<= 229){
    protests.PN0.syria$Period[i] = "Protest"
  }else {
    protests.PN0.syria$Period[i] = "PostProtest"}}

ggplot(protests.PN0.syria, aes(x = factor(Period, levels = c("PreProtest", "Protest", "PostProtest")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("lightblue", "lightblue", "lightblue")) +
  labs(title = "Syria: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

protests.PN0.egypt$Period <- NA
for (i in 1:416){
  if (protests.PN0.egypt$day[i] <= 80){
    protests.PN0.egypt$Period[i] = "PreProtest"
  }else if (protests.PN0.egypt$day[i]<= 122){
    protests.PN0.egypt$Period[i] = "Protest #1"
  }else if (protests.PN0.egypt$day[i]<= 154){
    protests.PN0.egypt$Period[i] = "Intermediate #1"
  }else if (protests.PN0.egypt$day[i]<= 157){
    protests.PN0.egypt$Period[i] = "Protest #2"
  }else if (protests.PN0.egypt$day[i]<= 235){
    protests.PN0.egypt$Period[i] = "Intermediate #2"
  }else if (protests.PN0.egypt$day[i]<= 237){
    protests.PN0.egypt$Period[i] = "Protest #3"
  }else if (protests.PN0.egypt$day[i]<= 307){
    protests.PN0.egypt$Period[i] = "Intermediate #3"
  }else if (protests.PN0.egypt$day[i]<= 310){
    protests.PN0.egypt$Period[i] = "Protest #4"
  }else if (protests.PN0.egypt$day[i]<= 373){
    protests.PN0.egypt$Period[i] = "Intermediate #4"
  }else {
    protests.PN0.egypt$Period[i] = "Protest #5"}}

ggplot(protests.PN0.egypt, aes(x = factor(Period, levels = c("PreProtest", "Protest #1", "Intermediate #1", "Protest #2", "Intermediate #2", "Protest #3", "Intermediate #3", "Protest #4", "Intermediate #4", "Protest #5")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("lightblue", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue", "lightblue")) +
  labs(title = "Egypt: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")


