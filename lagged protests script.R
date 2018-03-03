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

#Lagging Protests

#Libya
lag.protests.PN0.libya <- subset(protests.PN0.libya, select = c(date, PropPositive, PropNegative, PropNeutral, day, Protest, Tweets))

lag.protests.PN0.libya$Lag1 = lag(zoo(lag.protests.PN0.libya$Protest), 1, na.pad = TRUE)
lag.protests.PN0.libya$Lag2 = lag(zoo(lag.protests.PN0.libya$Protest), 2, na.pad = TRUE)
lag.protests.PN0.libya$Lag3 = lag(zoo(lag.protests.PN0.libya$Protest), 3, na.pad = TRUE)
lag.protests.PN0.libya$Lag4 = lag(zoo(lag.protests.PN0.libya$Protest), 4, na.pad = TRUE)
lag.protests.PN0.libya$Lag5 = lag(zoo(lag.protests.PN0.libya$Protest), 5, na.pad = TRUE)

lag.protests.PN0.libya$Lead1 <- lead(lag.protests.PN0.libya$Protest, n = 1)
lag.protests.PN0.libya$Lead2 <- lead(lag.protests.PN0.libya$Protest, n = 2)
lag.protests.PN0.libya$Lead3 <- lead(lag.protests.PN0.libya$Protest, n = 3)
lag.protests.PN0.libya$Lead4 <- lead(lag.protests.PN0.libya$Protest, n = 4)
lag.protests.PN0.libya$Lead5 <- lead(lag.protests.PN0.libya$Protest, n = 5)

lm(Lag1 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag3 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag4 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag5 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead1 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead2 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead3 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead4 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead5 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()

lm(Protest ~ log(PropNegative), data = lag.protests.PN0.libya) %>%
  summary()
lm(Protest ~ log(PropNegative) + log(Tweets), data = lag.protests.PN0.libya) %>%
  summary()
LibyaPropNeg2 <- lag.protests.PN0.libya$PropNegative^2
lm(Protest ~ PropNegative + LibyaPropNeg2, data = lag.protests.PN0.libya) %>%
  summary()

ggplot(lag.protests.PN0.libya, aes(x = PropNegative, y = Lag5)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Libya: LAGGED Protests vs Negative Sentiment with Fitted Regression")

#Syria
lag.protests.PN0.syria <- subset(protests.PN0.syria, select = c(date, PropPositive, PropNegative, PropNeutral, day, Protest, Tweets))

lag.protests.PN0.syria$Lag1 = lag(zoo(lag.protests.PN0.syria$Protest), 1, na.pad = TRUE)
lag.protests.PN0.syria$Lag2 = lag(zoo(lag.protests.PN0.syria$Protest), 2, na.pad = TRUE)
lag.protests.PN0.syria$Lag3 = lag(zoo(lag.protests.PN0.syria$Protest), 3, na.pad = TRUE)
lag.protests.PN0.syria$Lag4 = lag(zoo(lag.protests.PN0.syria$Protest), 4, na.pad = TRUE)
lag.protests.PN0.syria$Lag5 = lag(zoo(lag.protests.PN0.syria$Protest), 5, na.pad = TRUE)

lag.protests.PN0.syria$Lead1 <- lead(lag.protests.PN0.syria$Protest, n = 1)
lag.protests.PN0.syria$Lead2 <- lead(lag.protests.PN0.syria$Protest, n = 2)
lag.protests.PN0.syria$Lead3 <- lead(lag.protests.PN0.syria$Protest, n = 3)
lag.protests.PN0.syria$Lead4 <- lead(lag.protests.PN0.syria$Protest, n = 4)
lag.protests.PN0.syria$Lead5 <- lead(lag.protests.PN0.syria$Protest, n = 5)

lm(Lag1 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag3 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag4 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag5 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead1 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead2 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead3 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead4 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead5 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()

ggplot(lag.protests.PN0.syria, aes(x = PropNegative, y = Protest)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Syria: Protests vs Negative Sentiment with Fitted Regression")

#Tunisia
lag.protests.PN0.tunisia <- subset(protests.PN0.tunisia, select = c(date, PropPositive, PropNegative, PropNeutral, day, Protest, Tweets))

lag.protests.PN0.tunisia$Lag1 = lag(zoo(lag.protests.PN0.tunisia$Protest), 1, na.pad = TRUE)
lag.protests.PN0.tunisia$Lag2 = lag(zoo(lag.protests.PN0.tunisia$Protest), 2, na.pad = TRUE)
lag.protests.PN0.tunisia$Lag3 = lag(zoo(lag.protests.PN0.tunisia$Protest), 3, na.pad = TRUE)
lag.protests.PN0.tunisia$Lag4 = lag(zoo(lag.protests.PN0.tunisia$Protest), 4, na.pad = TRUE)
lag.protests.PN0.tunisia$Lag5 = lag(zoo(lag.protests.PN0.tunisia$Protest), 5, na.pad = TRUE)

lag.protests.PN0.tunisia$Lead1 <- lead(lag.protests.PN0.tunisia$Protest, n = 1)
lag.protests.PN0.tunisia$Lead2 <- lead(lag.protests.PN0.tunisia$Protest, n = 2)
lag.protests.PN0.tunisia$Lead3 <- lead(lag.protests.PN0.tunisia$Protest, n = 3)
lag.protests.PN0.tunisia$Lead4 <- lead(lag.protests.PN0.tunisia$Protest, n = 4)
lag.protests.PN0.tunisia$Lead5 <- lead(lag.protests.PN0.tunisia$Protest, n = 5)

lm(Lag1 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag3 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag4 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag5 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead1 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead2 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead3 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead4 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead5 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()

#Egypt
lag.protests.PN0.egypt <- subset(protests.PN0.egypt, select = c(date, PropPositive, PropNegative, PropNeutral, day, Protest, Tweets))

lag.protests.PN0.egypt$Lag1 = lag(zoo(lag.protests.PN0.egypt$Protest), 1, na.pad = TRUE)
lag.protests.PN0.egypt$Lag2 = lag(zoo(lag.protests.PN0.egypt$Protest), 2, na.pad = TRUE)
lag.protests.PN0.egypt$Lag3 = lag(zoo(lag.protests.PN0.egypt$Protest), 3, na.pad = TRUE)
lag.protests.PN0.egypt$Lag4 = lag(zoo(lag.protests.PN0.egypt$Protest), 4, na.pad = TRUE)
lag.protests.PN0.egypt$Lag5 = lag(zoo(lag.protests.PN0.egypt$Protest), 5, na.pad = TRUE)

lag.protests.PN0.egypt$Lead1 <- lead(lag.protests.PN0.egypt$Protest, n = 1)
lag.protests.PN0.egypt$Lead2 <- lead(lag.protests.PN0.egypt$Protest, n = 2)
lag.protests.PN0.egypt$Lead3 <- lead(lag.protests.PN0.egypt$Protest, n = 3)
lag.protests.PN0.egypt$Lead4 <- lead(lag.protests.PN0.egypt$Protest, n = 4)
lag.protests.PN0.egypt$Lead5 <- lead(lag.protests.PN0.egypt$Protest, n = 5)

lm(Lag1 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag3 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag4 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag5 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead1 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead2 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead3 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead4 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead5 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()

#Regressions on Number of Tweets and lagged protests 
#Libya (All insigificant)
lm(Protest ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag1 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag2 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag3 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag4 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lag5 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead1 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead2 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead3 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead4 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()
lm(Lead5 ~ Tweets, data = na.exclude(lag.protests.PN0.libya)) %>%
  summary()

#Syria (All insignificant)
lm(Protest ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag1 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag2 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag3 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag4 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lag5 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead1 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead2 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead3 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead4 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()
lm(Lead5 ~ Tweets, data = na.exclude(lag.protests.PN0.syria)) %>%
  summary()

#Tunisia
lm(Protest ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag1 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag2 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag3 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag4 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lag5 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead1 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead2 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead3 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead4 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()
lm(Lead5 ~ Tweets, data = na.exclude(lag.protests.PN0.tunisia)) %>%
  summary()

#Egypt
lm(Protest ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag1 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag2 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag3 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag4 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lag5 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead1 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead2 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead3 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead4 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()
lm(Lead5 ~ Tweets, data = na.exclude(lag.protests.PN0.egypt)) %>%
  summary()

#T-tests to compare mean number of tweets
#Libya
PreProtestLibya <- filter(lag.protests.PN0.libya, day <= 107)
ProtestLibya <- filter(lag.protests.PN0.libya, day > 107 & day <= 179)
PostProtestLibya <- filter(lag.protests.PN0.libya, day >= 180)

t.test(PreProtestLibya$Tweets, lag.protests.PN0.libya$Tweets)
t.test(ProtestLibya$Tweets, lag.protests.PN0.libya$Tweets)
t.test(PostProtestLibya$Tweets, lag.protests.PN0.libya$Tweets)

#Tunisia
PreProtestTunisia <- filter(lag.protests.PN0.tunisia, day <= 44)
ProtestTunisia <- filter(lag.protests.PN0.tunisia, day > 44 & day <= 145)
PostProtestTunisia <- filter(lag.protests.PN0.tunisia, day >= 146)

t.test(PreProtestTunisia$Tweets, lag.protests.PN0.tunisia$Tweets)
t.test(ProtestTunisia$Tweets, lag.protests.PN0.tunisia$Tweets)
t.test(PostProtestTunisia$Tweets, lag.protests.PN0.tunisia$Tweets)

#Syria
PreProtestSyria <- filter(lag.protests.PN0.syria, day <= 129)
ProtestSyria <- filter(lag.protests.PN0.syria,day > 129 & day <= 299)
PostProtestSyria <- filter(lag.protests.PN0.syria,day >= 300)

t.test(PreProtestSyria$Tweets, lag.protests.PN0.syria$Tweets)
t.test(ProtestSyria$Tweets, lag.protests.PN0.syria$Tweets)
t.test(PostProtestSyria$Tweets, lag.protests.PN0.syria$Tweets)

#Egypt 
PreProtestEgypt <- filter(lag.protests.PN0.egypt,day <= 80)
Protest1Egypt <- filter(lag.protests.PN0.egypt, day >= 81 & day<= 122)
Intermediate1Egypt <- filter(lag.protests.PN0.egypt, day >=123  & day<= 154)
Protest2Egypt <- filter(lag.protests.PN0.egypt, day >=155  & day<= 157)
Intermediate2Egypt <- filter(lag.protests.PN0.egypt, day >=158  & day<= 235)
Protest3Egypt <- filter(lag.protests.PN0.egypt, day >=236  & day<= 237)
Intermediate3Egypt <- filter(lag.protests.PN0.egypt, day >=238  & day<= 307)
Protest4Egypt <- filter(lag.protests.PN0.egypt, day >=308  & day<= 310)
Intermediate4Egypt <- filter(lag.protests.PN0.egypt, day >=311  & day<= 373)
Protest5Egypt <- filter(lag.protests.PN0.egypt, day >=374  & day<= 416)

t.test(PreProtestEgypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Protest1Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Intermediate1Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Protest2Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Intermediate2Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Protest3Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Intermediate3Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Protest4Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Intermediate4Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
t.test(Protest5Egypt$Tweets, lag.protests.PN0.egypt$Tweets)
