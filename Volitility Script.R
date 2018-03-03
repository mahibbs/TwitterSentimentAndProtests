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
require(data.table)

#Compute and graph rolling SD of PropNegative and Negative 
#Libya
volitility.PN0.libya <- subset(protests.PN0.libya, select = c(date, PropPositive, PropNegative, PropNeutral, Positive, Negative, Neutral, day, Protest, Tweets))

volitility.PN0.libya$rollSDPropNeg <- rollapply(volitility.PN0.libya$PropNegative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDPropNeg)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of PropNegative with 30 day window")

volitility.PN0.libya$rollSDNeg <- rollapply(volitility.PN0.libya$Negative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDNeg)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of Negative with 30 day window")

volitility.PN0.libya$rollSDPropNeg7 <- rollapply(volitility.PN0.libya$PropNegative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDPropNeg7)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of PropNegative with 7 day window")

volitility.PN0.libya$rollSDNeg7 <- rollapply(volitility.PN0.libya$Negative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDNeg7)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of Negative with 7 day window")

#Plus protests (come back)
volitility.PN0.libya$rollSDPropNeg <- rollapply(volitility.PN0.libya$PropNegative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDPropNeg)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of PropNegative with 30 day window")

#Syria
volitility.PN0.syria <- subset(protests.PN0.syria, select = c(date, PropPositive, PropNegative, PropNeutral, Positive, Negative, Neutral, day, Protest, Tweets))

volitility.PN0.syria$rollSDPropNeg <- rollapply(volitility.PN0.syria$PropNegative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day, y = rollSDPropNeg)) +
  geom_line() +
  labs(title = "Syria: Rolling SD of PropNegative with 30 day window")

volitility.PN0.syria$rollSDNeg <- rollapply(volitility.PN0.syria$Negative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day, y = rollSDNeg)) +
  geom_line() +
  labs(title = "Syria: Rolling SD of Negative with 30 day window")

volitility.PN0.syria$rollSDPropNeg7 <- rollapply(volitility.PN0.syria$PropNegative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day, y = rollSDPropNeg7)) +
  geom_line() +
  labs(title = "Syria: Rolling SD of PropNegative with 7 day window")

volitility.PN0.syria$rollSDNeg7 <- rollapply(volitility.PN0.syria$Negative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day, y = rollSDNeg7)) +
  geom_line() +
  labs(title = "Syria: Rolling SD of Negative with 7 day window")

#Tunisia
volitility.PN0.tunisia <- subset(protests.PN0.tunisia, select = c(date, PropPositive, PropNegative, PropNeutral, Positive, Negative, Neutral, day, Protest, Tweets))

volitility.PN0.tunisia$rollSDPropNeg <- rollapply(volitility.PN0.tunisia$PropNegative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day, y = rollSDPropNeg)) +
  geom_line() +
  labs(title = "Tunisia: Rolling SD of PropNegative with 30 day window")

volitility.PN0.tunisia$rollSDNeg <- rollapply(volitility.PN0.tunisia$Negative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day, y = rollSDNeg)) +
  geom_line() +
  labs(title = "Tunisia: Rolling SD of Negative with 30 day window")

volitility.PN0.tunisia$rollSDPropNeg7 <- rollapply(volitility.PN0.tunisia$PropNegative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day, y = rollSDPropNeg7)) +
  geom_line() +
  labs(title = "Tunisia: Rolling SD of PropNegative with 7 day window")

volitility.PN0.tunisia$rollSDNeg7 <- rollapply(volitility.PN0.tunisia$Negative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day, y = rollSDNeg7)) +
  geom_line() +
  labs(title = "Tunisia: Rolling SD of Negative with 7 day window")

#Egypt 
volitility.PN0.egypt <- subset(protests.PN0.egypt, select = c(date, PropPositive, PropNegative, PropNeutral, Positive, Negative, Neutral, day, Protest, Tweets))

volitility.PN0.egypt$rollSDPropNeg <- rollapply(volitility.PN0.egypt$PropNegative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day, y = rollSDPropNeg)) +
  geom_line() +
  labs(title = "Egypt: Rolling SD of PropNegative with 30 day window")

volitility.PN0.egypt$rollSDNeg <- rollapply(volitility.PN0.egypt$Negative, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day, y = rollSDNeg)) +
  geom_line() +
  labs(title = "Egypt: Rolling SD of Negative with 30 day window")

volitility.PN0.egypt$rollSDPropNeg7 <- rollapply(volitility.PN0.egypt$PropNegative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day, y = rollSDPropNeg7)) +
  geom_line() +
  labs(title = "Egypt: Rolling SD of PropNegative with 7 day window")

volitility.PN0.egypt$rollSDNeg7 <- rollapply(volitility.PN0.egypt$Negative, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day, y = rollSDNeg7)) +
  geom_line() +
  labs(title = "Egypt: Rolling SD of Negative with 7 day window")

#All together now
#Libya
volitility.PN0.libya$rollSDPropPos <- rollapply(volitility.PN0.libya$PropPositive, width = 30, align = 'left', FUN = sd, fill = NA)
volitility.PN0.libya$rollSDPropNeu <- rollapply(volitility.PN0.libya$PropNeutral, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Libya: Rolling SD of PN0 with 30 day window", y = "Volatility", x = "Day") +
  geom_vline(xintercept = c(108, 179), linetype = "dashed", color = "red") +
  guides(colour = guide_legend("Sentiment")) 
  
  #ylim(0,0.1)

#Syria
volitility.PN0.syria$rollSDPropPos <- rollapply(volitility.PN0.syria$PropPositive, width = 30, align = 'left', FUN = sd, fill = NA)
volitility.PN0.syria$rollSDPropNeu <- rollapply(volitility.PN0.syria$PropNeutral, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Syria: Rolling SD of PN0 with 30 day window", y = "Volitility")

#Tunisia
volitility.PN0.tunisia$rollSDPropPos <- rollapply(volitility.PN0.tunisia$PropPositive, width = 30, align = 'left', FUN = sd, fill = NA)
volitility.PN0.tunisia$rollSDPropNeu <- rollapply(volitility.PN0.tunisia$PropNeutral, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Tunisia: Rolling SD of PN0 with 30 day window", y = "Volitility")

#Egypt
volitility.PN0.egypt$rollSDPropPos <- rollapply(volitility.PN0.egypt$PropPositive, width = 30, align = 'left', FUN = sd, fill = NA)
volitility.PN0.egypt$rollSDPropNeu <- rollapply(volitility.PN0.egypt$PropNeutral, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Egypt: Rolling SD of PN0 with 30 day window", y = "Volitility")


#Compute and graph rolling SD of Tweets 
#Libya
volitility.PN0.libya$rollSDTweets30 <- rollapply(volitility.PN0.libya$Tweets, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDTweets30)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of Tweets with 30 day window")

volitility.PN0.libya$rollSDTweets7 <- rollapply(volitility.PN0.libya$Tweets, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.libya, aes(x = day, y = rollSDTweets7)) +
  geom_line() +
  labs(title = "Libya: Rolling SD of Tweets with 7 day window")

#Syria
volitility.PN0.syria$rollSDTweets30 <- rollapply(volitility.PN0.syria$Tweets, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day, y = rollSDTweets30)) +
  geom_line() +
  labs(title = "Syria: Rolling SD of Tweets with 30 day window")

volitility.PN0.syria$rollSDTweets7 <- rollapply(volitility.PN0.syria$Tweets, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.syria, aes(x = day, y = rollSDTweets7)) +
  geom_line() +
  labs(title = "Syria: Rolling SD of Tweets with 7 day window")

#Tunisia
volitility.PN0.tunisia$rollSDTweets30 <- rollapply(volitility.PN0.tunisia$Tweets, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day, y = rollSDTweets30)) +
  geom_line() +
  labs(title = "Tunisia: Rolling SD of Tweets with 30 day window")

volitility.PN0.tunisia$rollSDTweets7 <- rollapply(volitility.PN0.tunisia$Tweets, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.tunisia, aes(x = day, y = rollSDTweets7)) +
  geom_line() +
  labs(title = "Tunisia: Rolling SD of Tweets with 7 day window")

#Egypt (run)
volitility.PN0.egypt$rollSDTweets30 <- rollapply(volitility.PN0.egypt$Tweets, width = 30, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day, y = rollSDTweets30)) +
  geom_line() +
  labs(title = "Egypt: Rolling SD of Tweets with 30 day window")

volitility.PN0.egypt$rollSDTweets7 <- rollapply(volitility.PN0.egypt$Tweets, width = 7, align = 'left', FUN = sd, fill = NA)
ggplot(volitility.PN0.egypt, aes(x = day, y = rollSDTweets7)) +
  geom_line() +
  labs(title = "Egypt: Rolling SD of Tweets with 7 day window")
