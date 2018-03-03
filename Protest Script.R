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

#load protests datasets
protests.libya <- read_csv("ICEWS_All_Protests_Libya_Aggregated.csv")
protests.egypt <- read_csv("ICEWS_All_Protests_Egypt_Aggregated.csv")
protests.tunisia <- read_csv("ICEWS_All_Protests_Tunisia_Aggregated.csv")
protests.syria <- read_csv("ICEWS_All_Protests_Syria_Aggregated.csv")

#merge all protests
protests.all <- bind_rows(protests.egypt, protests.libya, protests.syria, protests.tunisia)

#plot all protests
ggplot(protests.all, aes(x = Date, y = Protest)) +
  + geom_bar(stat = "identity", aes(fill = country)) +
  + facet_wrap(~country) +
  ggtitle("Number of Protests per day")

protests.libya$Date <- mdy(protests.libya$Date)
names(libya.by.date)[1] <- c("Date")