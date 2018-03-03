#Script for Data Outputs and visuals
#Box plots
ggplot(protests.PN0.libya, aes(x = factor(Period, levels = c("PreProtest", "Protest", "PostProtest")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("yellow", "yellow", "yellow")) +
  labs(title = "Libya: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

ggplot(protests.PN0.tunisia, aes(x = factor(Period, levels = c("PreProtest", "Protest", "PostProtest")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("yellow", "yellow", "yellow")) +
  labs(title = "Tunisia: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

ggplot(protests.PN0.syria, aes(x = factor(Period, levels = c("PreProtest", "Protest", "PostProtest")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("lightblue", "yellow", "yellow")) +
  labs(title = "Syria: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

ggplot(protests.PN0.egypt, aes(x = factor(Period, levels = c("PreProtest", "Protest #1", "Intermediate #1", "Protest #2", "Intermediate #2", "Protest #3", "Intermediate #3", "Protest #4", "Intermediate #4", "Protest #5")), y = PropNegative)) +
  geom_boxplot(aes(fill = Period)) +
  scale_fill_manual(values = c("lightblue", "yellow", "yellow", "yellow", "yellow", "yellow", "yellow", "lightblue", "yellow", "yellow")) +
  labs(title = "Egypt: Mean Proportion of Negative Sentiment by Period", x = "Period", y = "Proportion of Negative Sentiment")

#Protest Line Graphs
ggplot(protests.PN0.libya, aes(x = day, y = Protest)) +
  geom_line() +
  geom_vline(xintercept = c(108, 179), linetype = "dashed", color = "red") +
  labs(title = "Libya: Protest Magnitude by Day", x = "Day", y = "Protest Magnitude")

ggplot(protests.PN0.tunisia, aes(x = day, y = Protest)) +
  geom_line() +
  geom_vline(xintercept = c(45, 145), linetype = "dashed", color = "red") +
  labs(title = "Tunisia: Protest Magnitude by Day", x = "Day", y = "Protest Magnitude")

ggplot(protests.PN0.syria, aes(x = day, y = Protest)) +
  geom_line() +
  geom_vline(xintercept = c(130, 299), linetype = "dashed", color = "red") +
  labs(title = "Syria: Protest Magnitude by Day", x = "Day", y = "Protest Magnitude")

ggplot(protests.PN0.egypt, aes(x = day, y = Protest)) +
  geom_line() +
  geom_vline(xintercept = c(81, 155, 236, 308, 374), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(123, 158, 238, 311), linetype = "dashed", color = "blue") +
  labs(title = "Egypt: Protest Magnitude by Day", x = "Day", y = "Protest Magnitude")

#Volatility 
ggplot(volitility.PN0.libya, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Libya: Volatility of the Proportion of Positive and Negatvive Sentiment with a 30 Day Window", y = "Volatility", x = "Day") +
  geom_vline(xintercept = c(108, 179), linetype = "dashed", color = "red") +
  guides(colour = guide_legend("Sentiment")) 

ggplot(volitility.PN0.tunisia, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Tunisia: Volatility of the Proportion of Positive and Negatvive Sentiment with a 30 Day Window", y = "Volatility", x = "Day") +
  geom_vline(xintercept = c(45, 145), linetype = "dashed", color = "red") +
  guides(colour = guide_legend("Sentiment"))

ggplot(volitility.PN0.syria, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Syria: Volatility of the Proportion of Positive and Negatvive Sentiment with a 30 Day Window", y = "Volatility", x = "Day") +
  geom_vline(xintercept = c(130, 299), linetype = "dashed", color = "red") +
  guides(colour = guide_legend("Sentiment"))

ggplot(volitility.PN0.egypt, aes(x = day) ) +
  geom_line(aes(y = rollSDPropNeg, color = "Negative")) +
  geom_line(aes(y = rollSDPropPos, color = "Positive")) +
  labs(title = "Egypt: Volatility of the Proportion of Positive and Negatvive Sentiment with a 30 Day Window", y = "Volatility", x = "Day") +
  geom_vline(xintercept = c(81, 155, 236, 308, 374), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(123, 158, 238, 311), linetype = "dashed", color = "blue") +
  guides(colour = guide_legend("Sentiment"))

#Fitted Reggression and Scatter plot for Egypt
ggplot(protests.PN0.egypt, aes(x = PropNegative, y = Protest)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method = "loess") +
  guides(alpha = F) +
  labs(title = "Egypt: Protests vs Negative Sentiment with Fitted Regression", x = "Negative Sentiment", y = "Protest Magnitude")

#Regression outputs 
output <- lm(Protest ~ PropNegative, data = protests.PN0.libya)
output2 <- lm(Protest ~ PropNegative + Tweets, data = protests.PN0.libya)
output3 <- lm(Protest ~ Tweets + PropPositive, data = protests.PN0.libya)
output4 <- lm(Protest ~ PropNegative + PropPositive + Tweets, data = protests.PN0.libya)
stargazer(output, output2, output3, output4, type="html", out="test.html", out.header=TRUE, title = "Libya Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude")

outputS <- lm(Protest ~ PropNegative, data = protests.PN0.syria)
outputS2 <- lm(Protest ~ PropNegative + Tweets, data = protests.PN0.syria)
outputS3 <- lm(Protest ~ Tweets + PropPositive, data = protests.PN0.syria)
outputS4 <- lm(Protest ~ PropNegative + PropPositive + Tweets, data = protests.PN0.syria)
stargazer(outputS, outputS2, outputS3, outputS4, type="html", out="Syria.html", out.header=TRUE, title = "Syria Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude")

outputT <- lm(Protest ~ PropNegative, data = protests.PN0.tunisia)
outputT2 <- lm(Protest ~ PropNegative + Tweets, data = protests.PN0.tunisia)
outputT3 <- lm(Protest ~ Tweets + PropPositive, data = protests.PN0.tunisia)
outputT4 <- lm(Protest ~ PropNegative + PropPositive + Tweets, data = protests.PN0.tunisia)
stargazer(outputT, outputT2, outputT3, outputT4, type="html", out="Tunisia.html", out.header=TRUE, title = "Tunisia Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude")

outputE <- lm(Protest ~ PropNegative, data = protests.PN0.egypt)
outputE2 <- lm(Protest ~ PropNegative + Tweets, data = protests.PN0.egypt)
outputE3 <- lm(Protest ~ Tweets + PropPositive, data = protests.PN0.egypt)
outputE4 <- lm(Protest ~ PropNegative + PropPositive + Tweets, data = protests.PN0.egypt)
stargazer(outputE, outputE2, outputE3, outputE4, type="html", out="Egypt.html", out.header=TRUE, title = "Egypt Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude")

#Lagged Regression Output 
lagoutput <- lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.syria))
lagoutput2 <- lm(Lag2 ~ PropNegative + Tweets, data = na.exclude(lag.protests.PN0.syria))
lagoutput3 <- lm(Lag2 ~ Tweets + PropPositive, data = na.exclude(lag.protests.PN0.syria))
lagoutput4 <- lm(Lag2 ~ PropNegative + PropPositive + Tweets, data = na.exclude(lag.protests.PN0.syria))
stargazer(lagoutput, lagoutput2, lagoutput3, lagoutput4, type="html", out="lagsyria.html", out.header=TRUE, title = "Syria Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude with a Lag of 2")

lagoutputL <- lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.libya))
lagoutputL2 <- lm(Lag2 ~ PropNegative + Tweets, data = na.exclude(lag.protests.PN0.libya))
lagoutputL3 <- lm(Lag2 ~ Tweets + PropPositive, data = na.exclude(lag.protests.PN0.libya))
lagoutputL4 <- lm(Lag2 ~ PropNegative + PropPositive + Tweets, data = na.exclude(lag.protests.PN0.libya))
stargazer(lagoutputL, lagoutputL2, lagoutputL3, lagoutputL4, type="html", out="laglibya.html", out.header=TRUE, title = "Libya Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude with a Lag of 2")

lagoutputT <- lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.tunisia))
lagoutputT2 <- lm(Lag2 ~ PropNegative + Tweets, data = na.exclude(lag.protests.PN0.tunisia))
lagoutputT3 <- lm(Lag2 ~ Tweets + PropPositive, data = na.exclude(lag.protests.PN0.tunisia))
lagoutputT4 <- lm(Lag2 ~ PropNegative + PropPositive + Tweets, data = na.exclude(lag.protests.PN0.tunisia))
stargazer(lagoutputT, lagoutputT2, lagoutputT3, lagoutputT4, type="html", out="lagtunisia.html", out.header=TRUE, title = "Tunisia Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude with a Lag of 2")

lagoutputE <- lm(Lag2 ~ PropNegative, data = na.exclude(lag.protests.PN0.egypt))
lagoutputE2 <- lm(Lag2 ~ PropNegative + Tweets, data = na.exclude(lag.protests.PN0.egypt))
lagoutputE3 <- lm(Lag2 ~ Tweets + PropPositive, data = na.exclude(lag.protests.PN0.egypt))
lagoutputE4 <- lm(Lag2 ~ PropNegative + PropPositive + Tweets, data = na.exclude(lag.protests.PN0.egypt))
stargazer(lagoutputE, lagoutputE2, lagoutputE3, lagoutputE4, type="html", out="lagegypt.html", out.header=TRUE, title = "Egypt Linear Regression Models", covariate.labels = c("Negative Sentiment", "Number of Tweets", "Positive Sentiment"), dep.var.labels = "Protest Magnitude with a Lag of 2")
