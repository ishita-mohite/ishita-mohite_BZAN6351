# ishita-mohite_BZAN6351
---
title: "Banking Data"
output:
  word_document: default
  html_document: default
date: "2025-10-31"
---

```{r}
library(tidyverse)
library(ggplot2)
library(caret)
```

```{r}
setwd("C:/Users/ishita/OneDrive/Documents")
```

```{r}
df <- read.csv("bank.csv.csv", stringsAsFactors = TRUE)
df
```

```{r}
summary(df)
df %>% group_by(y) %>% summarize(mean_balance = mean(balance), count = n())
```
```{r}
df$contact_freq <- cut(df$campaign, breaks = c(0,1,4,10,Inf), labels = c("once","few","moderate","intense"))
ggplot(df, aes(x=job, fill=y)) + geom_bar(position="fill") + theme_minimal()
```
```{r}
fit <- glm(y ~ balance + duration + age + contact_freq, data=df, family="binomial")
summary(fit)
```
```{r}
actual <- factor(df$y, levels = c("no", "yes"))
predicted_class <- factor(ifelse(predict(fit, type = "response") > 0.5, "yes", "no"), levels = c("no", "yes"))
confusionMatrix(predicted_class, actual)
```
