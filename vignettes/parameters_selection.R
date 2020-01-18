## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

set.seed(333)

## ---- warning=FALSE, message=FALSE, fig.align='center', echo=FALSE------------
include_graphics("https://raw.githubusercontent.com/easystats/parameters/master/man/figures/figure2.png")

## ----message=FALSE, warning=FALSE---------------------------------------------
model <- lm(Sepal.Length ~ .*., data=iris)
summary(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(performance)

check_normality(model)
check_heteroscedasticity(model)
check_autocorrelation(model)
check_collinearity(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)
library(parameters)

lm(Sepal.Length ~ .*., data=iris) %>% 
  select_parameters() %>% 
  summary()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(lme4)

lmer(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + (1|Species), data=iris) %>%
  select_parameters() %>%
  summary()

