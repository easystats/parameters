## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

set.seed(333)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(parameters)
library(dplyr)
library(psych)

# Load the data
data <- psych::bfi[, 1:25]  # Select only the 25 first columns corresponding to the items
data <- na.omit(data)  # remove missing values

# Check factor structure
check_factorstructure(data)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Fit an EFA
efa <- psych::fa(data, nfactors = 5) %>% 
  model_parameters(sort = TRUE, threshold = "max")
efa

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  predict(efa, names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness"))

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
head(predict(efa, names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness")), 5)

## ----message=FALSE, warning=FALSE---------------------------------------------
n <- n_factors(data)
n

## ----message=FALSE, warning=FALSE---------------------------------------------
as.data.frame(n)
summary(n)

## ----message=FALSE, warning=FALSE---------------------------------------------
# library(see)

# plot(n) +
#   theme_modern()

## ----message=FALSE, warning=FALSE---------------------------------------------
partitions <- data_partition(data, training_proportion = 0.7)
training <- partitions$training
test <- partitions$test

## ----message=FALSE, warning=FALSE---------------------------------------------
structure_big5 <- psych::fa(training, nfactors = 5) %>% 
  efa_to_cfa()
structure_big6 <- psych::fa(training, nfactors = 6)  %>% 
  efa_to_cfa()

# Investigate how a model looks
structure_big5

## ----message=FALSE, warning=FALSE---------------------------------------------
library(lavaan)
library(performance)

big5 <- lavaan::cfa(structure_big5, data = test)
big6 <- lavaan::cfa(structure_big6, data = test)

performance::compare_performance(big5, big6)

