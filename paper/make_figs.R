# --------------------
library(parameters)

model <- lm(Sepal.Length ~ Species, data=iris)
parameters(model)

# --------------------
library(see)

lm(Sepal.Length ~ Petal.Width * Species, data=iris) %>%
  parameters() %>%
  plot()

# save
p <- lm(Sepal.Length ~ Petal.Width * Species, data=iris) %>%
  parameters() %>%
  plot()

ggplot2::ggsave("figure3.png", p, height=8, width=8*1.618034)
