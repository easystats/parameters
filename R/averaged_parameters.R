library(insight)
library(parameters)
library(MATA)

set.seed(0)
n <- 20 # 'n' is assumed to be even
x1 <- c(rep(0, n / 2), rep(1, n / 2)) # two groups: x1=0, and x1=1
x2 <- rnorm(n, mean = 10, sd = 3)
y <- rnorm(n, mean = 3 * x1 + 0.1 * x2) # data generation

x1 <- factor(x1)
m1 <- glm(y ~ x1) # using 'glm' provides AIC values.
m2 <- glm(y ~ x1 + x2) # using 'lm' doesn't.
aic <- c(m1$aic, m2$aic)
delta.aic <- aic - min(aic)
model.weights <- exp(-0.5 * delta.aic) / sum(exp(-0.5 * delta.aic))

# see also
# performance::compare_performance(m1, m2)

residual.dfs <- c(insight::get_df(m1), insight::get_df(m2))
# parameters::degrees_of_freedom(m1,method="residual")

g1 <- insight::get_datagrid(m1)
g2 <- insight::get_datagrid(m2)

nd1 <- as.data.frame(lapply(g1, function(i) {
  if (is.factor(i)) {
    as.factor(levels(i)[1])
  } else {
    unique(i)[1]
  }
}))


nd2 <- as.data.frame(lapply(g2, function(i) {
  if (is.factor(i)) {
    as.factor(levels(i)[1])
  } else {
    unique(i)[1]
  }
}))

p1 <- get_predicted(m1, data = nd1, ci = .95)
p2 <- get_predicted(m2, data = nd2, , ci = .95)

theta.hats <- c(p1, p2)
se.theta.hats <- c(attributes(p1)$ci_data$SE, attributes(p2)$ci_data$SE)

#  95% MATA-Wald confidence interval for theta:
mata.wald(theta.hats, se.theta.hats, model.weights,
  mata.t = TRUE,
  residual.dfs = residual.dfs
)
