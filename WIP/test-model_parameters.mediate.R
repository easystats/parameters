if (require("testthat") && require("parameters") && require("mediation")) {
  library(insight)

  data(jobs)
  b <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
  c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
  m <- mediate(b, c, sims=50, treat="treat", mediator="job_seek")

  summary(m)


  b2 <- lm(job_seek ~ educ + sex, data=jobs)
  c2 <- lm(depress2 ~ educ + job_seek + sex, data=jobs)

  m2 <- mediate(b2, c2, treat="educ", mediator="job_seek", sims=50, control.value = "gradwk", treat.value = "somcol")

  summary(m2)

  jobs$job_disc <- as.factor(jobs$job_disc)
  b.ord <- polr(job_disc ~ treat + econ_hard + sex + age, data=jobs,
                method="probit", Hess=TRUE)
  d.bin <- glm(work1 ~ treat + job_disc + econ_hard + sex + age, data=jobs,
               family=binomial(link="probit"))
  m3 <- mediate(b.ord, d.bin, sims=50, treat="treat", mediator="job_disc")
}
