% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equivalence_test.R
\name{equivalence_test.lm}
\alias{equivalence_test.lm}
\alias{equivalence_test.merMod}
\alias{equivalence_test.ggeffects}
\title{Equivalence test}
\usage{
\method{equivalence_test}{lm}(
  x,
  range = "default",
  ci = 0.95,
  rule = "classic",
  verbose = TRUE,
  ...
)

\method{equivalence_test}{merMod}(
  x,
  range = "default",
  ci = 0.95,
  rule = "classic",
  effects = c("fixed", "random"),
  verbose = TRUE,
  ...
)

\method{equivalence_test}{ggeffects}(
  x,
  range = "default",
  rule = "classic",
  test = "pairwise",
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x}{A statistical model.}

\item{range}{The range of practical equivalence of an effect. May be
\code{"default"}, to automatically define this range based on properties of the
model's data.}

\item{ci}{Confidence Interval (CI) level. Default to \code{0.95} (\verb{95\%}).}

\item{rule}{Character, indicating the rules when testing for practical
equivalence. Can be \code{"bayes"}, \code{"classic"} or \code{"cet"}. See
'Details'.}

\item{verbose}{Toggle warnings and messages.}

\item{...}{Arguments passed to or from other methods.}

\item{effects}{Should parameters for fixed effects (\code{"fixed"}), random
effects (\code{"random"}), or both (\code{"all"}) be returned? Only applies
to mixed models. May be abbreviated. If the calculation of random effects
parameters takes too long, you may use \code{effects = "fixed"}.}

\item{test}{Hypothesis test for computing contrasts or pairwise comparisons.
See \href{https://strengejacke.github.io/ggeffects/reference/test_predictions.html}{\code{?ggeffects::test_predictions}}
for details.}
}
\value{
A data frame.
}
\description{
Compute the (conditional) equivalence test for frequentist models.
}
\details{
In classical null hypothesis significance testing (NHST) within a frequentist
framework, it is not possible to accept the null hypothesis, H0 - unlike
in Bayesian statistics, where such probability statements are possible.
"\link{...} one can only reject the null hypothesis if the test
statistics falls into the critical region(s), or fail to reject this
hypothesis. In the latter case, all we can say is that no significant effect
was observed, but one cannot conclude that the null hypothesis is true."
(\emph{Pernet 2017}). One way to address this issues without Bayesian methods
is \emph{Equivalence Testing}, as implemented in \code{equivalence_test()}.
While you either can reject the null hypothesis or claim an inconclusive result
in NHST, the equivalence test - according to \emph{Pernet} - adds a third category,
\emph{"accept"}. Roughly speaking, the idea behind equivalence testing in a
frequentist framework is to check whether an estimate and its uncertainty
(i.e. confidence interval) falls within a region of "practical equivalence".
Depending on the rule for this test (see below), statistical significance
does not necessarily indicate whether the null hypothesis can be rejected or
not, i.e. the classical interpretation of the p-value may differ from the
results returned from the equivalence test.
\subsection{Calculation of equivalence testing}{
\itemize{
\item "bayes" - Bayesian rule (Kruschke 2018)

This rule follows the "HDI+ROPE decision rule" (\emph{Kruschke, 2014, 2018}) used
for the \code{\link[bayestestR:equivalence_test]{Bayesian counterpart()}}. This
means, if the confidence intervals are completely outside the ROPE, the
"null hypothesis" for this parameter is "rejected". If the ROPE
completely covers the CI, the null hypothesis is accepted. Else, it's
undecided whether to accept or reject the null hypothesis. Desirable
results are low proportions inside the ROPE (the closer to zero the
better).
\item "classic" - The TOST rule (Lakens 2017)

This rule follows the "TOST rule", i.e. a two one-sided test procedure
(\emph{Lakens 2017}). Following this rule...
\itemize{
\item practical equivalence is assumed (i.e. H0 \emph{"accepted"}) when the narrow
confidence intervals are completely inside the ROPE, no matter if the
effect is statistically significant or not;
\item practical equivalence (i.e. H0) is \emph{rejected}, when the coefficient is
statistically significant, both when the narrow confidence intervals
(i.e. \code{1-2*alpha}) include or exclude the the ROPE boundaries, but the
narrow confidence intervals are \emph{not fully covered} by the ROPE;
\item else the decision whether to accept or reject practical equivalence is
undecided (i.e. when effects are \emph{not} statistically significant \emph{and}
the narrow confidence intervals overlaps the ROPE).
}
\item "cet" - Conditional Equivalence Testing (Campbell/Gustafson 2018)

The Conditional Equivalence Testing as described by \emph{Campbell and
Gustafson 2018}. According to this rule, practical equivalence is
rejected when the coefficient is statistically significant. When the
effect is \emph{not} significant and the narrow confidence intervals are
completely inside the ROPE, we accept (i.e. assume) practical equivalence,
else it is undecided.
}
}

\subsection{Levels of Confidence Intervals used for Equivalence Testing}{

For \code{rule = "classic"}, "narrow" confidence intervals are used for
equivalence testing. "Narrow" means, the the intervals is not 1 - alpha,
but 1 - 2 * alpha. Thus, if \code{ci = .95}, alpha is assumed to be 0.05
and internally a ci-level of 0.90 is used. \code{rule = "cet"} uses
both regular and narrow confidence intervals, while \code{rule = "bayes"}
only uses the regular intervals.
}

\subsection{p-Values}{

The equivalence p-value is the area of the (cumulative) confidence
distribution that is outside of the region of equivalence. It can be
interpreted as p-value for \emph{rejecting} the alternative hypothesis
and \emph{accepting} the "null hypothesis" (i.e. assuming practical
equivalence). That is, a high p-value means we reject the assumption of
practical equivalence and accept the alternative hypothesis.
}

\subsection{Second Generation p-Value (SGPV)}{

Second generation p-values (SGPV) were proposed as a statistic that
represents \emph{the proportion of data-supported hypotheses that are also null
hypotheses} \emph{(Blume et al. 2018, Lakens and Delacre 2020)}. It represents the
proportion of the confidence interval range (assuming a normally distributed,
equal-tailed interval) that is inside the ROPE.
}

\subsection{ROPE range}{

Some attention is required for finding suitable values for the ROPE limits
(argument \code{range}). See 'Details' in \code{\link[bayestestR:rope_range]{bayestestR::rope_range()}}
for further information.
}
}
\note{
There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method}
implemented in the \href{https://easystats.github.io/see/}{\strong{see}-package}.
}
\examples{
data(qol_cancer)
model <- lm(QoL ~ time + age + education, data = qol_cancer)

# default rule
equivalence_test(model)

# conditional equivalence test
equivalence_test(model, rule = "cet")

# plot method
if (require("see", quietly = TRUE)) {
  result <- equivalence_test(model)
  plot(result)
}
}
\references{
\itemize{
\item Blume, J. D., D'Agostino McGowan, L., Dupont, W. D., & Greevy, R. A.
(2018). Second-generation p-values: Improved rigor, reproducibility, &
transparency in statistical analyses. PLOS ONE, 13(3), e0188299.
https://doi.org/10.1371/journal.pone.0188299
\item Campbell, H., & Gustafson, P. (2018). Conditional equivalence
testing: An alternative remedy for publication bias. PLOS ONE, 13(4),
e0195145. doi: 10.1371/journal.pone.0195145
\item Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with
R, JAGS, and Stan. Academic Press
\item Kruschke, J. K. (2018). Rejecting or accepting parameter values in
Bayesian estimation. Advances in Methods and Practices in Psychological
Science, 1(2), 270-280. doi: 10.1177/2515245918771304
\item Lakens, D. (2017). Equivalence Tests: A Practical Primer for t Tests,
Correlations, and Meta-Analyses. Social Psychological and Personality
Science, 8(4), 355–362. doi: 10.1177/1948550617697177
\item Lakens, D., & Delacre, M. (2020). Equivalence Testing and the Second
Generation P-Value. Meta-Psychology, 4.
https://doi.org/10.15626/MP.2018.933
\item Pernet, C. (2017). Null hypothesis significance testing: A guide to
commonly misunderstood concepts and recommendations for good practice.
F1000Research, 4, 621. doi: 10.12688/f1000research.6963.5
}
}
\seealso{
For more details, see \code{\link[bayestestR:equivalence_test]{bayestestR::equivalence_test()}}.
Further readings can be found in the references.
}
