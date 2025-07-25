% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_analysis.R, R/principal_components.R,
%   R/print_html.R, R/utils_pca_efa.R
\name{factor_analysis}
\alias{factor_analysis}
\alias{factor_analysis.data.frame}
\alias{factor_analysis.matrix}
\alias{principal_components}
\alias{rotated_data}
\alias{principal_components.data.frame}
\alias{print_html.parameters_efa}
\alias{predict.parameters_efa}
\alias{print.parameters_efa}
\alias{sort.parameters_efa}
\alias{closest_component}
\title{Principal Component Analysis (PCA) and Factor Analysis (FA)}
\usage{
factor_analysis(x, ...)

\method{factor_analysis}{data.frame}(
  x,
  n = "auto",
  rotation = "oblimin",
  factor_method = "minres",
  sort = FALSE,
  threshold = NULL,
  standardize = FALSE,
  ...
)

\method{factor_analysis}{matrix}(
  x,
  n = "auto",
  rotation = "oblimin",
  factor_method = "minres",
  n_obs = NULL,
  sort = FALSE,
  threshold = NULL,
  standardize = FALSE,
  ...
)

principal_components(x, ...)

rotated_data(x, verbose = TRUE)

\method{principal_components}{data.frame}(
  x,
  n = "auto",
  rotation = "none",
  sparse = FALSE,
  sort = FALSE,
  threshold = NULL,
  standardize = TRUE,
  ...
)

\method{print_html}{parameters_efa}(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...)

\method{predict}{parameters_efa}(
  object,
  newdata = NULL,
  names = NULL,
  keep_na = TRUE,
  verbose = TRUE,
  ...
)

\method{print}{parameters_efa}(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...)

\method{sort}{parameters_efa}(x, ...)

closest_component(x)
}
\arguments{
\item{x}{A data frame or a statistical model. For \code{closest_component()}, the
output of the \code{principal_components()} function.}

\item{...}{Arguments passed to or from other methods.}

\item{n}{Number of components to extract. If \code{n="all"}, then \code{n} is set as
the number of variables minus 1 (\code{ncol(x)-1}). If \code{n="auto"} (default) or
\code{n=NULL}, the number of components is selected through \code{\link[=n_factors]{n_factors()}}
resp. \code{\link[=n_components]{n_components()}}. Else, if \code{n} is a number, \code{n} components are
extracted. If \code{n} exceeds number of variables in the data, it is
automatically set to the maximum number (i.e. \code{ncol(x)}). In
\code{\link[=reduce_parameters]{reduce_parameters()}}, can also be \code{"max"}, in which case it will select
all the components that are maximally pseudo-loaded (i.e., correlated) by
at least one variable.}

\item{rotation}{If not \code{"none"}, the PCA / FA will be computed using the
\strong{psych} package. Possible options include \code{"varimax"}, \code{"quartimax"},
\code{"promax"}, \code{"oblimin"}, \code{"simplimax"}, or \code{"cluster"} (and more). See
\code{\link[psych:fa]{psych::fa()}} for details. The default is \code{"none"} for PCA, and
\code{"oblimin"} for FA.}

\item{factor_method}{The factoring method to be used. Passed to the \code{fm}
argument in \code{psych::fa()}. Defaults to \code{"minres"} (minimum residual). Other
options include \code{"uls"}, \code{"ols"}, \code{"wls"}, \code{"gls"}, \code{"ml"}, \code{"minchi"},
\code{"minrank"}, \code{"old.min"}, and \code{"alpha"}. See \code{?psych::fa} for details.}

\item{sort}{Sort the loadings.}

\item{threshold}{A value between 0 and 1 indicates which (absolute) values
from the loadings should be removed. An integer higher than 1 indicates the
n strongest loadings to retain. Can also be \code{"max"}, in which case it will
only display the maximum loading per variable (the most simple structure).}

\item{standardize}{A logical value indicating whether the variables should be
standardized (centered and scaled) to have unit variance before the
analysis (in general, such scaling is advisable). \strong{Note:} This defaults
to \code{TRUE} for PCA, but to \code{FALSE} for FA (because \code{factor_analysis()}
computes a correlation matrix and uses that r-matrix for the factor analysis
by default - therefore, standardization of the raw variables is unnecessary,
and even undesirable when using \code{cor = "poly"}).}

\item{n_obs}{An integer or a matrix.
\itemize{
\item \strong{Integer:} Number of observations in the original data set if \code{x} is a
correlation matrix. Required to compute correct fit indices.
\item \strong{Matrix:} A matrix where each cell \verb{[i, j]} specifies the number of
pairwise complete observations used to compute the correlation between
variable \code{i} and variable \code{j} in the input \code{x}. It is crucial when \code{x} is
a correlation matrix (rather than raw data), especially if that matrix
was derived from a dataset containing missing values using pairwise
deletion. Providing a matrix allows \code{psych::fa()} to accurately calculate
statistical measures, such as chi-square fit statistics, by accounting
for the varying sample sizes that contribute to each individual
correlation coefficient.
}}

\item{verbose}{Toggle warnings.}

\item{sparse}{Whether to compute sparse PCA (SPCA, using \code{\link[sparsepca:spca]{sparsepca::spca()}}).
SPCA attempts to find sparse loadings (with few nonzero values), which improves
interpretability and avoids overfitting. Can be \code{TRUE} or \code{"robust"} (see
\code{\link[sparsepca:robspca]{sparsepca::robspca()}}).}

\item{digits}{Argument for \code{print()}, indicates the number of digits
(rounding) to be used.}

\item{labels}{Argument for \code{print()}, character vector of same length as
columns in \code{x}. If provided, adds an additional column with the labels.}

\item{object}{An object of class \code{parameters_pca}, \code{parameters_efa} or
\code{psych_efa}.}

\item{newdata}{An optional data frame in which to look for variables with
which to predict. If omitted, the fitted values are used.}

\item{names}{Optional character vector to name columns of the returned data
frame.}

\item{keep_na}{Logical, if \code{TRUE}, predictions also return observations
with missing values from the original data, hence the number of rows of
predicted data and original data is equal.}
}
\value{
A data frame of loadings. For \code{factor_analysis()}, this data frame is
also of class \code{parameters_efa()}. Objects from \code{principal_components()} are
of class \code{parameters_pca()}.
}
\description{
The functions \code{principal_components()} and \code{factor_analysis()} can be used to
perform a principal component analysis (PCA) or a factor analysis (FA). They
return the loadings as a data frame, and various methods and functions are
available to access / display other information (see the 'Details' section).
}
\details{
\subsection{Methods and Utilities}{
\itemize{
\item \code{\link[=n_components]{n_components()}} and \code{\link[=n_factors]{n_factors()}} automatically estimates the optimal
number of dimensions to retain.
\item \code{\link[performance:check_factorstructure]{performance::check_factorstructure()}} checks the suitability of the
data for factor analysis using the sphericity (see
\code{\link[performance:check_factorstructure]{performance::check_sphericity_bartlett()}}) and the KMO (see
\code{\link[performance:check_factorstructure]{performance::check_kmo()}}) measure.
\item \code{\link[performance:check_itemscale]{performance::check_itemscale()}} computes various measures of internal
consistencies applied to the (sub)scales (i.e., components) extracted from
the PCA.
\item Running \code{summary()} returns information related to each component/factor,
such as the explained variance and the Eivenvalues.
\item Running \code{\link[=get_scores]{get_scores()}} computes scores for each subscale.
\item \code{\link[=factor_scores]{factor_scores()}} extracts the factor scores from objects returned by
\code{\link[psych:fa]{psych::fa()}}, \code{\link[=factor_analysis]{factor_analysis()}}, or \code{\link[psych:omega]{psych::omega()}}.
\item Running \code{\link[=closest_component]{closest_component()}} will return a numeric vector with the
assigned component index for each column from the original data frame.
\item Running \code{\link[=rotated_data]{rotated_data()}} will return the rotated data, including missing
values, so it matches the original data frame.
\item \code{performance::item_omega()} is a convenient wrapper around \code{psych::omega()},
which provides some additional methods to work seamlessly within the
\emph{easystats} framework.
\item \code{\link[performance:check_normality]{performance::check_normality()}} checks residuals from objects returned
by \code{\link[psych:fa]{psych::fa()}}, \code{\link[=factor_analysis]{factor_analysis()}}, \code{performance::item_omega()},
or \code{\link[psych:omega]{psych::omega()}} for normality.
\item \code{\link[performance:model_performance]{performance::model_performance()}} returns fit-indices for objects returned
by \code{\link[psych:fa]{psych::fa()}}, \code{\link[=factor_analysis]{factor_analysis()}}, or \code{\link[psych:omega]{psych::omega()}}.
\item Running
\href{https://easystats.github.io/see/articles/parameters.html#principal-component-analysis}{\code{plot()}}
visually displays the loadings (that requires the
\href{https://easystats.github.io/see/}{\strong{see}-package} to work).
}
}

\subsection{Complexity}{

Complexity represents the number of latent components needed to account
for the observed variables. Whereas a perfect simple structure solution
has a complexity of 1 in that each item would only load on one factor,
a solution with evenly distributed items has a complexity greater than 1
(\emph{Hofman, 1978; Pettersson and Turkheimer, 2010}).
}

\subsection{Uniqueness}{

Uniqueness represents the variance that is 'unique' to the variable and
not shared with other variables. It is equal to \code{1 - communality}
(variance that is shared with other variables). A uniqueness of \code{0.20}
suggests that \verb{20\%} or that variable's variance is not shared with other
variables in the overall factor model. The greater 'uniqueness' the lower
the relevance of the variable in the factor model.
}

\subsection{MSA}{

MSA represents the Kaiser-Meyer-Olkin Measure of Sampling Adequacy
(\emph{Kaiser and Rice, 1974}) for each item. It indicates whether there is
enough data for each factor give reliable results for the PCA. The value
should be > 0.6, and desirable values are > 0.8 (\emph{Tabachnick and Fidell, 2013}).
}

\subsection{PCA or FA?}{

There is a simplified rule of thumb that may help do decide whether to run
a factor analysis or a principal component analysis:
\itemize{
\item Run \emph{factor analysis} if you assume or wish to test a theoretical model of
\emph{latent factors} causing observed variables.
\item Run \emph{principal component analysis} If you want to simply \emph{reduce} your
correlated observed variables to a smaller set of important independent
composite variables.
}

(Source: \href{https://stats.stackexchange.com/q/1576/54740}{CrossValidated})
}

\subsection{Computing Item Scores}{

Use \code{\link[=get_scores]{get_scores()}} to compute scores for the "subscales" represented by the
extracted principal components or factors. \code{get_scores()} takes the results
from \code{principal_components()} or \code{factor_analysis()} and extracts the
variables for each component found by the PCA. Then, for each of these
"subscales", raw means are calculated (which equals adding up the single
items and dividing by the number of items). This results in a sum score for
each component from the PCA, which is on the same scale as the original,
single items that were used to compute the PCA. One can also use \code{predict()}
to back-predict scores for each component, to which one can provide \code{newdata}
or a vector of \code{names} for the components.
}

\subsection{Explained Variance and Eingenvalues}{

Use \code{summary()} to get the Eigenvalues and the explained variance for each
extracted component. The eigenvectors and eigenvalues represent the "core"
of a PCA: The eigenvectors (the principal components) determine the
directions of the new feature space, and the eigenvalues determine their
magnitude. In other words, the eigenvalues explain the variance of the
data along the new feature axes.
}
}
\examples{
\dontshow{if (require("nFactors", quietly = TRUE) && require("sparsepca", quietly = TRUE) && require("psych", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(parameters)

\donttest{
# Principal Component Analysis (PCA) -------------------
principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)

# Automated number of components
principal_components(mtcars[, 1:4], n = "auto")

# labels can be useful if variable names are not self-explanatory
print(
  principal_components(mtcars[, 1:4], n = "auto"),
  labels = c(
    "Miles/(US) gallon",
    "Number of cylinders",
    "Displacement (cu.in.)",
    "Gross horsepower"
  )
)

# Sparse PCA
principal_components(mtcars[, 1:7], n = 4, sparse = TRUE)
principal_components(mtcars[, 1:7], n = 4, sparse = "robust")

# Rotated PCA
principal_components(mtcars[, 1:7],
  n = 2, rotation = "oblimin",
  threshold = "max", sort = TRUE
)
principal_components(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)

pca <- principal_components(mtcars[, 1:5], n = 2, rotation = "varimax")
pca # Print loadings
summary(pca) # Print information about the factors
predict(pca, names = c("Component1", "Component2")) # Back-predict scores

# which variables from the original data belong to which extracted component?
closest_component(pca)
}

# Factor Analysis (FA) ------------------------

factor_analysis(mtcars[, 1:7], n = "all", threshold = 0.2, rotation = "Promax")
factor_analysis(mtcars[, 1:7], n = 2, threshold = "max", sort = TRUE)
factor_analysis(mtcars[, 1:7], n = 2, rotation = "none", threshold = 2, sort = TRUE)

efa <- factor_analysis(mtcars[, 1:5], n = 2)
summary(efa)
predict(efa, verbose = FALSE)

\donttest{
# Automated number of components
factor_analysis(mtcars[, 1:4], n = "auto")
}
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Kaiser, H.F. and Rice. J. (1974). Little jiffy, mark iv. Educational
and Psychological Measurement, 34(1):111–117
\item Hofmann, R. (1978). Complexity and simplicity as objective indices
descriptive of factor solutions. Multivariate Behavioral Research, 13:2,
247-250, \doi{10.1207/s15327906mbr1302_9}
\item Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation,
and simple structure in personality data. Journal of research in
personality, 44(4), 407-420, \doi{10.1016/j.jrp.2010.03.002}
\item Tabachnick, B. G., and Fidell, L. S. (2013). Using multivariate
statistics (6th ed.). Boston: Pearson Education.
}
}
