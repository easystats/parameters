# Dominance Analysis

Computes Dominance Analysis Statistics and Designations

## Usage

``` r
dominance_analysis(
  model,
  sets = NULL,
  all = NULL,
  conditional = TRUE,
  complete = TRUE,
  quote_args = NULL,
  contrasts = model$contrasts,
  ...
)
```

## Arguments

- model:

  A model object supported by
  [`performance::r2()`](https://easystats.github.io/performance/reference/r2.html).
  See 'Details'.

- sets:

  A (named) list of formula objects with no left hand side/response. If
  the list has names, the name provided each element will be used as the
  label for the set. Unnamed list elements will be provided a set number
  name based on its position among the sets as entered.

  Predictors in each formula are bound together as a set in the
  dominance analysis and dominance statistics and designations are
  computed for the predictors together. Predictors in `sets` must be
  present in the model submitted to the `model` argument and cannot be
  in the `all` argument.

- all:

  A formula with no left hand side/response.

  Predictors in the formula are included in each subset in the dominance
  analysis and the R2 value associated with them is subtracted from the
  overall value. Predictors in `all` must be present in the model
  submitted to the `model` argument and cannot be in the `sets`
  argument.

- conditional:

  Logical. If `FALSE` then conditional dominance matrix is not computed.

  If conditional dominance is not desired as an importance criterion,
  avoiding computing the conditional dominance matrix can save
  computation time.

- complete:

  Logical. If `FALSE` then complete dominance matrix is not computed.

  If complete dominance is not desired as an importance criterion,
  avoiding computing complete dominance designations can save
  computation time.

- quote_args:

  A character vector of arguments in the model submitted to `model` to
  [`quote()`](https://rdrr.io/r/base/substitute.html) prior to
  submitting to the dominance analysis. This is necessary for data
  masked arguments (e.g., `weights`) to prevent them from being
  evaluated before being applied to the model and causing an error.

- contrasts:

  A named list of [`contrasts`](https://rdrr.io/r/stats/contrasts.html)
  used by the model object. This list is required in order for the
  correct mapping of parameters to predictors in the output when the
  model creates indicator codes for factor variables using
  [`insight::get_modelmatrix()`](https://easystats.github.io/insight/reference/get_modelmatrix.html).
  By default, the `contrast` element from the model object submitted is
  used. If the model object does not have a `contrast` element the user
  can supply this named list.

- ...:

  Not used at current.

## Value

Object of class `"parameters_da"`.

An object of class `"parameters_da"` is a list of `data.frame`s composed
of the following elements:

- `General`:

  A `data.frame` which associates dominance statistics with model
  parameters. The variables in this `data.frame` include:

  `Parameter`

  :   Parameter names.

  `General_Dominance`

  :   Vector of general dominance statistics. The R2 ascribed to
      variables in the `all` argument are also reported here though they
      are not general dominance statistics.

  `Percent`

  :   Vector of general dominance statistics normalized to sum to 1.

  `Ranks`

  :   Vector of ranks applied to the general dominance statistics.

  `Subset`

  :   Names of the subset to which the parameter belongs in the
      dominance analysis. Each other `data.frame` returned will refer to
      these subset names.

- `Conditional`:

  A `data.frame` of conditional dominance statistics. Each observation
  represents a subset and each variable represents an the average
  increment to R2 with a specific number of subsets in the model. `NULL`
  if `conditional` argument is `FALSE`.

- `Complete`:

  A `data.frame` of complete dominance designations. The subsets in the
  observations are compared to the subsets referenced in each variable.
  Whether the subset in each variable dominates the subset in each
  observation is represented in the logical value. `NULL` if `complete`
  argument is `FALSE`.

## Details

Computes two decompositions of the model's R2 and returns a matrix of
designations from which predictor relative importance determinations can
be obtained.

Note in the output that the "constant" subset is associated with a
component of the model that does not directly contribute to the R2 such
as an intercept. The "all" subset is apportioned a component of the fit
statistic but is not considered a part of the dominance analysis and
therefore does not receive a rank, conditional dominance statistics, or
complete dominance designations.

The input model is parsed using
[`insight::find_predictors()`](https://easystats.github.io/insight/reference/find_predictors.html),
does not yet support interactions, transformations, or offsets applied
in the R formula, and will fail with an error if any such terms are
detected.

The model submitted must accept an formula object as a `formula`
argument. In addition, the model object must accept the data on which
the model is estimated as a `data` argument. Formulas submitted using
object references (i.e., `lm(mtcars$mpg ~ mtcars$vs)`) and functions
that accept data as a non-`data` argument (e.g.,
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) uses
`design`) will fail with an error.

Models that return `TRUE` for the
[`insight::model_info()`](https://easystats.github.io/insight/reference/model_info.html)
function's values "is_bayesian", "is_mixed", "is_gam", is_multivariate",
"is_zero_inflated", or "is_hurdle" are not supported at current.

When
[`performance::r2()`](https://easystats.github.io/performance/reference/r2.html)
returns multiple values, only the first is used by default.

## References

- Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for
  comparing predictors in multiple regression. Psychological Methods,
  8(2), 129-148. doi:10.1037/1082-989X.8.2.129

- Budescu, D. V. (1993). Dominance analysis: A new approach to the
  problem of relative importance of predictors in multiple regression.
  Psychological Bulletin, 114(3), 542-551.
  doi:10.1037/0033-2909.114.3.542

- Groemping, U. (2007). Estimators of relative importance in linear
  regression based on variance decomposition. The American Statistician,
  61(2), 139-147. doi:10.1198/000313007X188252

## See also

[`domir::domin()`](https://jluchman.github.io/domir/reference/domin.html)

## Author

Joseph Luchman

## Examples

``` r
data(mtcars)

# Dominance Analysis with Logit Regression
model <- glm(vs ~ cyl + carb + mpg, data = mtcars, family = binomial())

performance::r2(model)
#> # R2 for Logistic Regression
#>   Tjur's R2: 0.741
dominance_analysis(model)
#> # Dominance Analysis Results
#> 
#> Model R2 Value:  0.741 
#> 
#> General Dominance Statistics
#> 
#> Parameter   | General Dominance | Percent | Ranks |   Subset
#> ------------------------------------------------------------
#> (Intercept) |                   |         |       | constant
#> cyl         |             0.366 |   0.493 |     1 |      cyl
#> carb        |             0.178 |   0.241 |     3 |     carb
#> mpg         |             0.197 |   0.266 |     2 |      mpg
#> 
#> Conditional Dominance Statistics
#> 
#> Subset | IVs: 1 | IVs: 2 | IVs: 3
#> ---------------------------------
#> cyl    |  0.654 |  0.254 |  0.190
#> carb   |  0.394 |  0.066 |  0.074
#> mpg    |  0.474 |  0.085 |  0.032
#> 
#> Complete Dominance Designations
#> 
#> Subset | < cyl | < carb | < mpg
#> -------------------------------
#> cyl    |       |  FALSE | FALSE
#> carb   |  TRUE |        |      
#> mpg    |  TRUE |        |      

# Dominance Analysis with Weighted Logit Regression
model_wt <- glm(vs ~ cyl + carb + mpg,
  data = mtcars,
  weights = wt, family = quasibinomial()
)

dominance_analysis(model_wt, quote_args = "weights")
#> # Dominance Analysis Results
#> 
#> Model R2 Value:  0.776 
#> 
#> General Dominance Statistics
#> 
#> Parameter   | General Dominance | Percent | Ranks |   Subset
#> ------------------------------------------------------------
#> (Intercept) |                   |         |       | constant
#> cyl         |             0.390 |   0.503 |     1 |      cyl
#> carb        |             0.174 |   0.224 |     3 |     carb
#> mpg         |             0.212 |   0.273 |     2 |      mpg
#> 
#> Conditional Dominance Statistics
#> 
#> Subset | IVs: 1 | IVs: 2 | IVs: 3
#> ---------------------------------
#> cyl    |  0.679 |  0.279 |  0.213
#> carb   |  0.376 |  0.062 |  0.083
#> mpg    |  0.496 |  0.100 |  0.039
#> 
#> Complete Dominance Designations
#> 
#> Subset | < cyl | < carb | < mpg
#> -------------------------------
#> cyl    |       |  FALSE | FALSE
#> carb   |  TRUE |        |      
#> mpg    |  TRUE |        |      
```
