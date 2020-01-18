Structural Models (EFA, CFA, SEM…)
================

  - [How to perform a Factor Analysis
    (FA)](#how-to-perform-a-factor-analysis-fa)
      - [Factor Structure (Sphericity and
        KMO)](#factor-structure-sphericity-and-kmo)
      - [Exploratory Factor Analysis
        (EFA)](#exploratory-factor-analysis-efa)
      - [How many factors to retain in Factor Analysis
        (FA)](#how-many-factors-to-retain-in-factor-analysis-fa)
          - [The Method Agreement
            procedure](#the-method-agreement-procedure)
      - [Confirmatory Factor Analysis
        (CFA)](#confirmatory-factor-analysis-cfa)
          - [Partition the data](#partition-the-data)
          - [Create CFA structures out of EFA
            models](#create-cfa-structures-out-of-efa-models)
          - [Fit and Compare models](#fit-and-compare-models)
  - [References](#references)

# How to perform a Factor Analysis (FA)

The difference between PCA and EFA can be quite hard to intuitively
grasp as their output is very familiar. The idea is that PCA aims at
extracting the most variance possible from all variables of the dataset,
whereas EFA aims at creating consistent factors from the dataset without
desperately trying to represent all the variables.

This is why PCA is popular for feature reduction, as it will try to best
represent the variance contained in the original data, minimizing the
loss of information. On the other hand, EFA is usually in the context of
exploring the latent dimensions that might be hidden in the observed
variables, without necessary striving at representing the whole dataset.

To illustrate EFA, let us use the [International Personality Item
Pool](ipip.ori.org) data available in the
[`psych`](https://www.personality-project.org/r/html/bfi.html) package.
It includes 25 personality self report items. The authors built these
items following the **big 5** personality structure.

## Factor Structure (Sphericity and KMO)

The first step is to test the dataset for factor analysis suitability.
Two existing methods are the **Bartlett’s Test of Sphericity** and the
**Kaiser, Meyer, Olkin (KMO) Measure of Sampling Adequacy (MSA)**. The
former tests whether a matrix is significantly different from an
identity matrix. This statistical test for the presence of correlations
among variables, providing the statistical probability that the
correlation matrix has significant correlations among at least some of
variables. As for factor analysis to work, some relationships between
variables are needed, thus, a significant Bartlett’s test of sphericity
is required, say *p* \< .001. The latter was introduced by Kaiser (1970)
as the Measure of Sampling Adequacy (MSA), later modified by Kaiser and
Rice (1974). The Kaiser-Meyer-Olkin (KMO) statistic, which can vary from
0 to 1, indicates the degree to which each variable in a set is
predicted without error by the other variables. A value of 0 indicates
that the sum of partial correlations is large relative to the sum
correlations, indicating factor analysis is likely to be inappropriate.
A KMO value close to 1 indicates that the sum of partial correlations is
not large relative to the sum of correlations and so factor analysis
should yield distinct and reliable factors.

Both tests can be performed by using the `check_factorstructure()`
function.

``` r
library(parameters)
library(dplyr)
library(psych)

# Load the data
data <- psych::bfi[, 1:25]  # Select only the 25 first columns corresponding to the items
data <- na.omit(data)  # remove missing values

# Check factor structure
check_factorstructure(data)
```

    > # Is the data suitable for Factor Analysis?
    > 
    >   - KMO: The Kaiser, Meyer, Olkin (KMO) measure of sampling adequacy suggests that data seems appropriate for factor analysis (KMO = 0.85).
    >   - Sphericity: Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analaysis (Chisq(300) = 18146.07, p < .001).

## Exploratory Factor Analysis (EFA)

Now that we are confident that our dataset is appropriate, we will
explore a factor structure made of 5 latent variables, corresponding to
the items’ authors theory of personality.

``` r
# Fit an EFA
efa <- psych::fa(data, nfactors = 5) %>% 
  model_parameters(sort = TRUE, threshold = "max")
efa
```

    > # Rotated loadings from Principal Component Analysis (oblimin-rotation)
    > 
    > Variable |  MR2 |   MR3 |   MR1 |   MR5 |   MR4 | Complexity | Uniqueness
    > -------------------------------------------------------------------------
    > N1       | 0.83 |       |       |       |       |       1.07 |       0.32
    > N2       | 0.78 |       |       |       |       |       1.03 |       0.39
    > N3       | 0.70 |       |       |       |       |       1.08 |       0.46
    > N5       | 0.48 |       |       |       |       |       2.00 |       0.65
    > N4       | 0.47 |       |       |       |       |       2.33 |       0.49
    > C2       |      |  0.67 |       |       |       |       1.18 |       0.55
    > C4       |      | -0.64 |       |       |       |       1.13 |       0.52
    > C3       |      |  0.57 |       |       |       |       1.10 |       0.68
    > C5       |      | -0.56 |       |       |       |       1.41 |       0.56
    > C1       |      |  0.55 |       |       |       |       1.20 |       0.65
    > E2       |      |       |  0.67 |       |       |       1.08 |       0.45
    > E4       |      |       | -0.59 |       |       |       1.52 |       0.46
    > E1       |      |       |  0.55 |       |       |       1.22 |       0.65
    > E5       |      |       | -0.42 |       |       |       2.68 |       0.59
    > E3       |      |       | -0.41 |       |       |       2.65 |       0.56
    > A3       |      |       |       |  0.68 |       |       1.06 |       0.46
    > A2       |      |       |       |  0.66 |       |       1.03 |       0.54
    > A5       |      |       |       |  0.54 |       |       1.48 |       0.53
    > A4       |      |       |       |  0.45 |       |       1.74 |       0.70
    > A1       |      |       |       | -0.44 |       |       1.88 |       0.80
    > O3       |      |       |       |       |  0.62 |       1.16 |       0.53
    > O5       |      |       |       |       | -0.54 |       1.21 |       0.70
    > O1       |      |       |       |       |  0.52 |       1.10 |       0.68
    > O2       |      |       |       |       | -0.47 |       1.68 |       0.73
    > O4       |      |       |       |       |  0.36 |       2.65 |       0.75
    > 
    > The 5 latent factors (oblimin rotation) accounted for 42.36% of the total variance of the original data (MR2 = 10.31%, MR3 = 8.39%, MR1 = 8.83%, MR5 = 8.29%, MR4 = 6.55%).

As we can see, the 25 items nicely spread on the 5 latent factors, the
famous **big 5**. Based on this model, we can now predict back the
scores for each individual for these new variables:

``` r
predict(efa, names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness"))
```

    >   Neuroticism Conscientiousness Extraversion Agreeableness Opennness
    > 1       -0.22            -1.327       -0.128        -0.855     -1.61
    > 2        0.16            -0.572       -0.466        -0.072     -0.17
    > 3        0.62            -0.043       -0.141        -0.552      0.23
    > 4       -0.12            -1.063       -0.058        -0.091     -1.06
    > 5       -0.17            -0.099       -0.460        -0.712     -0.66

## How many factors to retain in Factor Analysis (FA)

When running a **factor analysis (FA)**, one often needs to specify
**how many components** (or latent variables) to retain or to extract.
This decision is often motivated or supported by some statistical
indices and procedures aiming at finding the optimal number of factors.

Interestingly, a huge amount of methods exist to statistically address
this issue, giving sometimes very different results… Unfortunately,
there is no consensus on **which method to use**, or which is the best.

### The Method Agreement procedure

The Method Agreement procedure, first implemented in the
[`psycho`](https://neuropsychology.github.io/psycho.R/2018/05/24/n_factors.html)
package (Makowski 2018), proposes to rely on the consensus of methods,
rather than on one method in particular.

This procedure can be easily used via the `n_factors()` function,
re-implemented and improved in the
[**parameters**](https://github.com/easystats/parameters) package. One
can provide a dataframe, and the function will run a large number of
routines and return the optimal number of factors based on the higher
consensus.

``` r
n <- n_factors(data)
n
```

    > # Method Agreement Procedure:
    > 
    > The choice of 1 dimensions is supported by 3 (13.04%) methods out of 23 (Acceleration factor, TLI, RMSEA).

Interestingly, the smallest nubmer of factors that most methods suggest
is 6… Which is consistent whith the newer models of personality (e.g.,
HEXACO).

More details, as well as a summary table can be obtained as follows:

``` r
as.data.frame(n)
```

    >    n_Factors              Method              Family
    > 1          1 Acceleration factor               Scree
    > 2          1                 TLI                 Fit
    > 3          1               RMSEA                 Fit
    > 4          3                 CNG                 CNG
    > 5          4                beta Multiple_regression
    > 6          4    VSS complexity 1                 VSS
    > 7          5    VSS complexity 2                 VSS
    > 8          5       Velicer's MAP        Velicers_MAP
    > 9          6 Optimal coordinates               Scree
    > 10         6   Parallel analysis               Scree
    > 11         6    Kaiser criterion               Scree
    > 12         7                   t Multiple_regression
    > 13         7                   p Multiple_regression
    > 14         7                  R2            Scree_SE
    > 15         8            SE Scree            Scree_SE
    > 16         8                 BIC                 BIC
    > 17         8                 BIC                 Fit
    > 18        11      BIC (adjusted)                 BIC
    > 19        18                CRMS                 Fit
    > 20        22             Bentler             Bentler
    > 21        24            Bartlett             Barlett
    > 22        24            Anderson             Barlett
    > 23        24              Lawley             Barlett

``` r
summary(n)
```

    >    n_Factors n_Methods
    > 1          1         3
    > 2          3         1
    > 3          4         2
    > 4          5         2
    > 5          6         3
    > 6          7         3
    > 7          8         3
    > 8         11         1
    > 9         18         1
    > 10        22         1
    > 11        24         3

A plot can also be obtained (the `see` package must be loaded):

``` r
# library(see)

# plot(n) +
#   theme_modern()
```

## Confirmatory Factor Analysis (CFA)

We’ve seen above that while an EFA with 5 latent variables works great
on our dataset, a structure with 6 latent factors might in fact be more
appropriate. How can we **statistically test** if that’s actually the
case? This can be done using Confirmatory Factor Analysis (CFA), that
bridges factor analysis with Structural Equation Modelling (SEM).

However, in order to do that cleanly, EFA should be independent from
CFA, in the sense that the factor structure should be explored on a
**“training” set**, and then tested (or “confirmed”) on a **test
set**. In other words, the dataset used for exploration and confirmation
is not the same. Note that this procedure is also standard in the field
of machine learning.

### Partition the data

The data can be easily split into two sets with the `data_partition()`
function, through which we will use 70% of the sample as training and
the rest as test.

``` r
partitions <- data_partition(data, training_proportion = 0.7)
training <- partitions$training
test <- partitions$test
```

### Create CFA structures out of EFA models

In the next step, we will run two EFA models on the training set,
specifying 5 and 6 latent factors respectively, that we will then
transform into CFA structures.

``` r
structure_big5 <- psych::fa(training, nfactors = 5) %>% 
  efa_to_cfa()
structure_big6 <- psych::fa(training, nfactors = 6)  %>% 
  efa_to_cfa()

# Investigate how a model looks
structure_big5
```

    > # Latent variables
    > MR2 =~ N1 + N2 + N3 + N4 + N5
    > MR1 =~ E1 + E2 + E3 + E4 + E5
    > MR3 =~ C1 + C2 + C3 + C4 + C5
    > MR5 =~ A1 + A2 + A3 + A4 + A5
    > MR4 =~ O1 + O2 + O3 + O4 + O5

As we can see, a structure is just a string encoding how the **manifest
variables** (the observed variables) are integrated into latent
variables.

### Fit and Compare models

We can finally with that structure to the test set using the `lavaan`
package, and compare these models together:

``` r
library(lavaan)
library(performance)

big5 <- lavaan::cfa(structure_big5, data = test)
big6 <- lavaan::cfa(structure_big6, data = test)

performance::compare_performance(big5, big6)
```

    > # Comparison of Model Performance Indices
    > 
    > Model |   Type |   Chisq | Chisq_df | Chisq_p | Baseline | Baseline_df | Baseline_p |  GFI | AGFI |  NFI | NNFI |  CFI | RMSEA | RMSEA_CI_low | RMSEA_CI_high | RMSEA_p |  RMR | SRMR |  RFI | PNFI |  IFI |  RNI | Loglikelihood |      AIC |      BIC | BIC_adjusted | BF
    > ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    > big5  | lavaan | 1357.27 |      265 |       0 |  5821.53 |         300 |          0 | 0.86 | 0.82 | 0.77 | 0.78 | 0.80 |  0.07 |         0.07 |          0.08 |       0 | 0.16 | 0.08 | 0.74 | 0.68 | 0.80 | 0.80 |     -29897.05 | 59914.10 | 60189.76 |     59999.24 |   
    > big6  | lavaan | 1529.93 |      265 |       0 |  5821.53 |         300 |          0 | 0.84 | 0.81 | 0.74 | 0.74 | 0.77 |  0.08 |         0.08 |          0.08 |       0 | 0.18 | 0.08 | 0.70 | 0.65 | 0.77 | 0.77 |     -29983.38 | 60086.76 | 60362.43 |     60171.91 |  0

All in all, it seems that the big 5 structure remains quite reliable.

# References

<div id="refs" class="references">

<div id="ref-makowski2018psycho">

Makowski, Dominique. 2018. “The Psycho Package: An Efficient and
Publishing-Oriented Workflow for Psychological Science.” *Journal of
Open Source Software* 3 (22): 470.

</div>

</div>
