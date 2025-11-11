# Package index

## Comprehensive Model Parameters

- [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  [`compare_models()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  : Compare model parameters of multiple models
- [`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.md)
  : Dominance Analysis
- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  [`parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  : Model Parameters
- [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md)
  : Pool Model Parameters
- [`random_parameters()`](https://easystats.github.io/parameters/reference/random_parameters.md)
  : Summary information from random effects
- [`format(`*`<parameters_model>`*`)`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
  [`print(`*`<parameters_model>`*`)`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
  [`summary(`*`<parameters_model>`*`)`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
  [`print_html(`*`<parameters_model>`*`)`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
  [`print_md(`*`<parameters_model>`*`)`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
  : Print model parameters
- [`sort_parameters()`](https://easystats.github.io/parameters/reference/sort_parameters.md)
  : Sort parameters by coefficient values
- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
  [`standardize_posteriors()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
  : Parameters standardization
- [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.md)
  : Get Standardization Information

## Documentation of Specific Class Objects

- [`model_parameters(`*`<aov>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.aov.md)
  : Parameters from ANOVAs

- [`model_parameters(`*`<befa>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.befa.md)
  : Parameters from Bayesian Exploratory Factor Analysis

- [`model_parameters(`*`<default>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.default.md)
  : Parameters from (General) Linear Models

- [`model_parameters(`*`<zcpglm>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.zcpglm.md)
  : Parameters from Zero-Inflated Models

- [`model_parameters(`*`<cgam>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.cgam.md)
  : Parameters from Generalized Additive (Mixed) Models

- [`model_parameters(`*`<mlm>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.mlm.md)
  : Parameters from multinomial or cumulative link models

- [`model_parameters(`*`<glmmTMB>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.glmmTMB.md)
  : Parameters from Mixed Models

- [`model_parameters(`*`<hclust>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.hclust.md)
  : Parameters from Cluster Models (k-means, ...)

- [`model_parameters(`*`<mira>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.mira.md)
  : Parameters from multiply imputed repeated analyses

- [`model_parameters(`*`<lavaan>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.principal.md)
  [`model_parameters(`*`<principal>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.principal.md)
  : Parameters from PCA, FA, CFA, SEM

- [`model_parameters(`*`<data.frame>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.brmsfit.md)
  [`model_parameters(`*`<brmsfit>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.brmsfit.md)
  : Parameters from Bayesian Models

- [`model_parameters(`*`<BFBayesFactor>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.BFBayesFactor.md)
  : Parameters from BayesFactor objects

- [`model_parameters(`*`<rma>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.rma.md)
  : Parameters from Meta-Analysis

- [`model_parameters(`*`<htest>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.htest.md)
  [`model_parameters(`*`<coeftest>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.htest.md)
  : Parameters from hypothesis tests

- [`model_parameters(`*`<glht>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.glht.md)
  : Parameters from Hypothesis Testing

- [`model_parameters(`*`<glimML>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.glimML.md)
  : Parameters from special models

- [`model_parameters(`*`<t1way>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.t1way.md)
  :

  Parameters from robust statistical objects in `WRS2`

- [`model_parameters(`*`<compare.loo>`*`)`](https://easystats.github.io/parameters/reference/model_parameters.compare.loo.md)
  : Bayesian Model Comparison

## Standard Errors, Confidence Intervals, Degrees of Freedom and p-values

- [`standard_error()`](https://easystats.github.io/parameters/reference/standard_error.md)
  : Standard Errors
- [`ci(`*`<default>`*`)`](https://easystats.github.io/parameters/reference/ci.default.md)
  : Confidence Intervals (CI)
- [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
  : p-values
- [`degrees_of_freedom()`](https://easystats.github.io/parameters/reference/degrees_of_freedom.md)
  [`dof()`](https://easystats.github.io/parameters/reference/degrees_of_freedom.md)
  : Degrees of Freedom (DoF)

### Approximation Methods

- [`ci_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md)
  [`dof_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md)
  [`p_value_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md)
  [`se_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md)
  : Kenward-Roger approximation for SEs, CIs and p-values
- [`ci_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md)
  [`dof_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md)
  [`p_value_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md)
  [`se_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md)
  : Satterthwaite approximation for SEs, CIs and p-values
- [`ci_betwithin()`](https://easystats.github.io/parameters/reference/p_value_betwithin.md)
  [`dof_betwithin()`](https://easystats.github.io/parameters/reference/p_value_betwithin.md)
  [`p_value_betwithin()`](https://easystats.github.io/parameters/reference/p_value_betwithin.md)
  : Between-within approximation for SEs, CIs and p-values
- [`ci_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md)
  [`dof_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md)
  [`p_value_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md)
  : "m-l-1" approximation for SEs, CIs and p-values

## Effect Existence and Significance

- [`equivalence_test(`*`<lm>`*`)`](https://easystats.github.io/parameters/reference/equivalence_test.lm.md)
  : Equivalence test
- [`p_calibrate()`](https://easystats.github.io/parameters/reference/p_calibrate.md)
  : Calculate calibrated p-values.
- [`p_direction(`*`<lm>`*`)`](https://easystats.github.io/parameters/reference/p_direction.lm.md)
  : Probability of Direction (pd)
- [`p_function()`](https://easystats.github.io/parameters/reference/p_function.md)
  [`consonance_function()`](https://easystats.github.io/parameters/reference/p_function.md)
  [`confidence_curve()`](https://easystats.github.io/parameters/reference/p_function.md)
  [`format(`*`<parameters_p_function>`*`)`](https://easystats.github.io/parameters/reference/p_function.md)
  [`print(`*`<parameters_p_function>`*`)`](https://easystats.github.io/parameters/reference/p_function.md)
  [`print_html(`*`<parameters_p_function>`*`)`](https://easystats.github.io/parameters/reference/p_function.md)
  : p-value or consonance function
- [`p_significance(`*`<lm>`*`)`](https://easystats.github.io/parameters/reference/p_significance.lm.md)
  : Practical Significance (ps)

## Parameter Sampling

- [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  : Model bootstrapping
- [`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)
  : Parameters bootstrapping
- [`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md)
  : Simulated draws from model coefficients
- [`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md)
  : Simulate Model Parameters

## Feature Reduction

- [`reduce_parameters()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
  [`reduce_data()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
  : Dimensionality reduction (DR) / Features Reduction
- [`select_parameters()`](https://easystats.github.io/parameters/reference/select_parameters.md)
  : Automated selection of model parameters

## Data Reduction

### Cluster Analysis

- [`cluster_analysis()`](https://easystats.github.io/parameters/reference/cluster_analysis.md)
  : Cluster Analysis
- [`cluster_centers()`](https://easystats.github.io/parameters/reference/cluster_centers.md)
  : Find the cluster centers in your data
- [`cluster_discrimination()`](https://easystats.github.io/parameters/reference/cluster_discrimination.md)
  : Compute a linear discriminant analysis on classified cluster groups
- [`cluster_meta()`](https://easystats.github.io/parameters/reference/cluster_meta.md)
  : Metaclustering
- [`cluster_performance()`](https://easystats.github.io/parameters/reference/cluster_performance.md)
  : Performance of clustering models
- [`n_clusters()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  [`n_clusters_elbow()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  [`n_clusters_gap()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  [`n_clusters_silhouette()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  [`n_clusters_dbscan()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  [`n_clusters_hclust()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  : Find number of clusters in your data
- [`predict(`*`<parameters_clusters>`*`)`](https://easystats.github.io/parameters/reference/predict.parameters_clusters.md)
  : Predict method for parameters_clusters objects

### Factors and Principal Components

- [`convert_efa_to_cfa()`](https://easystats.github.io/parameters/reference/convert_efa_to_cfa.md)
  [`efa_to_cfa()`](https://easystats.github.io/parameters/reference/convert_efa_to_cfa.md)
  : Conversion between EFA results and CFA structure
- [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`rotated_data()`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`print_html(`*`<parameters_efa>`*`)`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`predict(`*`<parameters_efa>`*`)`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`print(`*`<parameters_efa>`*`)`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`sort(`*`<parameters_efa>`*`)`](https://easystats.github.io/parameters/reference/principal_components.md)
  [`closest_component()`](https://easystats.github.io/parameters/reference/principal_components.md)
  : Principal Component Analysis (PCA) and Factor Analysis (FA)
- [`factor_scores()`](https://easystats.github.io/parameters/reference/factor_scores.md)
  : Extract factor scores from Factor Analysis (EFA) or Omega
- [`get_scores()`](https://easystats.github.io/parameters/reference/get_scores.md)
  : Get Scores from Principal Component or Factor Analysis (PCA/FA)
- [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  [`n_components()`](https://easystats.github.io/parameters/reference/n_factors.md)
  : Number of components/factors to retain in PCA/FA
- [`reduce_parameters()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
  [`reduce_data()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
  : Dimensionality reduction (DR) / Features Reduction
- [`reshape_loadings()`](https://easystats.github.io/parameters/reference/reshape_loadings.md)
  : Reshape loadings between wide/long formats

## Table and Value Formatting

- [`display(`*`<parameters_model>`*`)`](https://easystats.github.io/parameters/reference/display.parameters_model.md)
  : Print tables in different output formats
- [`format_order()`](https://easystats.github.io/parameters/reference/format_order.md)
  : Order (first, second, ...) formatting
- [`format_parameters()`](https://easystats.github.io/parameters/reference/format_parameters.md)
  : Parameter names formatting
- [`format_p_adjust()`](https://easystats.github.io/parameters/reference/format_p_adjust.md)
  : Format the name of the p-value adjustment methods
- [`format_df_adjust()`](https://easystats.github.io/parameters/reference/format_df_adjust.md)
  : Format the name of the degrees-of-freedom adjustment methods
- [`format(`*`<compare_parameters>`*`)`](https://easystats.github.io/parameters/reference/print.compare_parameters.md)
  [`print(`*`<compare_parameters>`*`)`](https://easystats.github.io/parameters/reference/print.compare_parameters.md)
  [`print_html(`*`<compare_parameters>`*`)`](https://easystats.github.io/parameters/reference/print.compare_parameters.md)
  [`print_md(`*`<compare_parameters>`*`)`](https://easystats.github.io/parameters/reference/print.compare_parameters.md)
  : Print comparisons of model parameters
- [`parameters_type()`](https://easystats.github.io/parameters/reference/parameters_type.md)
  : Type of model parameters
- [`reexports`](https://easystats.github.io/parameters/reference/reexports.md)
  [`equivalence_test`](https://easystats.github.io/parameters/reference/reexports.md)
  [`ci`](https://easystats.github.io/parameters/reference/reexports.md)
  [`n_parameters`](https://easystats.github.io/parameters/reference/reexports.md)
  [`p_direction`](https://easystats.github.io/parameters/reference/reexports.md)
  [`p_significance`](https://easystats.github.io/parameters/reference/reexports.md)
  [`standardize_names`](https://easystats.github.io/parameters/reference/reexports.md)
  [`supported_models`](https://easystats.github.io/parameters/reference/reexports.md)
  [`print_html`](https://easystats.github.io/parameters/reference/reexports.md)
  [`print_md`](https://easystats.github.io/parameters/reference/reexports.md)
  [`display`](https://easystats.github.io/parameters/reference/reexports.md)
  [`describe_distribution`](https://easystats.github.io/parameters/reference/reexports.md)
  [`demean`](https://easystats.github.io/parameters/reference/reexports.md)
  [`rescale_weights`](https://easystats.github.io/parameters/reference/reexports.md)
  [`visualisation_recipe`](https://easystats.github.io/parameters/reference/reexports.md)
  [`kurtosis`](https://easystats.github.io/parameters/reference/reexports.md)
  [`skewness`](https://easystats.github.io/parameters/reference/reexports.md)
  : Objects exported from other packages

## Functions exported from other packages

- [`reexports`](https://easystats.github.io/parameters/reference/reexports.md)
  [`equivalence_test`](https://easystats.github.io/parameters/reference/reexports.md)
  [`ci`](https://easystats.github.io/parameters/reference/reexports.md)
  [`n_parameters`](https://easystats.github.io/parameters/reference/reexports.md)
  [`p_direction`](https://easystats.github.io/parameters/reference/reexports.md)
  [`p_significance`](https://easystats.github.io/parameters/reference/reexports.md)
  [`standardize_names`](https://easystats.github.io/parameters/reference/reexports.md)
  [`supported_models`](https://easystats.github.io/parameters/reference/reexports.md)
  [`print_html`](https://easystats.github.io/parameters/reference/reexports.md)
  [`print_md`](https://easystats.github.io/parameters/reference/reexports.md)
  [`display`](https://easystats.github.io/parameters/reference/reexports.md)
  [`describe_distribution`](https://easystats.github.io/parameters/reference/reexports.md)
  [`demean`](https://easystats.github.io/parameters/reference/reexports.md)
  [`rescale_weights`](https://easystats.github.io/parameters/reference/reexports.md)
  [`visualisation_recipe`](https://easystats.github.io/parameters/reference/reexports.md)
  [`kurtosis`](https://easystats.github.io/parameters/reference/reexports.md)
  [`skewness`](https://easystats.github.io/parameters/reference/reexports.md)
  : Objects exported from other packages

## Global options

- [`parameters-options`](https://easystats.github.io/parameters/reference/parameters-options.md)
  : Global options from the parameters package

## Example Data Sets

- [`qol_cancer`](https://easystats.github.io/parameters/reference/qol_cancer.md)
  : Sample data set
- [`fish`](https://easystats.github.io/parameters/reference/fish.md) :
  Sample data set
