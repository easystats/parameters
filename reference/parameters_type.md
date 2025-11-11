# Type of model parameters

In a regression model, the parameters do not all have the meaning. For
instance, the intercept has to be interpreted as theoretical outcome
value under some conditions (when predictors are set to 0), whereas
other coefficients are to be interpreted as amounts of change. Others,
such as interactions, represent changes in another of the parameter. The
`parameters_type` function attempts to retrieve information and meaning
of parameters. It outputs a dataframe of information for each
parameters, such as the `Type` (whether the parameter corresponds to a
factor or a numeric predictor, or whether it is a (regular) interaction
or a nested one), the `Link` (whether the parameter can be interpreted
as a mean value, the slope of an association or a difference between two
levels) and, in the case of interactions, which other parameters is
impacted by which parameter.

## Usage

``` r
parameters_type(model, ...)
```

## Arguments

- model:

  A statistical model.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame.

## Examples

``` r
library(parameters)

model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
parameters_type(model)
#>           Parameter      Type        Link              Term     Variable
#> 1       (Intercept) intercept        Mean       (Intercept)         <NA>
#> 2      Petal.Length   numeric Association      Petal.Length Petal.Length
#> 3 Speciesversicolor    factor  Difference Speciesversicolor      Species
#> 4  Speciesvirginica    factor  Difference  Speciesvirginica      Species
#>        Level Secondary_Parameter Secondary_Type Secondary_Link Secondary_Term
#> 1       <NA>                <NA>             NA             NA             NA
#> 2       <NA>                <NA>             NA             NA             NA
#> 3 versicolor                <NA>             NA             NA             NA
#> 4  virginica                <NA>             NA             NA             NA
#>   Secondary_Variable Secondary_Level Tertiary_Parameter
#> 1                 NA              NA                 NA
#> 2                 NA              NA                 NA
#> 3                 NA              NA                 NA
#> 4                 NA              NA                 NA

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
parameters_type(model)
#>               Parameter      Type        Link                  Term    Variable
#> 1           (Intercept) intercept        Mean           (Intercept)        <NA>
#> 2     Speciesversicolor    factor  Difference     Speciesversicolor     Species
#> 3      Speciesvirginica    factor  Difference      Speciesvirginica     Species
#> 4 poly(Sepal.Width, 2)1      poly Association poly(Sepal.Width, 2)1 Sepal.Width
#> 5 poly(Sepal.Width, 2)2      poly Association poly(Sepal.Width, 2)2 Sepal.Width
#>        Level Secondary_Parameter Secondary_Type Secondary_Link Secondary_Term
#> 1       <NA>                <NA>             NA             NA             NA
#> 2 versicolor                <NA>             NA             NA             NA
#> 3  virginica                <NA>             NA             NA             NA
#> 4          1                <NA>             NA             NA             NA
#> 5          2                <NA>             NA             NA             NA
#>   Secondary_Variable Secondary_Level Tertiary_Parameter
#> 1                 NA              NA                 NA
#> 2                 NA              NA                 NA
#> 3                 NA              NA                 NA
#> 4                 NA              NA                 NA
#> 5                 NA              NA                 NA

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
parameters_type(model)
#>                           Parameter      Type        Link                  Term
#> 1                       (Intercept) intercept        Mean           (Intercept)
#> 2                 Speciesversicolor    factor  Difference     Speciesversicolor
#> 3                  Speciesvirginica    factor  Difference      Speciesvirginica
#> 4 poly(Sepal.Width, 2, raw = TRUE)1  poly_raw Association poly(Sepal.Width, 2)1
#> 5 poly(Sepal.Width, 2, raw = TRUE)2  poly_raw Association poly(Sepal.Width, 2)2
#>      Variable      Level Secondary_Parameter Secondary_Type Secondary_Link
#> 1        <NA>       <NA>                <NA>             NA             NA
#> 2     Species versicolor                <NA>             NA             NA
#> 3     Species  virginica                <NA>             NA             NA
#> 4 Sepal.Width          1                <NA>             NA             NA
#> 5 Sepal.Width          2                <NA>             NA             NA
#>   Secondary_Term Secondary_Variable Secondary_Level Tertiary_Parameter
#> 1             NA                 NA              NA                 NA
#> 2             NA                 NA              NA                 NA
#> 3             NA                 NA              NA                 NA
#> 4             NA                 NA              NA                 NA
#> 5             NA                 NA              NA                 NA

# Interactions
model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
parameters_type(model)
#>                       Parameter        Type        Link              Term
#> 1                   (Intercept)   intercept        Mean       (Intercept)
#> 2                   Sepal.Width     numeric Association       Sepal.Width
#> 3             Speciesversicolor      factor  Difference Speciesversicolor
#> 4              Speciesvirginica      factor  Difference  Speciesvirginica
#> 5 Sepal.Width:Speciesversicolor interaction  Difference Speciesversicolor
#> 6  Sepal.Width:Speciesvirginica interaction  Difference  Speciesvirginica
#>      Variable      Level Secondary_Parameter Secondary_Type Secondary_Link
#> 1        <NA>       <NA>                <NA>           <NA>           <NA>
#> 2 Sepal.Width       <NA>                <NA>           <NA>           <NA>
#> 3     Species versicolor                <NA>           <NA>           <NA>
#> 4     Species  virginica                <NA>           <NA>           <NA>
#> 5     Species versicolor         Sepal.Width        numeric    Association
#> 6     Species  virginica         Sepal.Width        numeric    Association
#>   Secondary_Term Secondary_Variable Secondary_Level Tertiary_Parameter
#> 1           <NA>               <NA>            <NA>               <NA>
#> 2           <NA>               <NA>            <NA>               <NA>
#> 3           <NA>               <NA>            <NA>               <NA>
#> 4           <NA>               <NA>            <NA>               <NA>
#> 5    Sepal.Width        Sepal.Width            <NA>               <NA>
#> 6    Sepal.Width        Sepal.Width            <NA>               <NA>

model <- lm(Sepal.Length ~ Sepal.Width * Species * Petal.Length, data = iris)
parameters_type(model)
#>                                     Parameter        Type        Link
#> 1                                 (Intercept)   intercept        Mean
#> 2                                 Sepal.Width     numeric Association
#> 3                           Speciesversicolor      factor  Difference
#> 4                            Speciesvirginica      factor  Difference
#> 5                                Petal.Length     numeric Association
#> 6               Sepal.Width:Speciesversicolor interaction  Difference
#> 7                Sepal.Width:Speciesvirginica interaction  Difference
#> 8                    Sepal.Width:Petal.Length interaction Association
#> 9              Speciesversicolor:Petal.Length interaction  Difference
#> 10              Speciesvirginica:Petal.Length interaction  Difference
#> 11 Sepal.Width:Speciesversicolor:Petal.Length interaction Association
#> 12  Sepal.Width:Speciesvirginica:Petal.Length interaction Association
#>                 Term     Variable      Level           Secondary_Parameter
#> 1        (Intercept)         <NA>       <NA>                          <NA>
#> 2        Sepal.Width  Sepal.Width       <NA>                          <NA>
#> 3  Speciesversicolor      Species versicolor                          <NA>
#> 4   Speciesvirginica      Species  virginica                          <NA>
#> 5       Petal.Length Petal.Length       <NA>                          <NA>
#> 6  Speciesversicolor      Species versicolor                   Sepal.Width
#> 7   Speciesvirginica      Species  virginica                   Sepal.Width
#> 8       Petal.Length Petal.Length       <NA>                   Sepal.Width
#> 9       Petal.Length Petal.Length       <NA>             Speciesversicolor
#> 10      Petal.Length Petal.Length       <NA>              Speciesvirginica
#> 11      Petal.Length Petal.Length       <NA> Sepal.Width:Speciesversicolor
#> 12      Petal.Length Petal.Length       <NA>  Sepal.Width:Speciesvirginica
#>    Secondary_Type Secondary_Link    Secondary_Term Secondary_Variable
#> 1            <NA>           <NA>              <NA>               <NA>
#> 2            <NA>           <NA>              <NA>               <NA>
#> 3            <NA>           <NA>              <NA>               <NA>
#> 4            <NA>           <NA>              <NA>               <NA>
#> 5            <NA>           <NA>              <NA>               <NA>
#> 6         numeric    Association       Sepal.Width        Sepal.Width
#> 7         numeric    Association       Sepal.Width        Sepal.Width
#> 8         numeric    Association       Sepal.Width        Sepal.Width
#> 9          factor     Difference Speciesversicolor            Species
#> 10         factor     Difference  Speciesvirginica            Species
#> 11    interaction     Difference Speciesversicolor            Species
#> 12    interaction     Difference  Speciesvirginica            Species
#>    Secondary_Level Tertiary_Parameter
#> 1             <NA>               <NA>
#> 2             <NA>               <NA>
#> 3             <NA>               <NA>
#> 4             <NA>               <NA>
#> 5             <NA>               <NA>
#> 6             <NA>               <NA>
#> 7             <NA>               <NA>
#> 8             <NA>               <NA>
#> 9       versicolor               <NA>
#> 10       virginica               <NA>
#> 11      versicolor        Sepal.Width
#> 12       virginica        Sepal.Width

model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
parameters_type(model)
#>                       Parameter        Type        Link              Term
#> 1                   (Intercept)   intercept        Mean       (Intercept)
#> 2             Speciesversicolor      factor  Difference Speciesversicolor
#> 3              Speciesvirginica      factor  Difference  Speciesvirginica
#> 4                   Sepal.Width     numeric Association       Sepal.Width
#> 5 Speciesversicolor:Sepal.Width interaction  Difference       Sepal.Width
#> 6  Speciesvirginica:Sepal.Width interaction  Difference       Sepal.Width
#>      Variable      Level Secondary_Parameter Secondary_Type Secondary_Link
#> 1        <NA>       <NA>                <NA>           <NA>           <NA>
#> 2     Species versicolor                <NA>           <NA>           <NA>
#> 3     Species  virginica                <NA>           <NA>           <NA>
#> 4 Sepal.Width       <NA>                <NA>           <NA>           <NA>
#> 5 Sepal.Width       <NA>   Speciesversicolor         factor     Difference
#> 6 Sepal.Width       <NA>    Speciesvirginica         factor     Difference
#>      Secondary_Term Secondary_Variable Secondary_Level Tertiary_Parameter
#> 1              <NA>               <NA>            <NA>               <NA>
#> 2              <NA>               <NA>            <NA>               <NA>
#> 3              <NA>               <NA>            <NA>               <NA>
#> 4              <NA>               <NA>            <NA>               <NA>
#> 5 Speciesversicolor            Species      versicolor               <NA>
#> 6  Speciesvirginica            Species       virginica               <NA>

model <- lm(Sepal.Length ~ Species / Sepal.Width, data = iris)
parameters_type(model)
#>                       Parameter      Type       Link              Term
#> 1                   (Intercept) intercept       Mean       (Intercept)
#> 2             Speciesversicolor    factor Difference Speciesversicolor
#> 3              Speciesvirginica    factor Difference  Speciesvirginica
#> 4     Speciessetosa:Sepal.Width    nested Difference       Sepal.Width
#> 5 Speciesversicolor:Sepal.Width    nested Difference       Sepal.Width
#> 6  Speciesvirginica:Sepal.Width    nested Difference       Sepal.Width
#>      Variable      Level Secondary_Parameter Secondary_Type Secondary_Link
#> 1        <NA>       <NA>                <NA>           <NA>           <NA>
#> 2     Species versicolor                <NA>           <NA>           <NA>
#> 3     Species  virginica                <NA>           <NA>           <NA>
#> 4 Sepal.Width       <NA>       Speciessetosa         factor     Difference
#> 5 Sepal.Width       <NA>   Speciesversicolor         factor     Difference
#> 6 Sepal.Width       <NA>    Speciesvirginica         factor     Difference
#>      Secondary_Term Secondary_Variable Secondary_Level Tertiary_Parameter
#> 1              <NA>               <NA>            <NA>               <NA>
#> 2              <NA>               <NA>            <NA>               <NA>
#> 3              <NA>               <NA>            <NA>               <NA>
#> 4     Speciessetosa            Species          setosa               <NA>
#> 5 Speciesversicolor            Species      versicolor               <NA>
#> 6  Speciesvirginica            Species       virginica               <NA>


# Complex interactions
data <- iris
data$fac2 <- ifelse(data$Sepal.Width > mean(data$Sepal.Width), "A", "B")
model <- lm(Sepal.Length ~ Species / fac2 / Petal.Length, data = data)
parameters_type(model)
#>                               Parameter      Type        Link              Term
#> 1                           (Intercept) intercept        Mean       (Intercept)
#> 2                     Speciesversicolor    factor  Difference Speciesversicolor
#> 3                      Speciesvirginica    factor  Difference  Speciesvirginica
#> 4                   Speciessetosa:fac2B    nested  Difference             fac2B
#> 5               Speciesversicolor:fac2B    nested  Difference             fac2B
#> 6                Speciesvirginica:fac2B    nested  Difference             fac2B
#> 7      Speciessetosa:fac2A:Petal.Length    nested Association      Petal.Length
#> 8  Speciesversicolor:fac2A:Petal.Length    nested Association      Petal.Length
#> 9   Speciesvirginica:fac2A:Petal.Length    nested Association      Petal.Length
#> 10     Speciessetosa:fac2B:Petal.Length    nested Association      Petal.Length
#> 11 Speciesversicolor:fac2B:Petal.Length    nested Association      Petal.Length
#> 12  Speciesvirginica:fac2B:Petal.Length    nested Association      Petal.Length
#>        Variable      Level     Secondary_Parameter Secondary_Type
#> 1          <NA>       <NA>                    <NA>           <NA>
#> 2       Species versicolor                    <NA>           <NA>
#> 3       Species  virginica                    <NA>           <NA>
#> 4          fac2          B           Speciessetosa         factor
#> 5          fac2          B       Speciesversicolor         factor
#> 6          fac2          B        Speciesvirginica         factor
#> 7  Petal.Length       <NA>     Speciessetosa:fac2A    interaction
#> 8  Petal.Length       <NA> Speciesversicolor:fac2A    interaction
#> 9  Petal.Length       <NA>  Speciesvirginica:fac2A    interaction
#> 10 Petal.Length       <NA>     Speciessetosa:fac2B         nested
#> 11 Petal.Length       <NA> Speciesversicolor:fac2B         nested
#> 12 Petal.Length       <NA>  Speciesvirginica:fac2B         nested
#>    Secondary_Link    Secondary_Term Secondary_Variable Secondary_Level
#> 1            <NA>              <NA>               <NA>            <NA>
#> 2            <NA>              <NA>               <NA>            <NA>
#> 3            <NA>              <NA>               <NA>            <NA>
#> 4      Difference     Speciessetosa            Species          setosa
#> 5      Difference Speciesversicolor            Species      versicolor
#> 6      Difference  Speciesvirginica            Species       virginica
#> 7      Difference             fac2A               fac2               A
#> 8      Difference             fac2A               fac2               A
#> 9      Difference             fac2A               fac2               A
#> 10     Difference             fac2B               fac2               B
#> 11     Difference             fac2B               fac2               B
#> 12     Difference             fac2B               fac2               B
#>    Tertiary_Parameter
#> 1                <NA>
#> 2                <NA>
#> 3                <NA>
#> 4                <NA>
#> 5                <NA>
#> 6                <NA>
#> 7       Speciessetosa
#> 8   Speciesversicolor
#> 9    Speciesvirginica
#> 10      Speciessetosa
#> 11  Speciesversicolor
#> 12   Speciesvirginica

model <- lm(Sepal.Length ~ Species / fac2 * Petal.Length, data = data)
parameters_type(model)
#>                               Parameter        Type        Link
#> 1                           (Intercept)   intercept        Mean
#> 2                     Speciesversicolor      factor  Difference
#> 3                      Speciesvirginica      factor  Difference
#> 4                          Petal.Length     numeric Association
#> 5                   Speciessetosa:fac2B      nested  Difference
#> 6               Speciesversicolor:fac2B      nested  Difference
#> 7                Speciesvirginica:fac2B      nested  Difference
#> 8        Speciesversicolor:Petal.Length interaction  Difference
#> 9         Speciesvirginica:Petal.Length interaction  Difference
#> 10     Speciessetosa:fac2B:Petal.Length      simple Association
#> 11 Speciesversicolor:fac2B:Petal.Length      simple Association
#> 12  Speciesvirginica:fac2B:Petal.Length      simple Association
#>                 Term     Variable      Level     Secondary_Parameter
#> 1        (Intercept)         <NA>       <NA>                    <NA>
#> 2  Speciesversicolor      Species versicolor                    <NA>
#> 3   Speciesvirginica      Species  virginica                    <NA>
#> 4       Petal.Length Petal.Length       <NA>                    <NA>
#> 5              fac2B         fac2          B           Speciessetosa
#> 6              fac2B         fac2          B       Speciesversicolor
#> 7              fac2B         fac2          B        Speciesvirginica
#> 8       Petal.Length Petal.Length       <NA>       Speciesversicolor
#> 9       Petal.Length Petal.Length       <NA>        Speciesvirginica
#> 10      Petal.Length Petal.Length       <NA>     Speciessetosa:fac2B
#> 11      Petal.Length Petal.Length       <NA> Speciesversicolor:fac2B
#> 12      Petal.Length Petal.Length       <NA>  Speciesvirginica:fac2B
#>    Secondary_Type Secondary_Link    Secondary_Term Secondary_Variable
#> 1            <NA>           <NA>              <NA>               <NA>
#> 2            <NA>           <NA>              <NA>               <NA>
#> 3            <NA>           <NA>              <NA>               <NA>
#> 4            <NA>           <NA>              <NA>               <NA>
#> 5          factor     Difference     Speciessetosa            Species
#> 6          factor     Difference Speciesversicolor            Species
#> 7          factor     Difference  Speciesvirginica            Species
#> 8          factor     Difference Speciesversicolor            Species
#> 9          factor     Difference  Speciesvirginica            Species
#> 10         nested     Difference             fac2B               fac2
#> 11         nested     Difference             fac2B               fac2
#> 12         nested     Difference             fac2B               fac2
#>    Secondary_Level Tertiary_Parameter
#> 1             <NA>               <NA>
#> 2             <NA>               <NA>
#> 3             <NA>               <NA>
#> 4             <NA>               <NA>
#> 5           setosa               <NA>
#> 6       versicolor               <NA>
#> 7        virginica               <NA>
#> 8       versicolor               <NA>
#> 9        virginica               <NA>
#> 10               B      Speciessetosa
#> 11               B  Speciesversicolor
#> 12               B   Speciesvirginica
```
