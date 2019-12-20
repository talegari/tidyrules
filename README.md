
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyrules

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/talegari/tidyrules.svg?branch=master)](https://travis-ci.org/talegari/tidyrules)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/tidyrules)](https://cran.r-project.org/package=tidyrules)
<!-- badges: end -->

`tidyrules` converts texual rules from models to dataframes with
parseable rules. Supported models are: `C5`, `cubist` and `rpart`.

## Example

``` r
library(tidyrules)
```

``` r
model_c5 = C50::C5.0(Species ~ ., data = iris, rules = TRUE)
summary(model_c5)
#> 
#> Call:
#> C5.0.formula(formula = Species ~ ., data = iris, rules = TRUE)
#> 
#> 
#> C5.0 [Release 2.07 GPL Edition]      Tue Dec 10 14:47:18 2019
#> -------------------------------
#> 
#> Class specified by attribute `outcome'
#> 
#> Read 150 cases (5 attributes) from undefined.data
#> 
#> Rules:
#> 
#> Rule 1: (50, lift 2.9)
#>  Petal.Length <= 1.9
#>  ->  class setosa  [0.981]
#> 
#> Rule 2: (48/1, lift 2.9)
#>  Petal.Length > 1.9
#>  Petal.Length <= 4.9
#>  Petal.Width <= 1.7
#>  ->  class versicolor  [0.960]
#> 
#> Rule 3: (46/1, lift 2.9)
#>  Petal.Width > 1.7
#>  ->  class virginica  [0.958]
#> 
#> Rule 4: (46/2, lift 2.8)
#>  Petal.Length > 4.9
#>  ->  class virginica  [0.938]
#> 
#> Default class: setosa
#> 
#> 
#> Evaluation on training data (150 cases):
#> 
#>          Rules     
#>    ----------------
#>      No      Errors
#> 
#>       4    4( 2.7%)   <<
#> 
#> 
#>     (a)   (b)   (c)    <-classified as
#>    ----  ----  ----
#>      50                (a): class setosa
#>            47     3    (b): class versicolor
#>             1    49    (c): class virginica
#> 
#> 
#>  Attribute usage:
#> 
#>   96.00% Petal.Length
#>   62.67% Petal.Width
#> 
#> 
#> Time: 0.0 secs
```

Tidy the rules:

``` r
pander::pandoc.table(tidyRules(model_c5), split.tables = 120)
#> 
#> ----------------------------------------------------------------------------------------------------
#>  id            LHS               RHS       support   confidence   lift   rule_number   trial_number 
#> ---- ----------------------- ------------ --------- ------------ ------ ------------- --------------
#>  1     Petal.Length <= 1.9      setosa       50        0.9808     2.9         1             1       
#> 
#>  2    Petal.Length > 1.9 &    versicolor     48         0.96      2.9         2             1       
#>       Petal.Length <= 4.9 &                                                                         
#>        Petal.Width <= 1.7                                                                           
#> 
#>  3      Petal.Width > 1.7     virginica      46        0.9583     2.9         3             1       
#> 
#>  4     Petal.Length > 4.9     virginica      46        0.9375     2.8         4             1       
#> ----------------------------------------------------------------------------------------------------
```

## Installation

You can install the released version of tidyrules from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyrules")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("talegari/tidyrules")
```
