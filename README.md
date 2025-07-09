
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SHAPBoost

<!-- badges: start -->

<!-- badges: end -->

SHAPBoost is an R package for the implementation of the SHAPBoost
feature selection algorithm, which is a boosting method that uses SHAP
values for feature ranking and selects in an iterative forward fashion.
It is designed to work with regression and survival analysis.

## Installation

You can install the development version of SHAPBoost from
[GitHub](https://github.com/O-T-O-Z/SHAPBoost-R) with:

``` r
# install.packages("pak")
pak::pak("O-T-O-Z/SHAPBoost-R")
```

## Regression example

For regression tasks, SHAPBoost can be used with various evaluators such
as linear regression or XGBoost (`xgb`). For metrics, it support `mae`
(Mean Absolute Error), `mse` (Mean Squared Error), and `r2` (R-squared
or $R^{2}$).

Below is an example using `eyedata`.

``` r
library(SHAPBoost)
library(flare)

shapboost <- SHAPBoostRegressor$new(
    evaluator = "lr",
    metric = "mae",
    siso_ranking_size = 10,
    verbose = 0,
)

data(eyedata)
X <- as.data.frame(x)
y <- as.data.frame(y)
subset <- shapboost$fit(X, y)
#> 
#>  Iteration: 1 
#> Selected variables:
#>   
#> Best feature(s): 21907 with mae = 0.07519698 
#> --------------------------------------------
#> 
#> Found 51 correlated variables: 2679, 2789, 5892, 6690, 8675, 8835, 9096, 9187, 9303, 10438, 10693, 11711, 12205, 12997, 13092, 13629, 14631, 15224, 15368, 15636, 15850, 16541, 16801, 16984, 17200, 17599, 17645, 21094, 21564, 21791, 21907, 21978, 22938, 23050, 23110, 23618, 23805, 24087, 24245, 24653, 25141, 25403, 25425, 26738, 26809, 26932, 27244, 28891, 28899, 28983, 30116 
#> Best feature(s): 15224 with mae = 0.0736655 
#> --------------------------------------------
#> 
#> Found 68 correlated variables: 2487, 3244, 3732, 6222, 6359, 7069, 9061, 9340, 10144, 10196, 10780, 11024, 11421, 11609, 11719, 11928, 11995, 12081, 12085, 12813, 14903, 15224, 15289, 15752, 15787, 15940, 16014, 16313, 16569, 16964, 16988, 17270, 17436, 17723, 17803, 17816, 17986, 18389, 18405, 19331, 21864, 22029, 22043, 22304, 22423, 22640, 22980, 23041, 23161, 23206, 23288, 24225, 24396, 24565, 24618, 24892, 24901, 25000, 25109, 25367, 26369, 26868, 27354, 28306, 29842, 29984, 30031, 30078 
#> Best feature(s): 15787 with mae = 0.07705071 
#> --------------------------------------------
#> 
#> Found 6 correlated variables: 9972, 15787, 18062, 25852, 25903, 28680 
#> Best feature(s): 28680 with mae = 0.07614963 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 2 
#> Selected variables:
#>  28680 
#> Best feature(s): 10326, 28680 with mae = 0.07115704 
#> --------------------------------------------
#> 
#> Found 3 correlated variables: 10326, 24783, 28964 
#> Best feature(s): 28964, 28680 with mae = 0.07351863 
#> --------------------------------------------
#> 
#> Found 2 correlated variables: 25014, 28964 
#> Best feature(s): 28964, 28680 with mae = 0.06987332 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 3 
#> Selected variables:
#>  28680, 28964 
#> Best feature(s): 21092, 28680, 28964 with mae = 0.06981038 
#> --------------------------------------------
#> 
#> Found 2 correlated variables: 21092, 22731 
#> Best feature(s): 22731, 28680, 28964 with mae = 0.06801388 
#> --------------------------------------------
#> 
#> Found 42 correlated variables: 1377, 1748, 2875, 3375, 7941, 10540, 13858, 13901, 14046, 18283, 21469, 21680, 22277, 22694, 22731, 22869, 22896, 23006, 23404, 23804, 23877, 23942, 24198, 24282, 24353, 24422, 24597, 25055, 25281, 25439, 25443, 26672, 26712, 27408, 28164, 28343, 28738, 29041, 29045, 29566, 29665, 29912 
#> Best feature(s): 24597, 28680, 28964 with mae = 0.06646264 
#> --------------------------------------------
#> 
#> STOP CONDITION: Epsilon value falls below the threshold
#> Resetting weights...
#> 
#>  Iteration: 3 
#> Selected variables:
#>  28680, 28964, 24597 
#> Best feature(s): 21550, 28680, 28964, 24597 with mae = 0.0653044 
#> --------------------------------------------
#> 
#> Found 8 correlated variables: 21550, 22813, 22978, 23348, 24857, 26696, 29773, 29896 
#> Best feature(s): 23348, 28680, 28964, 24597 with mae = 0.06749156 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 4 
#> Selected variables:
#>  28680, 28964, 24597, 23348 
#> Best feature(s): 15863, 28680, 28964, 24597, 23348 with mae = 0.06447365 
#> --------------------------------------------
#> 
#> Found 2 correlated variables: 15863, 27179 
#> Best feature(s): 27179, 28680, 28964, 24597, 23348 with mae = 0.0654549 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 5 
#> Selected variables:
#>  28680, 28964, 24597, 23348, 27179 
#> Best feature(s): 30037, 28680, 28964, 24597, 23348, 27179 with mae = 0.06525289 
#> --------------------------------------------
#> 
#> STOP CONDITION: Epsilon value falls below the threshold
#> No more features to select or stop conditions met.
#> *--------------------------------------------*
#> Selected subset: 28680, 28964, 24597, 23348, 27179 
#> *--------------------------------------------*
```

## Survival example

For survival analysis, SHAPBoost can be used with the `coxph` or `xgb`
evaluator and the `c-index` metric. Please provide the survival data in
a format where the first column is the time to event and the second
column is the event indicator (1 for event, 0 for censored). Moreover,
the `xgb_params` argument can be used to pass additional parameters to
the XGBoost model, such as `objective` and `eval_metric`. Supported
objectives are `survival:cox` and `survival:aft`, with their respective
evaluation metrics `cox-nloglik` and `aft-nloglik`.

An example using the `gbsg` dataset is shown below.

``` r
library(SHAPBoost)
library(survival)

shapboost <- SHAPBoostSurvival$new(
    evaluator = "coxph",
    metric = "c-index",
    verbose = 0,
    xgb_params = list(
        objective = "survival:cox",
        eval_metric = "cox-nloglik"
    )
)
X <- as.data.frame(gbsg[, -c(1, 10, 11)])
y <- as.data.frame(gbsg[, c(10, 11)])

subset <- shapboost$fit(X, y)
#> 
#>  Iteration: 1 
#> Selected variables:
#>   
#> Best feature(s): nodes with c-index = 0.6402908 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 2 
#> Selected variables:
#>  nodes 
#> Best feature(s): pgr, nodes with c-index = 0.6806932 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 3 
#> Selected variables:
#>  nodes, pgr 
#> Best feature(s): hormon, nodes, pgr with c-index = 0.6835486 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 4 
#> Selected variables:
#>  nodes, pgr, hormon 
#> Best feature(s): size, nodes, pgr, hormon with c-index = 0.6834421 
#> --------------------------------------------
#> 
#> 
#>  Iteration: 5 
#> Selected variables:
#>  nodes, pgr, hormon, size 
#> Best feature(s): size, nodes, pgr, hormon, size with c-index = 0.6807224 
#> --------------------------------------------
#> 
#> STOP CONDITION: Repeated variable found
#> Resetting weights...
#> 
#>  Iteration: 4 
#> Selected variables:
#>  nodes, pgr, hormon, size 
#> Best feature(s): nodes, nodes, pgr, hormon, size with c-index = 0.6873254 
#> --------------------------------------------
#> 
#> STOP CONDITION: Repeated variable found
#> No more features to select or stop conditions met.
#> *--------------------------------------------*
#> Selected subset: nodes, pgr, hormon, size 
#> *--------------------------------------------*
```
