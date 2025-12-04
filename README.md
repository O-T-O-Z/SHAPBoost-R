
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SHAPBoost

<!-- badges: start -->
[![](https://cranlogs.r-pkg.org/badges/SHAPBoost)](https://cran.r-project.org/package=SHAPBoost)
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
data(eyedata)

shapboost <- SHAPBoostRegressor$new(
    evaluator = "lr",
    metric = "mae",
    siso_ranking_size = 10,
    verbose = 0,
)

X <- as.data.frame(x)
y <- as.data.frame(y)
subset <- shapboost$fit(X, y)
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
```
