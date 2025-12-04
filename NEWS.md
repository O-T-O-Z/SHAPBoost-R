# SHAPBoost 1.0.1

## Bug Fixes
* Fixed a bug that caused unsupported types in xgboost for SHAPBoostRegressor

# SHAPBoost 1.0.0

## New Features

* Initial release of SHAPBoost package
* Implementation of SHAPBoost algorithm for feature selection using SHAP values
* Support for regression analysis with `SHAPBoostRegressor` class
* Support for survival analysis with `SHAPBoostSurvival` class
* Integration with XGBoost for gradient boosting
* Support for Cox proportional hazards models in survival analysis
* Cross-validation based feature selection
* Collinearity detection and handling
* Configurable metrics (MAE, MSE, R², C-index)

## Dependencies

* Depends on R >= 3.5.0
* Imports: xgboost, SHAPforxgboost, methods, caret, Matrix

## Documentation

* Complete documentation for all exported functions and classes
* Examples for both regression and survival analysis use cases
