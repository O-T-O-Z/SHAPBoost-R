#' @importFrom methods new
#' @importFrom xgboost xgb.DMatrix xgboost
#' @importFrom Matrix Matrix
NULL

#' SHAPBoostRegressor is a reference class for regression feature selection through gradient boosting.
#' 
#' This class extends the SHAPBoostEstimator class and implements methods for initializing, updating weights, scoring, and fitting estimators.
#' 
#' @field evaluator The model that is used to evaluate each additional feature. Choice between "lr" and "xgb".
#' @field metric The metric used for evaluation, such as "mae", "mse", or "r2".
#' @field xgb_params A list of parameters for the XGBoost model.
#' @field number_of_folds The number of folds for cross-validation.
#' @field epsilon A small value to prevent division by zero.
#' @field max_number_of_features The maximum number of features to consider.
#' @field siso_ranking_size The size of the SISO ranking.
#' @field siso_order The order of the SISO ranking.
#' @field reset A boolean indicating whether to reset the model.
#' @field xgb_importance The importance type for XGBoost.
#' @field num_resets The number of resets for the model.
#' @field fold_random_state The random state for folds.
#' @field verbose The verbosity level for logging.
#' @field stratification A boolean indicating whether to use stratification. Only applicable for c-index metric.
#' @field use_shap A boolean indicating whether to use SHAP values.
#' @field collinearity_check A boolean indicating whether to check for collinearity.
#' @field correlation_threshold The threshold for correlation to consider features as collinear.
#' 
#' @examples
#' if (requireNamespace("flare", quietly = TRUE)) {
#'   data("eyedata", package = "flare")
#'   shapboost <- SHAPBoostRegressor$new(
#'     max_number_of_features = 1,
#'     evaluator = "lr",
#'     metric = "mae",
#'     siso_ranking_size = 10,
#'     verbose = 0
#'   )
#'   X <- as.data.frame(x)
#'   y <- as.data.frame(y)
#'   subset <- shapboost$fit(X, y)
#' }
#' 
#' @export SHAPBoostRegressor
#' @exportClass SHAPBoostRegressor
SHAPBoostRegressor <- setRefClass("SHAPBoostRegressor",
    contains = "SHAPBoostEstimator",
    fields = list(),
    methods = list(
        initialize = function(...) {
            callSuper(...)
        },
        init_alpha = function(y) {
            n_samples <- nrow(y)
            alpha_abs <<- matrix(0, nrow = n_samples, ncol = max_number_of_features + 1)
            alpha <<- matrix(0, nrow = n_samples, ncol = max_number_of_features + 1)
            alpha_abs[, 1] <<- rep(1, n_samples)
            alpha[, 1] <<- rep(1, n_samples)
            global_sample_weights <<- rep(1, n_samples)
        },
        update_weights = function(X, y) {
            fit_estimator(X, y, estimator_id = 0)
            X <- as.matrix(X)
            y_pred <- predict(estimators[[1]], X)

            y <- as.numeric(y[[1]])
            y_pred <- as.numeric(y_pred)

            min_val <- quantile(y, probs = 0.01)
            max_val <- quantile(y, probs = 0.99)
            y <- (y - min_val) / (max_val - min_val)

            y_pred <- (y_pred - min_val) / (max_val - min_val)

            errors <- abs(y - y_pred)

            sigmoid <- function(x) {
                2 * (1 / (1 + exp(-x)))
            }

            alpha_abs[, i + 1] <<- sapply(seq_along(errors), function(i) {
                sigmoid(errors[i])
            })
            alpha[, i + 1] <<- alpha_abs[, i + 1] / alpha_abs[, i]

            global_sample_weights <<- global_sample_weights * alpha[, i + 1]

            global_sample_weights <<- global_sample_weights / sum(global_sample_weights) * length(y)
        },
        score = function(preds, y_test) {
            preds_vec <- preds$preds
            y_test_vec <- y_test$y_test
            if (identical(metric, "mae")) {
                mae <- mean(abs(preds_vec - y_test_vec))
                return(mae)
            } else if (identical(metric, "mse")) {
                mse <- mean((preds_vec - y_test_vec)^2)
                return(mse)
            } else if (identical(metric, "r2")) {
                r2 <- 1 - sum((preds_vec - y_test_vec)^2) / sum((y_test_vec - mean(y_test_vec))^2)
                return(r2)
            }
            stop("Invalid metric")
        },
        fit_estimator = function(X, y, sample_weight = NULL, estimator_id = 0) {
            X_mat <- Matrix::Matrix(as.matrix(X), sparse = TRUE)
            y_mat <- Matrix::Matrix(as.matrix(y), sparse = TRUE)

            if (!is.null(sample_weight)) {
                sample_weight <- as.numeric(sample_weight)
            }
            # TODO: add early stopping and hyperparameter tuning
            if (estimator_id == 0) {
                dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_mat, weight = sample_weight)
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    verbose = 0,
                    params = xgb_params,
                )
            } else if (estimator_id == 1 && evaluator == "xgb") {
                dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_mat)
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    verbose = 0,
                    params = xgb_params,
                )
            } else if (estimator_id == 1 && evaluator == "lr") {
                X_df <- as.data.frame(as.matrix(X_mat))
                # remove columns with the same name
                col_names <- colnames(X_df)
                non_duplicated_cols <- seq_along(col_names)
                if (any(duplicated(col_names))) {
                    non_duplicated_cols <- setdiff(seq_along(col_names), which(duplicated(col_names)))
                    X_df <- as.data.frame(X_df[, non_duplicated_cols])
                }
                colnames(X_df) <- col_names[non_duplicated_cols]

                estimators[[estimator_id + 1]] <<- lm(
                    formula = as.formula(paste("y ~ .")),
                    data = cbind(X_df, y),
                )
            } else {
                stop("Evaluator", evaluator, "is not supported! Please choose one of ['lr', 'xgb'].")
            }
            return(estimators[[estimator_id + 1]])
        }
    )
)
