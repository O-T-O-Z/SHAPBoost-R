#' @importFrom methods new
#' @importFrom xgboost xgb.DMatrix xgboost
#' @importFrom Matrix Matrix
NULL

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
            alpha_abs <<- matrix(0, nrow = n_samples, ncol = max_number_of_features)
            alpha <<- matrix(0, nrow = n_samples, ncol = max_number_of_features)
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
            X <- Matrix::Matrix(as.matrix(X), sparse = TRUE)
            y <- Matrix::Matrix(as.matrix(y), sparse = TRUE)
            # TODO: add early stopping and hyperparameter tuning
            if (estimator_id == 0) {
                dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = sample_weight)
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    verbose = verbose,
                )
            } else {
                dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    verbose = verbose
                )
            }
            return(estimators[[estimator_id + 1]])
        }
    )
)
