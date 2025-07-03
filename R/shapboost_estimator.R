#' @importFrom methods new
#' @importFrom Matrix Matrix
#' @importFrom caret createFolds
#' @importFrom SHAPforxgboost shap.values
NULL

#' SHAPBoostEstimator Class
#' 
#' This class implements the SHAPBoost algorithm for feature selection.
#' It is designed to be extended by specific implementations such as SHAPBoostRegressor and
#' SHAPBoostSurvival. Any new method should implement the abstract methods defined in this class.
#'
#' @field evaluator The model that is used to evaluate each additional feature.
#' @field metric A character string representing the evaluation metric.
#' @field number_of_folds The number of folds for cross-validation.
#' @field epsilon A small value to determine convergence.
#' @field max_number_of_features The maximum number of features to select.
#' @field siso_ranking_size The number of features to consider in the SISO ranking.
#' @field siso_order The order of combinations to consider in SISO.
#' @field reset A logical indicating whether to reset the weights.
#' @field num_resets The number of resets allowed.
#' @field fold_random_state The random state for reproducibility in cross-validation.
#' @field verbose The verbosity level of the output.
#' @field stratification A logical indicating whether to use stratified sampling. Only applicable for c-index metric.
#' @field collinearity_check A logical indicating whether to check for collinearity.
#' @field correlation_threshold The threshold for correlation to consider features as collinear.
#' 
#' @export SHAPBoostEstimator
#' @exportClass SHAPBoostEstimator
SHAPBoostEstimator <- setRefClass(
    "SHAPBoostEstimator",
    fields = list(
        evaluator = "character",
        metric = "character",
        number_of_folds = "numeric",
        epsilon = "numeric",
        max_number_of_features = "numeric",
        siso_ranking_size = "numeric",
        siso_order = "numeric",
        reset = "logical",
        num_resets = "numeric",
        fold_random_state = "numeric",
        verbose = "numeric",
        stratification = "logical",
        collinearity_check = "logical",
        correlation_threshold = "numeric",
        estimators = "list",
        all_selected_variables = "list",
        selected_subset = "list",
        stop_conditions = "list",
        i = "numeric",
        reset_count = "numeric",
        global_sample_weights = "numeric",
        alpha_abs = "array",
        alpha = "array",
        metrics_miso = "list",
        collinear_features = "list"
    ),
    methods = list(
        initialize = function(evaluator,
                              metric,
                              number_of_folds = 5,
                              epsilon = 1e-3,
                              max_number_of_features = 100,
                              siso_ranking_size = 100,
                              siso_order = 1,
                              reset = TRUE,
                              num_resets = 1,
                              fold_random_state = 275,
                              verbose = 0,
                              stratification = FALSE,
                              collinearity_check = TRUE,
                              correlation_threshold = 0.7) {
            evaluator <<- evaluator
            estimators <<- estimators
            metric <<- metric
            number_of_folds <<- number_of_folds
            epsilon <<- epsilon
            max_number_of_features <<- max_number_of_features
            siso_ranking_size <<- siso_ranking_size
            siso_order <<- siso_order
            reset <<- reset
            num_resets <<- num_resets
            fold_random_state <<- fold_random_state
            verbose <<- verbose
            stratification <<- stratification
            collinearity_check <<- collinearity_check
            correlation_threshold <<- correlation_threshold
            global_sample_weights <<- numeric(0)
            all_selected_variables <<- list()
            selected_subset <<- list()
            collinear_features <<- list()
            stop_conditions <<- list(
                stop_epsilon = 10e6,
                repeated_variable = FALSE,
                reset_count = 1,
                reset_allowed = TRUE
            )
            i <<- 1
            metrics_miso <<- list()
            set.seed(fold_random_state)
        },
        score = function(preds, y_test) {
            stop("Abstract method: score() must be implemented by child classes")
        },
        init_alpha = function(Y) {
            stop("Abstract method: init_alpha() must be implemented by child classes")
        },
        update_weights = function(X, y, global_sample_weights, metric_miso) {
            stop("Abstract method: update_weights() must be implemented by child classes")
        },
        fit = function(X, Y, feature_names = NULL, custom_global_sample_weights = NULL) {
            if (!is.data.frame(X) || !is.data.frame(Y)) {
                stop("X and Y must be data frames.")
            }
            if (!is.null(custom_global_sample_weights)) {
                global_sample_weights <<- custom_global_sample_weights
            }

            global_sample_weights <<- rep(1, nrow(X))
            i <<- 1
            stop_conditions$stop_epsilon <<- 10e6
            stop_conditions$repeated_variable <<- FALSE
            stop_conditions$reset_count <<- 0

            init_alpha(Y)

            while (
                stop_conditions$stop_epsilon > epsilon &&
                    i <= max_number_of_features &&
                    stop_conditions$repeated_variable == FALSE
            ) {
                cat("\n", "Iteration:", i, "\n")
                if (stop_conditions$reset_count >= num_resets) {
                    stop_conditions$reset_count <<- 0
                    stop_conditions$reset_allowed <<- FALSE
                }
                cat("Selected variables:\n", paste(all_selected_variables, collapse = ", "), "\n")

                selected_variable <- siso(X, y)
                if (length(selected_variable) == 0) {
                    selected_subset <<- all_selected_variables
                    break
                }

                new_variables <- setdiff(selected_variable, all_selected_variables)
                if (length(new_variables) == 0) {
                    stop_conditions$repeated_variable <<- TRUE
                    check_stop_conditions(y)
                    next
                }
                all_selected_variables <<- c(all_selected_variables, new_variables)
                selected_vars <- unlist(all_selected_variables)
                miso(X[selected_vars], y)
                update_weights(X[selected_vars], y)
                if (length(metrics_miso) > 1) {
                    stop_conditions$stop_epsilon <<- abs(unlist(metrics_miso[length(metrics_miso)]) - unlist(metrics_miso[length(metrics_miso) - 1]))
                }
                i <<- i + 1
                check_stop_conditions(y)
            }

            selected_subset <<- selected_subset[!duplicated(selected_subset)]
            cat("Selected subset:", paste(selected_subset, collapse = ", "), "\n")
            return(selected_subset)
        },
        check_stop_conditions = function(y) {
            # Condition 1 -> Maximum number of features reached
            if (i >= max_number_of_features) {
                cat("Maximum number of features reached\n")
                selected_subset <<- all_selected_variables
            }
            # Condition 2 -> epsilon value falls below the threshold.
            if (stop_conditions$stop_epsilon <= epsilon) {
                cat("Epsilon value falls below the threshold\n")
                selected_subset <<- all_selected_variables[-length(all_selected_variables)]
                metrics_miso <<- metrics_miso[-length(metrics_miso)]
                if (stop_conditions$reset_allowed == TRUE) {
                    stop_conditions$stop_epsilon <<- epsilon + 1
                    reset_weights(y)
                }
            }
            # Condition 3 -> a specific feature has been already selected previously.
            if (stop_conditions$repeated_variable == TRUE) {
                cat("Repeated variable found\n")
                selected_subset <<- all_selected_variables
                if (stop_conditions$reset_allowed == TRUE) {
                    stop_conditions$repeated_variable <<- FALSE
                    reset_weights(y)
                }
            }
        },
        reset_weights = function(y) {
            cat("Resetting weights\n")
            if (stop_conditions$reset_count < num_resets) {
                global_sample_weights <<- rep(1, nrow(X))
            } else {
                global_sample_weights <<- runif(nrow(X))
                global_sample_weights <<- global_sample_weights / sum(global_sample_weights) * length(y)
            }
            stop_conditions$reset_count <<- stop_conditions$reset_count + 1
            i <<- i - 1
        },
        select_best_siso = function(X, y, combs) {
            if (identical(metric, "mae") || identical(metric, "mse") || identical(metric, "logloss")) {
                best_metric <- Inf
            } else {
                best_metric <- 0
            }

            for (comb_index in seq_along(combs)) {
                comb <- combs[[comb_index]]
                X_subset <- X[, comb, drop = FALSE]
                X_subset <- cbind(X_subset, X[, unlist(all_selected_variables), drop = FALSE])
                if (evaluator == "xgb") {
                    X_subset <- Matrix::Matrix(as.matrix(X_subset), sparse = TRUE)
                } else {
                    X_subset <- as.data.frame(X_subset)
                }
                if (stratification && metric == "c-index") {
                    strat <- as.integer(y[, 1] == y[, 2])
                } else {
                    strat <- seq_len(nrow(X_subset))
                }
                folds <- caret::createFolds(y = strat, k = number_of_folds, list = TRUE, returnTrain = FALSE)
                metrics <- numeric(number_of_folds)

                for (i in seq_along(folds)) {
                    # Split data
                    test_indices <- folds[[i]]
                    X_train <- X_subset[-test_indices, , drop = FALSE]
                    X_test <- X_subset[test_indices, , drop = FALSE]
                    y_train <- y[-test_indices, , drop = FALSE]
                    y_test <- y[test_indices, , drop = FALSE]
                    fit_estimator(X_train, y_train, estimator_id = 1)

                    preds <- as.data.frame(predict(estimators[[2]], X_test))
                    colnames(preds) <- "preds"
                    y_test <- as.data.frame(as.matrix(y_test))
                    if (length(y_test) == 1) {
                        colnames(y_test) <- c("y_test")
                    } else {
                        colnames(y_test) <- c("y_test", "y_test_upper_bound")
                    }
                    metrics[i] <- score(preds, y_test)
                }
                # Calculate mean metric
                mean_metric <- mean(metrics)
                if (verbose > 0) {
                    cat("SISO (", comb_index, "/", length(combs), ") [", paste(colnames(X_subset), collapse = ", "), "]:", metric, "=", mean_metric, "\n")
                }
                if (identical(metric, "mae") || identical(metric, "mse") || identical(metric, "logloss")) {
                    if (mean_metric < best_metric) {
                        best_metric <- mean_metric
                        best_comb <- comb
                    }
                } else if (mean_metric > best_metric) {
                    best_metric <- mean_metric
                    best_comb <- comb
                }
            }

            if (verbose > 0) {
                cat("Best combination:", paste(c(best_comb, all_selected_variables), collapse = ", "), "\n")
            }
            return(best_comb)
        },
        siso = function(X, y) {
            if (length(collinear_features) > 0) {
                if (verbose > 0) {
                    cat("Removing", length(collinear_features), "collinear features.\n")
                }
                X <- as.matrix(X)
                for (col in collinear_features) {
                    if (col %in% colnames(X)) {
                        X[, col] <- 0
                    }
                }
            }
            ranking <- rank_features(X, y)

            if (verbose > 0) {
                cat("Ranking of features:\n")
                for (j in seq_along(ranking$Feature)) {
                    cat(ranking$Feature[j], ": ", ranking$Importance[j], "\n", sep = "")
                }
            }
            ranking <- ranking$Feature
            if (length(ranking) == 0) {
                return()
            }
            combs <- lapply(1:siso_order, function(i) {
                combn(ranking, i, simplify = FALSE)
            })
            combs <- unlist(combs, recursive = FALSE)
            selected_variable <- select_best_siso(X, y, combs)
            if (collinearity_check) {
                correlated_vars <- correlation_check(X, selected_variable)
                while (length(correlated_vars) > 1) {
                    cat("Found", length(correlated_vars), "correlated variables:", paste(correlated_vars, collapse = ", "), "\n")
                    selected_variable <- select_best_siso(X, y, correlated_vars)
                    highly_correlated_vars <- correlated_vars[!correlated_vars %in% selected_variable]
                    collinear_features <<- c(collinear_features, highly_correlated_vars)
                    correlated_vars <- correlation_check(X, selected_variable)
                    correlated_vars <- setdiff(correlated_vars, unlist(collinear_features))
                }
                cat("No correlated variables found.\n")
            }
            return(selected_variable)
        },
        miso = function(X, y) {
            if (evaluator == "xgb") {
                X <- Matrix::Matrix(as.matrix(X), sparse = TRUE)
                y <- Matrix::Matrix(as.matrix(y), sparse = TRUE)
            } else {
                X <- as.data.frame(X)
                y <- as.data.frame(y)
            }

            if (stratification && metric == "c-index") {
                strat <- as.integer(y[, 1] == y[, 2])
            } else {
                strat <- seq_len(nrow(X))
            }
            folds <- caret::createFolds(y = seq_len(nrow(X)), k = number_of_folds, list = TRUE, returnTrain = FALSE)
            metrics <- numeric(number_of_folds)

            for (i in seq_along(folds)) {
                test_indices <- folds[[i]]
                X_train <- X[-test_indices, , drop = FALSE]
                X_test <- X[test_indices, , drop = FALSE]
                y_train <- y[-test_indices, , drop = FALSE]
                y_test <- y[test_indices, , drop = FALSE]

                fit_estimator(X_train, y_train, estimator_id = 1)

                preds <- as.data.frame(predict(estimators[[2]], X_test))
                colnames(preds) <- "preds"
                y_test <- as.data.frame(as.matrix(y_test))
                if (length(y_test) == 1) {
                    colnames(y_test) <- c("y_test")
                } else {
                    colnames(y_test) <- c("y_test", "y_test_upper_bound")
                }

                metrics[i] <- score(preds, y_test)
            }
            mean_metric <- mean(metrics)

            if (verbose > 0) {
                cat("MISO Mean metric:", metric, ":", mean_metric, "\n")
            }
            metrics_miso <<- c(metrics_miso, mean_metric)
        },
        rank_features = function(X, y) {
            fit_estimator(X, y, sample_weight = global_sample_weights, estimator_id = 0)
            X_matrix <- Matrix::Matrix(as.matrix(X), sparse = TRUE)
            shap_values <- SHAPforxgboost::shap.values(estimators[[1]], X_train = X_matrix)
            feature_importance <- data.frame(
                Feature = names(shap_values$mean_shap_score),
                Importance = shap_values$mean_shap_score,
                row.names = NULL
            )
            feature_importance <- feature_importance[order(-feature_importance$Importance), ]
            feature_importance <- feature_importance[feature_importance$Importance != 0, ]
            if (length(feature_importance) == 0) {
                cat("No features with non-zero importance found.\n")
                return()
            }

            # get siso_ranking_size features
            if (nrow(feature_importance) > siso_ranking_size) {
                feature_importance <- feature_importance[1:siso_ranking_size, ]
            }
            return(feature_importance)
        },
        correlation_check = function(X, selected_variable) {
            X <- as.matrix(X)
            target <- X[, selected_variable]

            # Calculate correlations only between target and all other features
            non_constant_cols <- which(apply(X, 2, function(x) {
                !all(x == 0) && sd(x, na.rm = TRUE) > .Machine$double.eps  # Check if non-zero & non-constant
            }))

            correlations <- rep(NA, ncol(X))  # Initialize with NA
            if (length(non_constant_cols) > 0) {
                sub_X <- X[, non_constant_cols, drop = FALSE]
                correlations[non_constant_cols] <- apply(sub_X, 2, function(x) cor(x, target))
            }

            # Find highly correlated features (excluding self)
            highly_correlated <- which(abs(correlations) > correlation_threshold)
            return(colnames(X)[highly_correlated])
        },
        fit_estimator = function(X, y, sample_weight = NULL, estimator_id = 0) {
            stop("Abstract method: fit_estimator() must be implemented by child classes")
        }
    )
)
