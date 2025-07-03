#' @importFrom methods new
#' @importFrom xgboost xgb.DMatrix xgboost
#' @importFrom Matrix Matrix
NULL

#' SHAPBoostSurvival is a reference class for survival analysis feature selection through gradient boosting.
#' 
#' This class extends the SHAPBoostEstimator class and implements methods for initializing, updating weights, scoring, and fitting estimators.
#' 
#' @field evaluator The model that is used to evaluate each additional feature. Choice between "coxph" and "xgb".
#' @field metric The metric used for evaluation, such as "mae", "mse", or "r2".
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
#' @export SHAPBoostSurvival
#' @exportClass SHAPBoostSurvival
SHAPBoostSurvival <- setRefClass("SHAPBoostSurvival",
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
            y[, 1] <- as.numeric(y[, 1])
            y[, 2] <- as.numeric(y[, 2])
            y_pred <- as.numeric(y_pred)
            cindex_res <- calculate_c_index(y_pred, y)
            discordant_pairs <- cindex_res$discordant
            all_discordant_samples <- c(discordant_pairs[, 1], discordant_pairs[, 2])
            # Count occurrences of each sample
            discordant_counts_table <- table(all_discordant_samples)
            sample_indices <- seq_len(nrow(y))
            discordant_counts <- setNames(
                rep(0, length(sample_indices)),
                sample_indices
            )
            discordant_counts[names(discordant_counts_table)] <- as.numeric(discordant_counts_table)
            discordant_counts[setdiff(sample_indices, names(discordant_counts_table))] <- 1
            discordant_counts <- as.matrix(discordant_counts)
            alpha_abs[, i + 1] <<- log(discordant_counts) + 1
            alpha[, i + 1] <<- alpha_abs[, i + 1] / alpha_abs[, i]

            global_sample_weights <<- global_sample_weights * alpha[, i + 1]
            global_sample_weights <<- global_sample_weights / sum(global_sample_weights) * length(y[, 1])
        },
        score = function(preds, y_test) {
            preds_vec <- preds$preds
            y_test_vec <- y_test
            if (identical(metric, "c-index")) {
                c_index <- calculate_c_index(preds_vec, y_test_vec)$c_index
                return(c_index)
            }
            stop("Invalid metric")
        },
        calculate_c_index = function(y_pred, y_test) {
            t <- -y_test[, 1]
            e <- as.integer(y_test[, 1] == y_test[, 2])
            p <- y_pred

            # Create pairwise comparisons
            n <- length(t)
            t_mat <- outer(t, t, "-")
            p_mat <- outer(p, p, "-")

            # Create indicator matrices
            t_gt <- t_mat > 0
            t_lt <- t_mat < 0
            p_gt <- p_mat > 0
            p_lt <- p_mat < 0
            p_eq <- p_mat == 0

            # Create masks for concordant, discordant, and tied pairs
            e_col <- matrix(e, nrow = n, ncol = n, byrow = FALSE)
            e_row <- matrix(e, nrow = n, ncol = n, byrow = TRUE)

            # Concordant pairs
            concordant1 <- t_gt & p_gt & e_col
            concordant2 <- t_lt & p_lt & e_row
            concordant_pairs <- which(concordant1 | concordant2, arr.ind = TRUE)

            # Discordant pairs
            discordant1 <- t_gt & p_lt & e_col
            discordant2 <- t_lt & p_gt & e_row
            discordant_pairs <- which(discordant1 | discordant2, arr.ind = TRUE)

            # Tied pairs
            tied1 <- t_gt & p_eq & e_col
            tied2 <- t_lt & p_eq & e_row
            tied_pairs <- which(tied1 | tied2, arr.ind = TRUE)

            # Calculate c-index
            n_concordant <- nrow(concordant_pairs)
            n_discordant <- nrow(discordant_pairs)
            n_tied <- nrow(tied_pairs)

            c_index <- (n_concordant + 0.5 * n_tied) / (n_concordant + n_tied + n_discordant)

            return(list(
                c_index = c_index,
                concordant = concordant_pairs,
                discordant = discordant_pairs
            ))
        },
        fit_estimator = function(X, y, sample_weight = NULL, estimator_id = 0) {
            # y should be negative when censored and positive when not censored
            y[, 1] <- ifelse(y[, 1] == y[, 2], y[, 1], -y[, 1])
            X_mat <- Matrix::Matrix(as.matrix(X), sparse = TRUE)
            y_mat <- Matrix::Matrix(as.matrix(y), sparse = TRUE)
            # TODO: add early stopping and hyperparameter tuning
            if (estimator_id == 0) {
                dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_mat[, 1], weight = sample_weight)
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    verbose = 0,
                    objective = "survival:cox",
                    eval_metric = "cox-nloglik",
                )
            } else if (estimator_id == 1 && evaluator == "xgb") {
                dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_mat[, 1])
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    verbose = 0,
                    objective = "survival:cox",
                    eval_metric = "cox-nloglik",
                )
            } else if (estimator_id == 1 && evaluator == "coxph") {
                X_df <- as.data.frame(as.matrix(X_mat))
                col_names <- colnames(X_df)
                non_duplicated_cols <- seq_along(col_names)
                if (any(duplicated(col_names))) {
                    non_duplicated_cols <- setdiff(seq_along(col_names), which(duplicated(col_names)))
                    X_df <- as.data.frame(X_df[, non_duplicated_cols])
                }
                colnames(X_df) <- col_names[non_duplicated_cols]

                time <- y[, 1]
                status <- ifelse(y[, 1] == y[, 2], 1, 0)
                y_surv <- survival::Surv(time, status)
                estimators[[estimator_id + 1]] <<- survival::coxph(
                    formula = y_surv ~ .,
                    data = X_df,
                    x = TRUE,
                    y = TRUE
                )
            } else {
                stop("Evaluator", evaluator, "is not supported! Please choose one of ['coxph', 'xgb'].")
            }
            return(estimators[[estimator_id + 1]])
        }
    )
)
