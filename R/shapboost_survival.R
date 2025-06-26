#' @importFrom methods new
#' @importFrom xgboost xgb.DMatrix xgboost
#' @importFrom Matrix Matrix
NULL

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
            X <- Matrix::Matrix(as.matrix(X), sparse = TRUE)
            y <- Matrix::Matrix(as.matrix(y), sparse = TRUE)
            # TODO: add early stopping and hyperparameter tuning
            if (estimator_id == 0) {
                dtrain <- xgboost::xgb.DMatrix(data = X, weight = sample_weight, label = y[, 1])
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    objective = "survival:cox",
                    eval_metric = "cox-nloglik",
                    verbose = verbose,
                )
            } else {
                dtrain <- xgboost::xgb.DMatrix(data = X, label = y[, 1])
                estimators[[estimator_id + 1]] <<- xgboost::xgboost(
                    data = dtrain,
                    nrounds = 100,
                    objective = "survival:cox",
                    eval_metric = "cox-nloglik",
                    verbose = verbose,
                )
            }
            return(estimators[[estimator_id + 1]])
        }
    )
)
