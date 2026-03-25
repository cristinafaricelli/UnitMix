#' Assign Clusters Using Custom Translations and EM Algorithm
#'
#' Performs clustering on a numeric dataset by shifting the mean vector
#' according to user-defined translation vectors (shifts), and fitting a
#' Gaussian Mixture Model (GMM) using the Expectation-Maximization (EM) algorithm.
#'
#' Each observation is softly assigned to all clusters through posterior probabilities.
#' Final cluster assignment is made only if the maximum posterior probability exceeds a user-defined
#' threshold (`prob_thrsh`) and the normalized Shannon entropy of the probability vector is
#' below another threshold (`entropy_thrsh`). This allows uncertainty-aware clustering.
#' Entropy is low, close to zero, when the posterior probability is concentrated on a single cluster.
#' The maximum entropy value is 1. Values below 0.1 are generally considered acceptable.
#' The default value for `prob_thrsh` is 0, while for `entropy_thrsh` it is 1, which means that clusters
#' are returned exactly as produced by the algorithm, without any filtering.
#'
#' @param db A numeric data frame or matrix containing the variables to be clustered.
#'        All columns must be numeric and positive.
#'        
#' @param var A character vector indicating the names of the numeric variables in `db` to be used for clustering. 
#'        All specified variables must be numeric and positive.
#'
#' @param errorPatterns A list of positive numeric vectors, each representing a shift from the global
#'        mean vector to define a cluster centroid. Each vector must have the same length
#'        as the number of columns in \code{data} and each element of the vector represent the erroneous scale factor for each examined variable (see example).
#'        Avoid patterns where all the examinated variables are considered erroneus.
#'
#' @param prob_thrsh Numeric. Minimum required posterior probability for an observation
#'        to be assigned to a cluster (default is 0). If no cluster exceeds this value,
#'        the observation is left unassigned.
#'
#' @param entropy_thrsh Numeric. Maximum normalized Shannon entropy allowed for a cluster assignment
#'        (default is 1). Entropy is calculated on each observation's posterior distribution and
#'        normalized by \code{log(k)} where \code{k} is the number of clusters. Lower values indicate
#'        more confident assignments.
#'
#' @param max_iter Integer. Maximum number of iterations for the EM algorithm. Default is 500.
#'
#' @param tol Numeric. Convergence threshold for the log-likelihood. EM stops when
#'        the change in log-likelihood between iterations is below this value.
#'
#' @param ID (Optional) A character string specifying the name of the identifier column in `db`.
#'        If provided and the column exists in `db`, its values will be included in the output
#'        as the first column, labeled `ID`. This allows traceability of each corrected record.
#'        If not provided or if the specified column does not exist, a default sequence
#'        from 1 to the number of rows will be used as identifiers.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{A data frame including:
#'     \itemize{
#'       \item The observation ID (from the `ID` column if provided, otherwise a sequence number),
#'       \item The original variables used for clustering,
#'       \item The final cluster assignment for each observation (with unassigned cases labeled as \code{0}),
#'       \item The normalized Shannon entropy of the posterior distribution,
#'       \item The posterior probabilities for each cluster.
#'     }
#'   }
#'   \item{postprob}{A data frame containing posterior probabilities for each cluster. One row per observation, one column per cluster.}
#'   \item{sigma}{The estimated shared covariance matrix used in the Gaussian mixture model.}
#'   \item{mu}{A matrix of cluster centroids, obtained by translating the overall mean vector with each specified error pattern.}
#'   \item{iterations}{The number of EM iterations executed before convergence.}
#' }
#'
#' @details
#' This method assumes multivariate log-normality and equal covariance across clusters.
#' It implements robust assignment logic by rejecting uncertain observations based on entropy and minimum probability thresholds.
#' Observations with no dominant cluster (high entropy or low posterior probability) are marked with cluster 0.
#' It is the \strong{user's responsibility} to ensure that input variables are:
#' \itemize{
#'   \item Numeric
#'   \item Strictly positive (since log transformation is applied)
#'   \item Appropriately scaled (e.g., unit conversion issues should be captured by `errorPatterns`)
#' }
#' If these conditions are not met, the function will return informative error messages and halt execution.
#'
#' Input data is not preprocessed automatically: \strong{preprocessing must be handled by the user}.

#'
#' @importFrom mvtnorm dmvnorm
#' @examples
#' # Simulated dataset with unit-of-measurement error on 3 values
#' data <- data.frame(
#'   x1 = c(rlnorm(50, meanlog = 4, sdlog = 0.3), rlnorm(3, meanlog = 4, sdlog = 0.3) * 1000),
#'   x2 = rlnorm(53, meanlog = 6, sdlog = 0.3)
#' )
#'
#'  var <- names(data)
#'  
#' # Define two errorPatterns: no shift, and a ×1000 shift in x1 only
#' errorPatterns <- list(
#'   c(0, 0),            # Cluster 1: correct units
#'   c(1000, 0)          # Cluster 2: scale error ×1000
#' )
#'
#' # Run the clustering algorithm
#' ac <- assign.cluster(
#'   db = data,
#'   var= var,
#'   errorPatterns = errorPatterns
#' )
#'
#' # Examine the results
#' table(ac$data$cluster)
#'
#' @export

assign.cluster <- function(db,
                           var,
                           errorPatterns,
                           prob_thrsh    = 0,
                           entropy_thrsh = 1,
                           max_iter      = 500,
                           tol           = 1e-6,
                           ID            = NULL) {
  #--------------------------------------------------
  #  Libreria richiesta
  #--------------------------------------------------
  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Package 'mvtnorm' richiesto ma non disponibile. Installalo con install.packages('mvtnorm').")
  }
  
  #--------------------------------------------------
  #  Selezione dati
  #--------------------------------------------------
  data <- db[, var, drop = FALSE]
  
  #--------------------------------------------------
  #  Checks su input
  #--------------------------------------------------
  if (exists("check.data")) {
    vars_chk <- check.data(data, errorPatterns)
    
    if (vars_chk$ret == -9) {
      stop(vars_chk$msg.err)
    }
    if (vars_chk$ret != 0) {
      warning(vars_chk$msg.err)
    }
  }
  
  if (!all(sapply(errorPatterns, length) == ncol(data))) {
    stop("Numbers of variables and errorPatterns not coherent")
  }
  
  # Dati devono essere > 0 per il log10
  if (any(as.matrix(data) <= 0, na.rm = TRUE)) {
    stop("I dati contengono valori <= 0: la trasformazione log10 richiede valori strettamente positivi.")
  }
  
  #--------------------------------------------------
  #  Trasformazioni log10
  #--------------------------------------------------
  # errorPatterns: 0 -> 1 (nessuno shift) → log10(1)=0
  errorPatterns_log <- lapply(errorPatterns, function(x) {
    log10(ifelse(x == 0, 1, x))
  })
  
  data_log <- log10(data)
  data_matrix <- as.matrix(data_log)
  
  #--------------------------------------------------
  #  Centroidi: mu_base + errorPatterns_log
  #--------------------------------------------------
  mu_base <- colMeans(data_matrix, na.rm = TRUE)
  
  k <- length(errorPatterns_log)
  p <- length(mu_base)
  
  centers <- matrix(0, nrow = k, ncol = p)
  for (i in seq_len(k)) {
    centers[i, ] <- mu_base + errorPatterns_log[[i]]
  }
  centroidi <- centers
  
  #--------------------------------------------------
  #  Inizializzazione EM
  #--------------------------------------------------
  n    <- nrow(data_matrix)
  pi_k <- rep(1 / k, k)
  sigma <- diag(p)
  
  loglik <- -Inf
  iter <- 0
  converged <- FALSE
  
  #--------------------------------------------------
  #  EM Algorithm con log-sum-exp
  #--------------------------------------------------
  while (iter < max_iter) {
    iter <- iter + 1
    
    # E-step: log-probabilità per ciascun cluster
    log_probs <- sapply(1:k, function(j) {
      log(pi_k[j]) +
        mvtnorm::dmvnorm(data_matrix,
                         mean  = centroidi[j, ],
                         sigma = sigma,
                         log   = TRUE)
    })
    
    # stabilizzazione numerica per riga
    max_log_probs <- apply(log_probs, 1, max)
    log_probs_stab <- sweep(log_probs, 1, max_log_probs, FUN = "-")
    
    probs <- exp(log_probs_stab)
    row_sums <- rowSums(probs)
    resp <- probs / row_sums
    
    # log-likelihood corrente (log-sum-exp)
    new_loglik <- sum(log(row_sums) + max_log_probs)
    
    # controllo convergenza
    if (abs(new_loglik - loglik) < tol) {
      converged <- TRUE
      loglik <- new_loglik
      break
    }
    loglik <- new_loglik
    
    # M-step
    Nk   <- colSums(resp)
    pi_k <- Nk / n
    
    sigma <- Reduce(`+`, lapply(1:k, function(j) {
      x_centered <- sweep(data_matrix, 2, centroidi[j, ])
      t(x_centered) %*% (x_centered * resp[, j])
    })) / n
  }
  
  if (!converged) {
    warning("EM algorithm did not converge within max_iter")
  }
  
  #--------------------------------------------------
  #  Post-probabilità e entropia
  #--------------------------------------------------
  resp_df <- as.data.frame(resp)
  for (j in 1:k) {
    names(resp_df)[j] <- paste0("prob_cluster_", j)
  }
  resp_mat <- as.matrix(resp_df)
  
  entropy_row <- function(p) {
    p <- p[p > 0]
    -sum(p * log(p)) / log(k)
  }
  
  entropies <- apply(resp_mat, 1, entropy_row)
  
  cluster_assignment <- apply(resp_mat, 1, function(x) {
    ent <- entropy_row(x)
    if (max(x) >= prob_thrsh && ent < entropy_thrsh) {
      which.max(x)
    } else {
      0L
    }
  })
  
  #--------------------------------------------------
  #  Output
  #--------------------------------------------------
  out_ID <- if (!is.null(ID) && ID %in% names(db)) db[[ID]] else seq_len(nrow(db))
  
  result <- data.frame(
    ID        = out_ID,
    db[, var, drop = FALSE],
    cluster   = cluster_assignment,
    entropy   = entropies,
    resp_df,
    row.names = NULL
  )
  
  list(
    data       = result,
    postprob   = resp_df,
    sigma      = sigma,
    mu         = centroidi,
    iterations = iter
  )
}

