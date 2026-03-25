#' Refine Cluster Assignments Using Mahalanobis Distance Compatibility
#'
#' Post-processing function for the output of \code{\link{assign.cluster}}. 
#' It evaluates the compatibility of each record with its assigned cluster 
#' using Mahalanobis distance on log-transformed data. Records incompatible 
#' with their cluster (distance > chi-square cutoff) or belonging to unreliable 
#' clusters (low compatibility proportion or small size) are reassigned to 
#' cluster 0 (unassigned). This enhances assignment reliability by discarding 
#' outliers and unstable groups.
#'
#' @param ac List. Output object returned by \code{\link{assign.cluster}}.
#' @param vars Character vector. Names of the variables used in 
#'   \code{assign.cluster}. Must match exactly those in \code{ac}.
#' @param compat_level Numeric. Confidence level for the chi-square cutoff 
#'   used to assess individual compatibility (default: 0.99).
#' @param min_good_prop Numeric. Minimum proportion of compatible records 
#'   required to consider a cluster reliable (default: 0.90).
#' @param min_cluster_size Integer. Minimum cluster size required to consider 
#'   a cluster valid (default: 5).
#'
#' @return A list containing:
#' \item{data}{ A data frame with the original results and additional
#'   columns: \code{mahal_d2} ( Mahalanobis distance squared),
#'   \code{compatible_Mah} (logical indicator of compatibility), and
#'   \code{cluster_refined} (refined cluster assignments).}
#' \item{postprob}{ Posterior probability matrix with rows set to zero
#'   for discarded records.}
#' \item{cluster_summary}{ Diagnostic table reporting, for each cluster:
#'   \code{n_total}, \code{n_compatible}, \code{n_incompatible}, and
#'   \code{prop_compatible}.}
#' \item{params}{ List of parameters used in the refinement process.}
#'
#' @details 
#' This method assumes multivariate log-normality and uses the shared covariance 
#' matrix from \code{assign.cluster}. It is the user's responsibility to ensure 
#' input variables are numeric and strictly positive (log transformation applied). 
#' Cluster-level diagnostics exclude cluster 0. Good clusters must meet both 
#' size and compatibility thresholds. For visualization pipe to \code{\link{cluster.plot}}.
#'
#' @seealso \code{\link{assign.cluster}}, \code{\link{cluster.plot}}
#'
#' @examples
#' # Simulated dataset with:
#' # - 50 correct observations
#' # - 3 unit-of-measurement errors (x1 ×1000)
#' # - 2 structural outliers
#'
#' set.seed(123)
#'
#' data <- data.frame(
#'   x1 = c(
#'     rlnorm(50, meanlog = 4, sdlog = 0.3),        # correct data
#'     rlnorm(3,  meanlog = 4, sdlog = 0.3) * 1000, # scale error
#'     rlnorm(2,  meanlog = 8, sdlog = 0.1)         # outliers
#'   ),
#'   x2 = c(
#'     rlnorm(50, meanlog = 6, sdlog = 0.3),
#'     rlnorm(3,  meanlog = 6, sdlog = 0.3),
#'     rlnorm(2,  meanlog = 9, sdlog = 0.1)         # outliers
#'   )
#' )
#'
#' var <- names(data)
#'
#' # Define two errorPatterns: no shift, and ×1000 shift in x1 only
#' errorPatterns <- list(
#'   c(0, 0),       # Cluster 1: correct units
#'   c(1000, 0)     # Cluster 2: scale error ×1000
#' )
#'
#' # Run the clustering algorithm
#' ac <- assign.cluster(
#'   db = data,
#'   var = var,
#'   errorPatterns = errorPatterns
#' )
#'
#' # Examine initial cluster assignments
#' table(ac$data$cluster)
#' 
#' # Run the refine cluster algorithm
#' rc <- refine.cluster(ac, var, min_cluster_size=1) 
#' 
#' # Re-examine cluster assignments
#' table(rc$data$cluster_refined)
#'
#' @export

refine.cluster <- function(ac,
                           vars,
                           compat_level     = 0.99,  # confidence level (chi-square)
                           min_good_prop    = 0.90,  # minimum share of compatible records to keep a cluster
                           min_cluster_size = 5      # minimum size for a cluster to be considered reliable
) {
  # ac = output of assign.cluster()
  # vars = vector of variables used in assign.cluster (e.g., c("GAS_ACQ_QUANT_smc","GAS_ACQ_SPESA"))
  
  if (is.null(ac$data) || is.null(ac$sigma) || is.null(ac$mu) || is.null(ac$postprob)) {
    stop("Object 'ac' does not appear to be a valid output of assign.cluster (missing data/sigma/mu/postprob).")
  }
  
  df       <- ac$data
  Sigma    <- ac$sigma
  Mu       <- ac$mu
  postprob <- ac$postprob
  
  if (!"cluster" %in% names(df)) {
    stop("Column 'cluster' is missing in ac$data.")
  }
  
  # 1) Align Mu and Sigma with the variables used in the model
  if (!is.null(colnames(Mu))) {
    Mu <- Mu[, vars, drop = FALSE]
  }
  if (!is.null(colnames(Sigma))) {
    Sigma <- Sigma[vars, vars, drop = FALSE]
  }
  
  # Check dimensional consistency
  p <- length(vars)
  if (ncol(Mu) != p || nrow(Sigma) != p || ncol(Sigma) != p) {
    stop("Inconsistent dimensions between Mu/Sigma and vars.  
         Ensure that 'vars' matches exactly the variables used in assign.cluster.")
  }
  
  # 2) Transform data to log10 scale for the model variables
  #    (assign.cluster operates in log10 scale, while output data are in original scale)
  X_log <- as.matrix(log10(df[, vars, drop = FALSE]))
  
  # 3) Mahalanobis distance from the centroid of the assigned cluster
  cl     <- df$cluster
  k      <- nrow(Mu)
  n      <- nrow(X_log)
  Sinv   <- solve(Sigma)
  cutoff <- qchisq(compat_level, df = p)   # chi-square threshold (e.g., 99%)
  
  d2 <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    c_i <- cl[i]
    if (is.na(c_i) || c_i == 0L || c_i > k) {
      next
    }
    xc <- X_log[i, ] - Mu[c_i, ]
    d2[i] <- as.numeric(t(xc) %*% Sinv %*% xc)
  }
  
  compatible <- !is.na(d2) & d2 <= cutoff
  
  # 4) Cluster-level diagnostics (excluding cluster 0)
  tab <- table(cl, compatible)
  cl_levels <- as.integer(rownames(tab))
  cl_levels <- cl_levels[cl_levels != 0]
  
  cluster_summary <- lapply(cl_levels, function(cj) {
    row <- tab[as.character(cj), , drop = FALSE]
    n_true  <- if ("TRUE"  %in% colnames(row)) row[1, "TRUE"]  else 0L
    n_false <- if ("FALSE" %in% colnames(row)) row[1, "FALSE"] else 0L
    n_tot   <- n_true + n_false
    prop    <- if (n_tot > 0) n_true / n_tot else NA_real_
    
    data.frame(
      cluster         = cj,
      n_total         = n_tot,
      n_compatible    = n_true,
      n_incompatible  = n_false,
      prop_compatible = prop,
      stringsAsFactors = FALSE
    )
  })
  
  cluster_summary <- do.call(rbind, cluster_summary)
  
  # 5) Identify "good" clusters: sufficiently large and with high compatibility rate
	good_clusters <- cluster_summary$cluster[
	  !is.na(cluster_summary$prop_compatible) &
		cluster_summary$prop_compatible >= min_good_prop &
		cluster_summary$n_total >= min_cluster_size
	]

  
  cluster_summary$is_good <- cluster_summary$cluster %in% good_clusters
  
  # 6) Keep records only if:
  #    - they belong to a good cluster
  #    - they are compatible according to Mahalanobis distance
  keep_row <- cl %in% good_clusters & compatible
  
  cluster_refined <- ifelse(keep_row, cl, 0L)
  
  # 7) Set posterior probabilities to zero for discarded records
  postprob_refined <- postprob
  if (is.matrix(postprob_refined)) {
    postprob_refined[!keep_row, ] <- 0
  }
  
  # 8) Output: refined data and diagnostics
  df_out <- df
  df_out$mahal_d2        <- d2
  df_out$compatible_Mah  <- compatible
  df_out$cluster_refined <- cluster_refined
  
  list(
    data            = df_out,
    postprob        = postprob_refined,
    cluster_summary = cluster_summary,
    params          = list(
      vars             = vars,
      compat_level     = compat_level,
      cutoff_chisq     = cutoff,
      min_good_prop    = min_good_prop,
      min_cluster_size = min_cluster_size
    )
  )
}
