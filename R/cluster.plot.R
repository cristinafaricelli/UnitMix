#' Visualize Cluster Assignments with Pairwise Scatterplots
#'
#' This function dynamically generates a set of scatter plots for all pairs of variables, log-transformed, in the provided dataset,
#' enabling detailed visualisation of cluster assignments.
#' The points are coloured according to the cluster to which each observation is assigned, based on posterior probabilities.
#' If the number of variables is large, the number of plots is automatically adjusted to maintain readability,
#' creating multiple graphs.
#' Observations are only assigned to a cluster if the a posteriori probability for that cluster is above a specified threshold (default = 0.6).
#'
#' @param db A numeric data frame or matrix containing the variables to plot.
#' 
#' @param var A character vector indicating the names of the ckusters numeric variables in `db. All specified variables must be numeric and positive.
#' 
#' @param postprob A matrix or dataframe of posterior probabilities where each column represents a cluster.
#' It is typically obtained from the function \code{assign.cluster()}. If set to 1 (default), it is internally set to a matrix
#' assigning all observations to a single cluster. They must have the same row number of 'db'
#' 
#' @param errorPatterns A list of positive numeric vectors that have been identified as being associated with potential unit errors
#' which will be highlighted in the graphs. If not provided, no special errorPatterns are highlighted (default = ncol(data), used internally)
#' 
#' @param threshold Minimum probability threshold for cluster assignment (default = 0.6).
#'
#' @details
#' It is the \strong{user's responsibility} to ensure that input variables are:
#' \itemize{
#'   \item Numeric
#'   \item Strictly positive (since log transformation is applied)
#'   \item Appropriately scaled (e.g., unit conversion issues should be captured by `errorPatterns`)
#' }
#' If these conditions are not met, the function will return informative error messages and halt execution.
#'
#' Input data is not preprocessed automatically: \strong{preprocessing must be handled by the user}.
#' @return No value returned. The function produces side-effect graphs, which are useful for visually inspecting
#' the separation between clusters and the reliability of assignments.
#'
#' @seealso \code{\link{assign.cluster}}
#' @examples

# Simulated dataset with unit-of-measurement error on 3 values


#' data <- data.frame(
#'  x1 = c(rlnorm(50, meanlog = 4, sdlog = 0.3), rlnorm(10, meanlog = 4, sdlog = 0.3) * 1000),
#'  x2 = rlnorm(60, meanlog = 6, sdlog = 0.3)
#' )
#'
#' var <- names(data)

#' # Define two errorPatterns: no shift, and a ×1000 shift in x1 only
#' errorPatterns <- list(
#'   c(0, 0),            # Cluster 1: correct units
#'   c(1000, 0)          # Cluster 2: scale error ×1000
#' )
#' 
#' results <- assign.cluster(
#'   db = data,
#'   var= var,
#'   errorPatterns = errorPatterns
#' )
#' 
#' cluster.plot(data, var, postprob = results$postprob, errorPatterns, threshold = 0.6)

#' @export

cluster.plot <- function(db,var, postprob = 1, errorPatterns = ncol(data), threshold = 0.6) {
  
  data<-db[,var]
  
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix.")
  }

  n_obs <- nrow(data)

  if (length(postprob) == 1 && postprob == 1) {
    postprob <- matrix(1, nrow = n_obs, ncol = 1)
  }

  if (is.data.frame(postprob)) {
    postprob <- as.matrix(postprob)
  }

  if (!is.matrix(postprob)) {
    stop("'postprob' must be a matrix or data frame.")
  }

  if (nrow(postprob) != n_obs) {
    stop("'data' and 'postprob' must have the same number of rows.")
  }

  ncluster <- ncol(postprob)

  if (!(is.numeric(errorPatterns) && length(errorPatterns) == 1 && errorPatterns == ncol(data))) {
    if (!is.list(errorPatterns)) {
      stop("'errorPatterns' must be a list of integer vectors.")
    }
    if (length(errorPatterns) != ncluster) {
      stop("Length of 'errorPatterns' must match the number of clusters (columns in 'postprob').")
    }
  }

  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    stop("'threshold' must be a numeric value between 0 and 1.")
  }

  vars <- check.data(data, errorPatterns)

  generate_colors <- function(n) {
    hues <- seq(15, 375, length.out = n + 1)[-1]
    grDevices::hcl(h = hues, c = 100, l = 65)
  }

  colors <- generate_colors(ncluster)
  variabili <- names(data)

  data <- log(data)

  var_combinations <- combn(variabili, 2, simplify = FALSE)
  nplots <- length(var_combinations)
  nrow_plot <- ceiling(nplots / 2)

  if (interactive()) {
    grDevices::dev.new()
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)

  graphics::par(mfrow = c(nrow_plot, 2), mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0))

  for (i in seq_along(var_combinations)) {
    vars <- var_combinations[[i]]

    graphics::plot(data[[vars[1]]], data[[vars[2]]],
         xlab = vars[1], ylab = vars[2],
         cex.lab = 0.9, col = "gray90", pch = 19, cex = 0.6)

    for (j in 1:ncluster) {
      idx <- postprob[, j] > threshold
      graphics::points(data[idx, vars[1]], data[idx, vars[2]],
             col = colors[j], pch = 19, cex = 0.7)
    }

    graphics::grid(lty = 3, col = "gray80")

    if (i == 1) {
      clusters_present <- which(colSums(postprob > threshold) > 0)
      graphics::legend("topleft",
             legend = paste("Cluster", clusters_present),
             col = colors[clusters_present],
             pch = 19,
             cex = 0.7,
             pt.cex = 0.9,
             x.intersp = 0.6,
             y.intersp = 0.8,
             ncol = 2,
             bty = "n",
             inset = 0.01,
             xpd = NA)
    }
  }
}
