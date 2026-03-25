#' Check Input Data for Clustering Function
#'
#' Validates the input dataset and offset vectors for compatibility with the clustering algorithm.
#'
#' @param db A numeric matrix or data frame. All columns must be numeric, without missing, infinite, or NaN values.
#' 
#' @param errorPatterns A list of positive numeric vectors representing the shifts applied to the base mean. Must not contain NA values.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{ret}{Return code: 0 if checks pass, -9 if an error condition is met.}
#'   \item{msg.err}{A character string describing the error, if any.}
#' }
#'
#' #' @details
#' It is the **user's responsibility** to ensure that input variables are:
#' \itemize{
#'   \item Numeric
#'   \item Strictly positive (since log transformation is applied)
#'   \item Appropriately scaled (e.g., unit conversion issues should be captured by `errorPatterns`)
#' }
#' If these conditions are not met, the function will return informative error messages and halt execution.
#'
#' Input data is not preprocessed automatically: **preprocessing must be handled by the user.**
#'
#' @keywords internal
#' @export
check.data <- function(db, errorPatterns) {

  data <- as.matrix(db)
  lista <- list(ret = 0, msg.err = NULL)

  # Input type check: must be matrix or data frame
  if (!inherits(data, c("data.frame", "matrix"))) {
    lista$msg.err <- "Data must be supplied as a matrix or as a dataframe"
    lista$ret <- -9

    # All columns must be numeric
  } else if (!all(sapply(data, is.numeric))) {
    lista$msg.err <- "All variables in the dataset must be numeric"
    lista$ret <- -9

    # No negative values
  } else if (any(data < 0, na.rm = TRUE)) {
    lista$msg.err <- "Negative values are not allowed"
    lista$ret <- -9

    # No missing values
  } else if (any(is.na(data))) {
    lista$msg.err <- "Missing values (NA) are not allowed"
    lista$ret <- -9

    # No NaN or Inf values
  } else if (any(is.nan(data)) || any(is.infinite(data))) {
    lista$msg.err <- "NaN and +/- Inf values are not allowed"
    lista$ret <- -9

    # No values too close to zero
  } else if (any(data < 1e-11, na.rm = TRUE)) {
    lista$msg.err <- "Values lower than 1e-11 are not allowed"
    lista$ret <- -9
  }

  # Check for NA in errorPatterns vectors
  if (any(sapply(errorPatterns, function(x) any(is.na(x))))) {
    lista$msg.err <- "Missing values (NA) are not allowed in 'errorPatterns'"
    lista$ret <- -9

    # No negative errorPatterns
  } else if (any(sapply(errorPatterns, function(x) any(x < 0)))) {
    lista$msg.err <- "Negative values are not allowed in 'errorPatterns' "
    lista$ret <- -9
  }

  return(lista)
}
