test_that("check.data passa dati validi", {
  data <- data.frame(x1 = rlnorm(10, 4, 0.3), x2 = rlnorm(10, 6, 0.3))
  errorPatterns <- list(c(0, 0))
  result <- check.data(db = data, errorPatterns = errorPatterns)
  expect_equal(result$ret, 0)
  expect_null(result$msg.err)
})

test_that("check.data rileva negativi", {
  data <- data.frame(x1 = c(rlnorm(9, 4, 0.3), -0.1))
  errorPatterns <- list(c(0))
  result <- check.data(db = data, errorPatterns = errorPatterns)
  expect_equal(result$ret, -9)
  expect_match(result$msg.err, "Negative values")
})

test_that("check.data rileva NA in errorPatterns", {
  data <- data.frame(x1 = rlnorm(10, 4, 0.3))
  errorPatterns <- list(c(NA))
  result <- check.data(db = data, errorPatterns = errorPatterns)
  expect_equal(result$ret, -9)
  expect_match(result$msg.err, "Missing values")
})


