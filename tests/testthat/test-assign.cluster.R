test_that("assign.cluster gestisce dati validi base", {
  data <- data.frame(x1 = rlnorm(50, 4, 0.3), x2 = rlnorm(50, 6, 0.3))
  var <- names(data)
  errorPatterns <- list(c(0, 0), c(1000, 0))
  result <- assign.cluster(db = data, var = var, errorPatterns = errorPatterns)
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_equal(nrow(result$data), nrow(data))
  expect_true(all(result$data$cluster %in% c(0, 1, 2)))
})

test_that("assign.cluster rifiuta input invalido", {
  data_invalid <- data.frame(x1 = c(rlnorm(10, 4, 0.3), -1))
  var <- "x1"
  errorPatterns <- list(c(0))
  expect_error(assign.cluster(db = data_invalid, var = var, errorPatterns = errorPatterns), "Negative values")
})

test_that("assign.cluster applica thresholds correttamente", {
  data <- data.frame(x1 = rlnorm(10, 4, 0.3))
  var <- "x1"
  errorPatterns <- list(c(0), c(1))
  result <- assign.cluster(db = data, var = var, errorPatterns = errorPatterns, prob_thrsh = 0.9, entropy_thrsh = 0.1)
  expect_true(any(result$data$cluster == 0))  # Alcuni unassigned
})

