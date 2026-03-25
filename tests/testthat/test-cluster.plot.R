test_that("cluster.plot genera plot senza errori", {
  data <- data.frame(x1 = rlnorm(50, 4, 0.3), x2 = rlnorm(50, 6, 0.3))
  var <- names(data)
  errorPatterns <- list(c(0, 0), c(1000, 0))
  clusters <- assign.cluster(db = data, var = var, errorPatterns = errorPatterns)
  expect_silent(cluster.plot(clusters$data, var = var))  # No error
})




