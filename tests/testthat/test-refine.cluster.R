test_that("refine.cluster raffina assegnazioni", {
  data <- data.frame(x1 = rlnorm(50, 4, 0.3), x2 = rlnorm(50, 6, 0.3))
  var <- names(data)
  errorPatterns <- list(
    c(0, 0),            
    c(1000, 0))          
  ac <- assign.cluster (data, var, errorPatterns)
  refined <- refine.cluster(ac, var = c("x1", "x2"))
  expect_equal(nrow(refined$data), nrow(data))
  expect_true(all(refined$refined_cluster %in% c(0, unique(data$cluster))))  # Campo assumato
})



