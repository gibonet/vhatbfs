test_that("n_strata returns the number of rows equal to the number of strata", {
  n <- nrow(n_strata(d, weights = "w", strata = "s"))
  k <- length(unique(d$s))
  
  expect_equal(n, k)
})
