test_that("vhat_mean_strata works", {
  res <- vhat_mean_strata(d$w, d$w)
  res2 <- vhat_mean_strata(d$w, d$w, fpc = FALSE)
  
  # The estimated variance with fpc should be greater than without
  expect_gt(res, res2)
  
  # Verify that vhat_mean_strata works without weights
  res <- vhat_mean_strata(d$w)
  expect_false(is.na(res))
  
  # vhat_mean_strata works if data contains NA values
  d2 <- d
  d2$w[sample.int(nrow(d), size = 10)] <- NA
  res <- vhat_mean_strata(d2$w, d2$w)
  
  expect_false(is.na(res))
  
  # Verify that unweighted variance of mean is equal
  # to var(x) / length(x)
  res <- vhat_mean_strata(d$w)
  res2 <- var(d$w) / nrow(d)
  expect_equal(res, res2)
  
})
