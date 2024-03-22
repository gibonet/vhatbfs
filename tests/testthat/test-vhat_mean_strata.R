test_that("vhat_mean_strata works", {
  res <- vhat_mean_strata(d$w, d$w)
  res2 <- vhat_mean_strata(d$w, d$w, fpc = FALSE)
  expect_gt(res, res2)
})
