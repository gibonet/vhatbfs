test_that("vhat_strata works", {
  res <- vhat_strata(d, INDEX = ~ xcat == "cat1", weights = "w")
  res <- as.integer(res)
  expect_equal(res, 714)
})
