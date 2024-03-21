test_that("vhat works", {
  res <- vhat(d, INDEX = ~ xcat == "cat1", weights = "w")
  res2 <- vhat(d, INDEX = ~ xcat == "cat1", weights = "w", strata = "s")
  expect_gt(res2, res)
})
