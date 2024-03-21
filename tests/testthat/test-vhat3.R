test_that("vhat3 works", {
  tmp <- n_strata(d, weights = "w", strata = "s")
  tmp$s <- 1L:3L
  d <- merge(d, tmp, by = "s", all.x = TRUE)

  # Now d has columns mh e Nh
  k <- with(d, xcat == "cat2")
  res3 <- vhat3(d$w[k], d$mh[k], d$Nh[k], d$s[k])
  
  res <- vhat(d, INDEX = ~ xcat == "cat2", weights = "w", strata = "s")
  
  expect_equal(res, res3)
})
