test_that("vhat_strata5 works", {
  tmp <- n_strata(d, weights = "w", strata = "s")
  tmp$s <- 1L:3L
  d <- merge(d, tmp, by = "s", all.x = TRUE)

  # Now d has columns mh e Nh
  head(d)

  k <- with(d, s == 2 & xcat == "cat2")
  res5 <- vhat_strata5(d$w[k], d$mh[k], d$Nh[k])
  
  res4 <- vhat_strata4(
    data = d[d$s == 2 & d$xcat == "cat2", ], 
    weights = "w", 
    mh = "mh", 
    Nh = "Nh"
  )
  
  expect_equal(res4, res5)
})
