test_that("vhat_strata3 works", {
  tmp <- n_strata(d, weights = "w", strata = "s")
  tmp
  tmp$s <- 1L:3L
  
  res3 <- vhat_strata3(
    d$w[d$s == 2 & d$xcat == "cat2"], mh = tmp$mh[2], Nh = tmp$Nh[2]
  )
  
  res2 <- vhat_strata2(d[d$s == 2 & d$xcat == "cat2", ],
    weights = "w", mh = tmp$mh[2], Nh = tmp$Nh[2]
  )
  
  expect_equal(res2, res3)
})
