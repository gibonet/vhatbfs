test_that("vhat_strata4 works", {
  tmp <- n_strata(d, weights = "w", strata = "s")
  tmp$s <- 1L:3L
  d <- merge(d, tmp, by = "s", all.x = TRUE)
  
  res4 <- vhat_strata4(
    data = d[d$s == 2 & d$xcat == "cat2", ], 
    weights = "w", 
    mh = "mh", 
    Nh = "Nh"
  )
  
  res3 <- vhat_strata3(
    x = d$w[d$s == 2 & d$xcat == "cat2"], 
    mh = tmp$mh[2], 
    Nh = tmp$Nh[2]
  )
  
  expect_equal(res3, res4)
})
