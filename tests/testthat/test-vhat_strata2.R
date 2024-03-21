test_that("vhat_strata2 works", {
  tmp <- n_strata(d, weights = "w", strata = "s")
  tmp
  tmp$s <- 1L:3L
  
  res <- vhat_strata2(
    data = d[d$s == 2 & d$xcat == "cat2", ],
    weights = "w", 
    mh = tmp$mh[2], 
    Nh = tmp$Nh[2]
  )
  
  expect_equal(as.integer(res), 185)
})
