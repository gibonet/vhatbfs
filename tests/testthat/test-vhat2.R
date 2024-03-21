test_that("vhat2 works", {
  tmp <- n_strata(d, weights = "w", strata = "s")
  tmp$s <- 1L:3L
  d <- merge(d, tmp, by = "s", all.x = TRUE)

  # Now d has columns mh e Nh
  res2 <- vhat2(
    data = d[d$xcat == "cat2", ], 
    weights = "w", 
    mh = "mh", 
    Nh = "Nh", 
    strata = "s"
  )
  
  res <- vhat(d, INDEX = ~ xcat == "cat2", weights = "w", strata = "s")
  
  expect_equal(res, res2)
})
