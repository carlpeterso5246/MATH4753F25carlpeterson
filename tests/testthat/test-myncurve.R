test_that("the muncurve function works", {
  result <- myncurve(10,5,4)

  # check that mu is correct
  expect_equal(result$mu,10)

  # check that standard deviation is correct
  expect_equal(result$sigma,5)

  #check that probability equals pnorm(a,mu,sigma)
  expect_equal(result$probability,round(pnorm(4,10,5),4))
})
