test_that("restrict_fun", {

  g0 = function(x,y,z) {
    x + y + z + u
  }

  g1 <- restrict_fun_sub(g0, list(z=100))  
  g2 <- restrict_fun_sub(g1, list(y=10))
  g3 <- restrict_fun_sub(g2, list(x=1))

  u <- 1000
  expect_equal(g0(x=1, y=10, z=100), 1111)
  expect_equal(g1(x=1, y=10),        1111)
  expect_equal(g2(x=1),              1111)
  expect_equal(g3(),                 1111)

  f0 = function(x,y,z) {
    x + y + z + u
  }

  ## f1 <- restrict_fun(f0, list(z=200))  
  ## f2 <- restrict_fun(f1, list(y=20))
  ## f3 <- restrict_fun(f2, list(x=2))

  
  ## u <- 2000
  ## expect_equal(f0(x=2, y=20, z=200), 2222)
  ## expect_equal(f1(x=2, y=20),        2222)
  ## expect_equal(f2(x=2),              2222)
  ## expect_equal(f3(),                 2222)
  
})



