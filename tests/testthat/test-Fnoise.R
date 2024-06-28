test_that("Valid Input",{
  expect_error(application(formula=USinf~x1+x2,data=test,N=150,S=50, B=10, start = c(0.16,  2.0, -7,  8, -3, 0.25, -0.25, 0.01),nr.cores=5,d.order=4,
                      s.order=-2),
               "invalid s.order",
               fixed=T)

  expect_error(application(formula=USinf~x1+x2,data=test,N=150,S=50, B=10, start = c(0.16,  2.0, -7,  8, -3, 0.25, -0.25, 0.01),nr.cores=5,s.order=2,
                      d.order=-4),
               "invalid d.order",
               fixed=T)
  expect_error(application(formula=USinf~x1+x2,data=test,N=150, B=10, start = c(0.16,  2.0, -7,  8, -3, 0.25, -0.25, 0.01),nr.cores=5,d.order=4,s.order=2,
                      S=-2),
               "invalid parameters",
               fixed=T)
  expect_error(application(formula=USinf~x1+x2,data=test,N=150,S=50, B=10, nr.cores=5,d.order=4,s.order=2,
                      start = c(0.16,  2.0, -7,  8, -3, 0.25, -0.25, 0.01, 1)),
               "los valores iniciales no coinciden",
               fixed=T)
  expect_error(application(data=test,N=150,S=50, B=100, start = c(0.16,  2.0, -7,  8, -3, 0.25, -0.25, 0.01),nr.cores=5,d.order=4,s.order=2,
                      formula=8),
               "invalid formula",
               fixed=T)
})
