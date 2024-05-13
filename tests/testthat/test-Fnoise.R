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

test_that("Example", {
  data("USinf")
  n    <- length(USinf)
  shift<-201
  u1<-c((1:shift)/shift,rep(0, n-shift))
  u2<-c(rep(0, shift),(1:(n-shift))/(n-shift))
  u<-(1:n)/n
  switch <- c(rep(1,shift), rep(0, n-shift))
  x1<-switch*u
  x2<-(1-switch)*u
  test<- data.frame(USinf, x1=x1, x2=x2)
  results <- application(formula=USinf~x1+x2,
                    data=test,
                    N=150,
                    S=50,
                    B=20,
                    start = c(0.16,  2.0, -7,  8, -3, 0.25, -0.25, 0.01),
                    nr.cores=2,
                    d.order=4,
                    s.order=2,
                    seed=123)
  res <-  c(0.2087753305,1.5887870366,-0.0108561831,0.0009097003,1.7771289215,-6.8170159545,8.1724199357,-2.8474165569,0.3962597216,-0.7371173995,0.4331257085)
  num <- as.numeric(results$coeff[["Estimate"]])
  expect_equal(num,res)
})
