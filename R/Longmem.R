kappa <- function(n, beta, alpha){
  u <- (1:n)/n
  d <- beta[1]+ u*exp(u*beta[2])
  sigma <- alpha[1] + alpha[2]*u + alpha[3]*u*u
  out <- matrix(0,ncol=n,nrow =n)
  for (i in 1:n){
    j <- 1:i
    a0 <- gamma(1-d[i]-d[j])
    a1 <- lgamma(i-j+d[i])
    a2 <- lgamma(i-j+1-d[j])
    a3 <- gamma(1-d[i])*gamma(d[i])
    out[i,j] <- sigma[i] * sigma[j] * a0 * exp(a1-a2)/a3
  }
  out <- out + t(out)
  diag(out) <- diag(out)/2
  return(out)
}

lsfn.innov.long <- function(n,beta,alpha,z){
  a <- kappa(n, beta = beta, alpha = alpha)
  b <- chol(a)
  y <- t(b)%*%z
  return(as.vector(y))
}

lsfn.long <- function(series1,N,S,start){
  {
    T <- length(series1)
    u<-(1:T)/T
    x<- u
    fit <-lm(series1~x-1)
    LS<-coef(fit)
    series1<-fit$res
    aux <- nlminb(start=start,whittle.linear.long,series=series1,N=N,S=S)
    par <- aux$par
    loglik <- -aux$objective
  }
  list(par,LS)
}

whittle.linear.long <- function(x,series,N,S){
  T1 <- length(series)
  M <- trunc((T1 - N + S)/S)
  beta.0 <- x[1]
  beta.1 <- x[2]
  alpha.0 <- x[3]
  alpha.1 <- x[4]
  alpha.2 <- x[5]
  u1=(1:T1)/T1
  d <- vector("numeric")
  d <- beta.0 + u1*exp(u1*beta.1)
  sigma <-vector("numeric")
  sigma <-alpha.0 + alpha.1 * u1 + alpha.2 * (u1*u1)
  if(all((min(d) <= 0) | max(d)>=0.48  | sum(is.na(d))==1 | sum(is.infinite(d))==1 | min(sigma) <=0 | sum(is.na(sigma))==1 | sum(is.infinite(sigma))==1)){
    loglik = 100000000000. }
  else{
    cc <- vector("numeric")
    for(j in 1:M) {
      u<-(((S * (j - 1))/T1 + (0.5 * N)/T1))
      d <-beta.0 + u*exp(u*beta.1)
      sigma <- alpha.0 + alpha.1*u + alpha.2*u*u
      i.ini <- S * (j - 1) + 1
      i.fin <- i.ini + N - 1
      series.j <- series[i.ini:i.fin]
      cc[j] <- whittle.taper.loglik(d=d, series = series.j, sigma = sigma)
    }
    loglik <- mean(cc)/(4*pi)
  }
  return(loglik)
}

psi<-function(d, m){
  c(1, gamma(d + 1:m)/(gamma(d) * gamma(1:m + 1)))
}

lsfn.kalman.filter_reg<-function(param,series,h,m)
{
  n <- length(series)
  u.d <- (1:n)/n
  d.u <- param[1]  + u.d*exp(param[2]*u.d)
  sigma.u <- param[3] + param[4]*u.d+param[5]*u.d*u.d
  d <- d.u
  sigma <- sigma.u
  ind.d <- (d <= 0 | d >= 0.5)
  ind.s <- (sigma <= 0)
  if (sum(ind.d) + sum(ind.s) == 0) {
    M <- m + 1
    sigma[(n + 1)] <- sigma[n]
    Omega <- matrix(0, nrow = M, ncol = M)
    diag(Omega) <- sigma[1]^2
    X <- rep(0, M)
    delta <- vector("numeric")
    hat.y <- vector("numeric")
    for (i in 1:n) {
      g <- sigma[i] * rev(psi(d[i], m))
      aux <- Omega %*% g
      delta[i] <- g %*% aux
      Theta <- c(aux[2:M], 0)
      aux <- matrix(0, nrow = M, ncol = M)
      aux[1:m, 1:m] <- Omega[2:M, 2:M]
      aux[M, M] <- 1
      if (is.na(series[i])) {
        Omega <- aux
        hat.y[i] <- t(g) %*% X + param[6]*h[i]
        X <- c(X[2:M], 0)
      }
      else {
        Omega <- aux - Theta %*% t(Theta)/delta[i]
        hat.y[i] <- t(g) %*% X +  param[6]*h[i]
        aux <- c(X[2:M], 0)
        X <- aux + Theta * (series[i] - hat.y[i])/delta[i]
      }
    }
    loglik <- sum(log(delta)) + sum(na.exclude((series - hat.y)^2/delta))
    res <- (series - hat.y)
    res.stand <- res/sigma[1:n]
    OUT = NULL
    OUT$res = res
    OUT$res.stand = res.stand
    OUT$y.hat = hat.y
    OUT$delta = delta
    OUT$par = param
    OUT$loglik = loglik
    OUT$truncation = m
    OUT$call = match.call()
    OUT$method = c("Kalman")
    class(OUT) = c("LSmodel")
  }
  else {
    OUT = NULL
    OUT$loglik = 10^10
  }
  OUT
}

VarAsintotica<-function(n, param){
  A2<-vector("numeric")
  d.function22<-function(x){
    return(-(param[1] + x*exp(param[2]*x)))
  }
  uo<-nlminb(start = 0.47, d.function22)$par
  x<-c(uo)

  d.function2=function(x){
    return((param[1] + x*exp(param[2]*x)))
  }

  d0<-d.function2(uo)

  g2.function=function(x,y){
    dx<-d.function2(x)
    dy<-d.function2(y)
    return(gamma(1-dx-dy)/(gamma(1-dx)*gamma(dx)) )
  }
  der<- -2*exp(-1)
  A2<-4^{d0}*sqrt(pi)*(n^{2*d0-1})*g2.function(uo,uo)*gamma(d0)* (9*x^2)*(-der*log(n))^{-d0-0.5}
  return(A2)
}
