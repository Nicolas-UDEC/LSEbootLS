lsfn.whittle <- function(series1, N, S, start ,d.order,s.order){
    aux <- nlminb(start = start,objective = whittle.linear.loglik, series = series1, N = N, S = S, d.order = d.order, s.order = s.order)
    par <- aux$par
    loglik <- -aux$objective
  return(par)
}

dissss <- function(beta,u5){
  X <- matrix(NA, ncol = length(beta), nrow = length(u5))
  for(i in 1:length(beta)){
    X[,i] <- u5^(i-1)
  }
  d  <- vector("numeric")
  d  <- X %*% beta
  return(as.vector(d))
}

whittle.linear.loglik <- function(x, series, N, S,d.order,s.order){
  T1 <- length(series)
  M  <- trunc((T1 - N + S)/S)
  u1 <- 1:T1/T1
  d  <- vector("numeric")
  d  <- dissss(beta=x[1:(d.order+1)],u5=u1)
  sigma <-  vector("numeric")
  sigma <-  dissss(beta=x[(d.order+2):s.order],u5=u1)
  min.d.u <- min(d)
  max.d.u <- max(d)
  min.sigma.u <- min(sigma)
  if( (min.d.u<=0)||(max.d.u>=0.499)||(min.sigma.u<=0) ){
    loglik <-  100000000000.
  }
  else{
    cc <- vector("numeric")
    for(j in 1:M) {
      u <-((S * (j - 1))/T1 + (0.5 * N)/T1)
      d <- dissss(beta=x[1:(d.order+1)],u5=u)
      sigma <- dissss(beta=x[(d.order+2):s.order],u5=u)
      i.ini <- S * (j - 1) + 1
      i.fin <- i.ini + N - 1
      series.j <- series[i.ini:i.fin]
      cc[j] <- whittle.taper.loglik(d=d, series = series.j, sigma = sigma)
    }
    loglik <- mean(cc)/(4*pi)
  }
  return(loglik)
}

whittle.taper.loglik <- function(d, series, sigma){
  series <- series - mean(series)
  series <- taper(series)
  a <- fft(series)
  a <- Mod(a)^2
  n <- length(series)
  a <- (4 * a[2:n])/(pi * 3 * n)
  w <- (2*pi * (1:(n-1)))/n
  b <- sigma^2*fn.density (lambda = w, d = d)
  loglik <- 2 * pi * ( sum(log(b)) + sum(a/b) )
  return(loglik/n)
}

fn.density <- function(lambda, d){
  a <- (2 * sin(lambda/2))^(-2 * d)
  a <- a/(2 * pi)
  return(a)
}

taper <- function(x){
  n <- length(x)
  a <- 0.5*(1-cos(2*pi*(0:(n-1))/n))
  return(x*a)
}

kapp <- function(n, beta, alpha){
  u1<- 1:n/n
  d <- dissss(beta,u1)
  sigma <- dissss(alpha,u1)
  out <- matrix(0,ncol=n,nrow=n)
  for (i in 1:n){
    j <- 1:i
    a0 <- gamma(1-d[i]-d[j])
    a1 <- lgamma(i-j+d[i])
    a2 <- lgamma(i-j+1-d[j])
    a3 <- gamma(1-d[i])*gamma(d[i])
    out[i,j] = sigma[i] * sigma[j] * a0 * exp(a1-a2)/a3
  }
  out <- out + t(out)
  diag(out) <- diag(out)/2
  return(out)
}

lsfn.innov.sim <- function(n = n, alpha, beta,z = z){
  a<-kapp(n, beta = beta, alpha = alpha)
  b<-chol(a,pivot=T)
  y<-t(b)%*%z
  return(as.vector(y))
}

test_t<- function(value){
  sd   <- c(apply(value, 2, sd))
  mean <- c(apply(value, 2, mean))
  return(round(as.vector(mean/sd),3))
}

bootstrap.summary <- function(b, t, p) {
  tibble(
    `Name` = names(b),
    `Estimate` = b,
    `t_value` = t,
    `Pr(>|t|)` = p
  )
}


checkInput <- function(formula, data, start, d.order, s.order, N, S, B, nr.cores, seed) {
  if(N <= 0 || S <= 0 || B <= 0 || nr.cores <=0 || nr.cores > detectCores() ) stop("invalid parameters")
  if(s.order < 0 || is.null(s.order)) stop("invalid s.order")
  if(d.order < 0 || is.null(d.order)) stop("invalid d.order")
  if(!inherits(formula, "formula")) stop("invalid formula")
  if(length(start) != (d.order+ s.order + 2)) stop("los valores iniciales no coinciden")
}
