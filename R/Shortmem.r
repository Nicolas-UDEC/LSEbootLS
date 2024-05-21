whittle.taper.logshort <- function(phi, series, sigma){
  series <- series - mean(series)
  series <- taper(series)
  a <- fft(series)
  a <- Mod(a)^2
  n <- length(series)
  a <- (4 * a[2:n])/(pi * 3 * n)
  w <- (2*pi * (1:(n-1)))/n
  b <- sigma^2*lsar1.density(lambda = w, phi = phi)
  loglik <- 2 * pi * ( sum(log(b)) + sum(a/b) )
  return(loglik/n)
}

lsar1.density <- function(lambda, phi){
  a <- (Mod(1-phi*exp(1i*lambda)))^(-2)/(2*pi)
  return(a)
}

whittle.linear.loglik.short <- function(x, series, N, S){
  omega<-2*pi
  T1 <- length(series)
  M <- trunc((T1 - N + S)/S)
  beta.0 <- x[1]
  beta.1 <- x[2]
  alpha.0 <- x[3]
  alpha.1 <- x[4]
  alpha.2 <- x[5]

  cond01<-is.infinite(beta.0)||is.nan(beta.0)
  cond02<-is.infinite(beta.1)||is.nan(beta.1)
  cond03<-is.infinite(alpha.0)||is.nan(alpha.0)
  cond04<-is.infinite(alpha.1)||is.nan(alpha.1)
  cond05<-is.infinite(alpha.2)||is.nan(alpha.2)

  if((cond01+cond02+cond03+cond04+cond05) ==0 )
  {
    u <- 1:T1/T1
    phi.u <- beta.0+beta.1*cos(u*omega)
    sigma.u <- alpha.0+alpha.1*u+alpha.2*u*u
    ind.d <- (  phi.u <= -1 | phi.u >= 1)
    ind.s <- (sigma.u <= 0)

    if (sum(ind.d) + sum(ind.s) == 0)
    {
      cc <- vector("numeric")
      for(j in 1:M) {
        phi <- beta.0 + beta.1 * cos(((S * (j - 1))/T1 + (0.5 * N)/T1)*omega)
        sigma <- alpha.0 + alpha.1 * ((S * (j - 1))/T1 + (0.5 * N)/T1)+alpha.2 * ((S * (j - 1))/T1 + (0.5 * N)/T1)* ((S * (j - 1))/T1 + (0.5 * N)/T1)
        i.ini <- S * (j - 1) + 1
        i.fin <- i.ini + N - 1
        series.j <- series[i.ini:i.fin]
        cc[j] <- whittle.taper.logshort(phi= phi, series = series.j, sigma = sigma)
      }
      loglik <- mean(cc)/(4*pi)
    }

    else
    {
      loglik = 100000000000.
    }

  }
  else
  {
    loglik = 100000000000.
  }
  return(loglik)
}

lsfn.whittle2 <- function(series1, N, S, start = NULL){
  {
    aux <- nlminb(start = start, whittle.linear.loglik2,series = series1, N = N, S = S)
    par <- aux$par
    loglik <- -aux$objective
  }
  list(par)
}

lsfn.whittle.short2 <- function(series1,N,S,start){
  {
    aux <- nlminb(start = start, whittle.linear.loglik.short,series = series1, N = N, S = S)
    par <- aux$par
    loglik <- -aux$objective
  }
  list(par)
}

lsfn.whittle.short <- function(series1,N,S,start2){
  {
    T <- length(series1)
    u<-(1:T)/T
    x<- u
    fit <-lm(series1~x-1)
    LS<-coef(fit)
    series1<-fit$res
    aux <- nlminb(start = start2, whittle.linear.loglik.short,series = series1, N = N, S = S)
    par <- aux$par
    loglik <- -aux$objective
  }
  list(par,LS)
}

lsfn.whittle22 <- function(series1, N, S, start =NULL){
  {
    T <- length(series1)
    u<-(1:T)/T
    x<- u
    fit <-lm(series1~x-1)
    LS<-coef(fit)
    series1<-fit$res
    aux <- nlminb(start = start, whittle.linear.loglik2,series = series1, N = N, S = S)
    par <- aux$par
    loglik <- -aux$objective
  }
  list(par,LS)
}

whittle.linear.loglik2 <- function(x, series, N, S){
  omega<-2*pi
  T1 <- length(series)
  M <- trunc((T1 - N + S)/S)
  beta.0 <- x[1]
  beta.1 <- x[2]
  alpha.0 <- x[3]
  alpha.1 <- x[4]
  cond01<-is.infinite(beta.0)||is.nan(beta.0)
  cond02<-is.infinite(beta.1)||is.nan(beta.1)
  cond03<-is.infinite(alpha.0)||is.nan(alpha.0)
  cond04<-is.infinite(alpha.1)||is.nan(alpha.1)
  if((cond01+cond02+cond03+cond04) ==0 )
  {
    u <- 1:T1/T1
    phi.u <- beta.0+beta.1*u
    sigma.u <- alpha.0+alpha.1*u
    ind.d <- (  phi.u <= -1 | phi.u >= 1)
    ind.s <- (sigma.u <= 0)
    if (sum(ind.d) + sum(ind.s) == 0)
    {
      cc <- vector("numeric")
      for(j in 1:M) {
        phi <- beta.0 + beta.1 *((S * (j - 1))/T1 + (0.5 * N)/T1)
        sigma <- alpha.0 + alpha.1 * ((S * (j - 1))/T1 + (0.5 * N)/T1)
        i.ini <- S * (j - 1) + 1
        i.fin <- i.ini + N - 1
        series.j <- series[i.ini:i.fin]
        cc[j] <- whittle.taper.logshort(phi= phi, series = series.j, sigma = sigma)
      }
      loglik <- mean(cc)/(4*pi)
    }
    else
    {
      loglik = 100000000000.
    }

  }
  else
  {
    loglik = 100000000000.

  }
  return(loglik)
}

Sem.ruidlexp<-function(K,l=1,mu=0.5,n=1000,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  for(k in l:K){
    set.seed(k)
    zz<-rexp(n)
    u<-1:n/n
    e1<-sim.dat.lsar.2(n=n,alpha=alpha1, beta=beta1, z=(zz-mean(zz))/sd(zz)) ##z=zz)
    Y<-mu*u+e1
    model<-lm(Y~u-1)
    hat.beta<-model$coeff
    residuos<-model$residuals
    fit<-lsfn.whittle22(residuos, N=N, S=S, start = start1)
    fit<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
    fit
    phi<-fit[1] + fit[2]*u
    sigma<- fit[3] + fit[4]*u
    Trend<-hat.beta*u
    e<-Y-Trend
    ruido<-c()
    for(i in 2:n){
      ruido[i]<- (e[i]-(fit[1] + fit[2]*u[i])*(e[i-1]))*(fit[3] +fit[4]*u[i])^(-1)
    }
    ruido<-ruido[-1]
    ruido<-ruido-mean(ruido)
    ACF<-acf(ruido,plot = F)
    if(all(ACF$acf[-1]< 2/sqrt(n)& ACF$acf[-1]> -2/sqrt(n)))
    {
      return(k)
    }
  }
}

ruidoflexp<-function(s.seed=4,n=500,mu=0.5,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  set.seed(s.seed)
  u<-1:n/n
  zz<-rexp(n)
  e<-sim.dat.lsar.2(n=n,alpha=alpha1, beta=beta1, z=(zz-mean(zz))/sd(zz)) ##z=zz)
  Y<-mu*u+e
  model<-lm(Y~u-1)
  hat.beta<-model$coeff
  res2<-summary(model)
  hat.se<-res2$coefficients[2]
  residuos<-model$residuals
  fit<-lsfn.whittle2(residuos, N=N, S=S, start = start1)
  fit<-as.vector(as.vector(c(fit[[1]])))
  phi<-fit[1] + fit[2]*u
  sigma<- fit[3] +fit[4]*u
  Trend<-hat.beta*u
  e<-Y-Trend
  ruido<-c()
  for(i in 2:n){
    ruido[i]<- (e[i]-(fit[1] + fit[2]*u[i])*(e[i-1]))*(fit[3] + fit[4]*u[i])^(-1)
  }
  ruido<-ruido[-1]
  ruido<-ruido-mean(ruido)
  return(list(ruido=ruido,phi=phi,sigma=sigma,Trend=Trend,hat.beta=hat.beta,fit=fit,hat.se=hat.se))
}

Sem.ruidlunif<-function(K,l=1,mu=0.5,n=1000,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  for(k in l:K){
    set.seed(k)
    zz<-runif(n)
    u<-1:n/n
    e1<-sim.dat.lsar.2(n=n,alpha=alpha1, beta=beta1, z=(zz-mean(zz))/sd(zz)) ##z=zz)
    Y<-mu*u+e1
    model<-lm(Y~u-1)
    hat.beta<-model$coeff
    residuos<-model$residuals
    fit<-lsfn.whittle22(residuos, N=N, S=S, start = start1)
    fit<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
    fit
    phi<-fit[1] + fit[2]*u
    sigma<- fit[3] + fit[4]*u
    Trend<-hat.beta*u
    e<-Y-Trend
    ruido=c()
    for(i in 2:n){
      ruido[i]<- (e[i]-(fit[1] + fit[2]*u[i])*(e[i-1]))*(fit[3] +fit[4]*u[i])^(-1)
    }
    ruido<-ruido[-1]
    ruido<-ruido-mean(ruido)
    ACF<-acf(ruido,plot = F)
    if(all(ACF$acf[-1]< 2/sqrt(n)& ACF$acf[-1]> -2/sqrt(n)))
    {
      return(k)
    }
  }
}

ruidoflunif<-function(s.seed=4,n=500,mu=0.5,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  set.seed(s.seed)
  u<-1:n/n
  zz<-runif(n)
  e<-sim.dat.lsar.2(n=n,alpha=alpha1, beta=beta1, z=(zz-mean(zz))/sd(zz)) ##z=zz)
  Y<-mu*u+e
  model<-lm(Y~u-1)
  hat.beta<-model$coeff
  res2<-summary(model)
  hat.se<-res2$coefficients[2]
  residuos<-model$residuals
  fit<-lsfn.whittle2(residuos, N=N, S=S, start = start1)
  fit<-as.vector(as.vector(c(fit[[1]])))
  phi<-fit[1] + fit[2]*u
  sigma<- fit[3] +fit[4]*u
  Trend<-hat.beta*u
  e<-Y-Trend
  ruido<-c()
  for(i in 2:n){
    ruido[i]<- (e[i]-(fit[1] + fit[2]*u[i])*(e[i-1]))*(fit[3] + fit[4]*u[i])^(-1)
  }
  ruido<-ruido[-1]
  ruido<-ruido-mean(ruido)
  return(list(ruido=ruido,phi=phi,sigma=sigma,Trend=Trend,hat.beta=hat.beta,fit=fit,hat.se=hat.se))
}

Sem.ruidlnorm<-function(K,l=1,mu=0.5,n=1000,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  for(k in l:K){
    set.seed(k)
    zz<-rnorm(n)
    u<-1:n/n
    e1<-sim.dat.lsar.2(n=n,alpha=alpha1, beta=beta1, z=zz)
    Y<-mu*u+e1
    model<-lm(Y~u-1)
    hat.beta<-model$coeff
    residuos<-model$residuals
    fit<-lsfn.whittle22(residuos, N=N, S=S, start = start1)
    fit<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
    fit
    phi<-fit[1] + fit[2]*u
    sigma<- fit[3] + fit[4]*u
    Trend<-hat.beta*u
    e<-Y-Trend
    ruido<-c()
    for(i in 2:n){
      ruido[i]<- (e[i]-(fit[1] + fit[2]*u[i])*(e[i-1]))*(fit[3] +fit[4]*u[i])^(-1)
    }
    ruido<-ruido[-1]
    ruido<-ruido-mean(ruido)
    ACF<-acf(ruido,plot = F)

    if(all(ACF$acf[-1]< 2/sqrt(n)& ACF$acf[-1]> -2/sqrt(n)))
    {
      return(k)
    }
  }
}

ruidoflnorm<-function(s.seed=4,n=500,mu=0.5,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  set.seed(s.seed)
  u<-1:n/n
  zz<-rnorm(n)
  e<-sim.dat.lsar.2(n=n,alpha=alpha1, beta=beta1, z=zz)
  Y<-mu*u+e
  model<-lm(Y~u-1)
  hat.beta<-model$coeff
  res2<-summary(model)
  hat.se<-res2$coefficients[2]
  residuos<-model$residuals
  fit<-lsfn.whittle2(residuos, N=N, S=S, start = start1)
  fit<-as.vector(as.vector(c(fit[[1]])))
  phi<-fit[1] + fit[2]*u
  sigma<- fit[3] +fit[4]*u
  Trend<-hat.beta*u
  e<-Y-Trend
  ruido<-c()
  for(i in 2:n){
    ruido[i]<- (e[i]-(fit[1] + fit[2]*u[i])*(e[i-1]))*(fit[3] + fit[4]*u[i])^(-1)
  }
  ruido<-ruido[-1]
  ruido<-ruido-mean(ruido)
  return(list(ruido=ruido,phi=phi,sigma=sigma,Trend=Trend,hat.beta=hat.beta,fit=fit,hat.se=hat.se))
}

Sem.ruid<-function(K,l=1,mu=0.5,n=1000,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  for(k in l:K){
    set.seed(k)
    zz<-rnorm(n)
    u<-1:n/n
    e1<-sim.dat.lsar.short(n=n,alp=alpha1,bet=beta1, z=zz,w=2*pi)
    Y<-mu*u+e1
    model<-lm(Y~u-1)
    hat.beta<-model$coeff
    residuos<-model$residuals
    fit<-lsfn.whittle.short(residuos, N=N, S=S, start2 = start1)
    fit<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
    fit
    phi<-fit[1] + fit[2]*cos(2*pi*u)
    sigma<- fit[3] + fit[4]*u+ fit[5]*u*u
    Trend<-hat.beta*u
    e<-Y-Trend
    ruido=c()
    for(i in 2:n){
      ruido[i]<- (e[i]-(fit[1] + fit[2]*cos(2*pi*u[i]))*(e[i-1]))*(fit[3] +fit[4]*u[i]+ fit[5]*u[i]*u[i])^(-1)
    }
    ruido<-ruido[-1]
    ruido<-ruido-mean(ruido)
    ACF<-acf(ruido,plot = F)
    if(all(ACF$acf[-1]< 2/sqrt(n)& ACF$acf[-1]> -2/sqrt(n)))
    {
      return(k)
    }
  }
}

Sem.ruidexp<-function(K,l=1,mu=0.5,n=1000,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL){
  for(k in l:K){
    set.seed(k)
    u<-1:n/n
    zz<-rexp(n)
    e1<-sim.dat.lsar.short(n=n,alp=alpha1,bet=beta1,z=(zz-mean(zz))/sd(zz),w=2*pi)
    Y<-mu*u+e1
    model<-lm(Y~u-1)
    hat.beta<-model$coeff
    residuos<-c(model$residuals)
    fit<-lsfn.whittle.short(residuos, N=N, S=S, start2 = start1)
    fit<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
    fit
    phi<-fit[1] + fit[2]*cos(2*pi*u)
    sigma<- fit[3] + fit[4]*u+ fit[5]*u*u
    Trend<-hat.beta*u
    e<-Y-Trend
    ruido=c()
    for(i in 2:n){
      ruido[i]<- (residuos[i]-(fit[1] + fit[2]*cos(2*pi*u[i]))*(residuos[i-1]))*(fit[3] +fit[4]*u[i]+ fit[5]*u[i]*u[i])^(-1)
    }
    ruido<-ruido[-1]
    ruido<-ruido-mean(ruido)
    ACF<-acf(ruido,plot = T)
    ACF<-ACF$acf[-1]
    if(all(ACF< 2/sqrt(n)& ACF> -2/sqrt(n)))
    {
      return(k)
    }
  }
}

Sem.ruidunif<-function(K,l=1,mu=0.5,n=1000,N=60,S=40,alpha1,beta1,start1){
  for(k in l:K){
    set.seed(k)
    u<-1:n/n
    zz<-runif(n)
    e1<-sim.dat.lsar.short(n=n,alp=alpha1,bet=beta1, z=(zz-mean(zz))/sd(zz),w=2*pi)
    Y<-mu*u+e1
    model<-lm(Y~u-1)
    hat.beta<-model$coeff
    residuos<-c(model$residuals)
    fit<-lsfn.whittle.short(residuos, N=N, S=S, start2 = start1)
    fit<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
    fit
    phi<-fit[1] + fit[2]*cos(2*pi*u)
    sigma<- fit[3] + fit[4]*u+ fit[5]*u*u
    Trend<-hat.beta*u
    e<-Y-Trend
    ruido=c()
    for(i in 2:n){
      ruido[i]<- (residuos[i]-(fit[1] + fit[2]*cos(2*pi*u[i]))*(residuos[i-1]))*(fit[3] +fit[4]*u[i]+ fit[5]*u[i]*u[i])^(-1)
    }
    ruido<-ruido[-1]
    ruido<-ruido-mean(ruido)
    ACF<-acf(ruido,plot = F)
    ACF<-ACF$acf[-1]
    if(all(ACF< 2/sqrt(n)& ACF> -2/sqrt(n)))
    {
      acf(ruido,plot = F)
      return(k)
    }
  }
}

ruidof<-function(s.seed=4,n=500,mu=0.5,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL )
{
  set.seed(s.seed)
  u<-1:n/n
  zz<-rnorm(n)
  e<-sim.dat.lsar.short(n=n,alp=alpha1,bet=beta1, z=zz,w=2*pi)
  Y<-mu*u+e
  model<-lm(Y~u-1)
  hat.beta<-model$coeff
  res2<-summary(model)
  hat.se<-res2$coefficients[2]
  residuos<-model$residuals
  fit<-lsfn.whittle.short2(residuos, N=N, S=S, start = start1)
  fit<-as.vector(as.vector(c(fit[[1]])))
  phi<-fit[1] + fit[2]*cos(2*pi*u)
  sigma<- fit[3] +fit[4]*u+ fit[5]*u*u
  Trend<-hat.beta*u
  e<-Y-Trend
  ruido<-c()
  for(i in 2:n){
    ruido[i]<- (e[i]-(fit[1] + fit[2]*cos(2*pi*u[i]))*(e[i-1]))*(fit[3] + fit[4]*u[i]+ fit[5]*u[i]*u[i])^(-1)
  }
  ruido<-ruido[-1]
  ruido<-ruido-mean(ruido)
  return(list(ruido=ruido,phi=phi,sigma=sigma,Trend=Trend,hat.beta=hat.beta,fit=fit,hat.se=hat.se))
}

ruidofexp<-function(s.seed = 4,n=500,mu=0.5,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL )
{
  set.seed(s.seed)
  u<-1:n/n
  e<-sim.dat.lsar.short(n=n,alp=alpha1,bet=beta1,z=(rexp(n)-mean(rexp(n)))/sd(rexp(n)),w=2*pi)
  Y<-mu*u+e
  model<-lm(Y~u-1)
  hat.beta<-model$coeff
  res2<-summary(model)
  hat.se<-res2$coefficients[2]
  residuos<-model$residuals
  fit<-lsfn.whittle.short2(residuos, N=60, S=40, start = start1)
  fit<-as.vector(as.vector(c(fit[[1]])))
  phi<-fit[1] + fit[2]*cos(2*pi*u)
  sigma<- fit[3] +fit[4]*u+ fit[5]*u*u
  Trend<-hat.beta*u
  ruido<-c()
  for(i in 2:n){
    ruido[i]<- (residuos[i]-(fit[1] + fit[2]*cos(2*pi*u[i]))*(residuos[i-1]))*(fit[3] + fit[4]*u[i]+ fit[5]*u[i]*u[i])^(-1)
  }
  ruido<-ruido[-1]
  ruido<-ruido-mean(ruido)
  return(list(ruido=ruido,phi=phi,sigma=sigma,Trend=Trend,hat.beta=hat.beta,fit=fit,hat.se=hat.se))
}


ruidofunif<-function(s.seed = 4,n=500,mu=0.5,N=60,S=40,alpha1=NULL, beta1=NULL,start1=NULL )
{
  set.seed(s.seed)
  u<-1:n/n
  e<-sim.dat.lsar.short(n=n,alp=alpha1,bet=beta1,z=(runif(n)-mean(runif(n)))/sd(runif(n)),w=2*pi)
  Y<-mu*u+e
  model<-lm(Y~u-1)
  hat.beta<-model$coeff
  res2<-summary(model)
  hat.se<-res2$coefficients[2]
  residuos<-model$residuals
  fit<-lsfn.whittle.short2(residuos, N=60, S=40, start = start1)
  fit<-as.vector(as.vector(c(fit[[1]])))
  phi<-fit[1] + fit[2]*cos(2*pi*u)
  sigma<- fit[3] +fit[4]*u+ fit[5]*u*u
  Trend<-hat.beta*u
  ruido<-c()
  for(i in 2:n){
    ruido[i]<- (residuos[i]-(fit[1] + fit[2]*cos(2*pi*u[i]))*(residuos[i-1]))*(fit[3] + fit[4]*u[i]+ fit[5]*u[i]*u[i])^(-1)
  }
  ruido<-ruido[-1]
  ruido<-ruido-mean(ruido)
  return(list(ruido=ruido,phi=phi,sigma=sigma,Trend=Trend,hat.beta=hat.beta,fit=fit,hat.se=hat.se))
}

Boot01<-function(ruido,phi,sigma,Trend,hat.beta,NN,B,m,n,mu)
{
  u<-1:n/n
  beta_B<-matrix(NA, ncol = 1, nrow = B)
  for(k in 1:B){
    ruido_B<-c()
    REsB   <- sample(ruido, size=m+NN, replace = TRUE, prob = NULL)
    ruido_B[1]<-sum((phi[1:NN]^(0:(NN-1)))*REsB[1:NN])
    for(i in 2:n){
      ruido_B[i] <- phi[i] * ruido_B[i-1] + sigma[i]*REsB[i]
    }
    YB<-Trend + ruido_B
    model<-lm(YB~u-1)
    beta_B[k]<-model$coeff
  }
  return(beta_B)
}

Boot01t<-function(ruido,phi,sigma,Trend,hat.beta,NN,B,m,n,mu)
{
  u<-1:n/n
  beta_B<-matrix(NA, ncol = 1, nrow = B)
  sdbeta_B<-matrix(NA, ncol = 1, nrow = B)
  for(k in 1:B){
    ruido_B<-c()
    REsB <- sample(ruido, size=m+NN, replace = TRUE, prob = NULL)
    ruido_B[1]<-sum((phi[1:NN]^(0:(NN-1)))*REsB[1:NN])
    for(i in 2:n){
      ruido_B[i] <- phi[i] * ruido_B[i-1] + sigma[i]*REsB[i]
    }
    YB<-Trend + ruido_B
    model<-lm(YB~u-1)
    res<-summary(model)
    beta_B[k]<-model$coeff
    sdbeta_B[k]<-res$coefficients[2]
  }
  return(list(beta_B,sdbeta_B))
}

sim.dat.lsar.short <- function(n,y0=0,alp,bet,z,w){
  u <- (1:n)/n
  phi <-alp[1]+alp[2]*cos(w*u)
  sigma<-bet[1]+bet[2]*u+bet[3]*u*u
  y <- c()
  y[1] <- y0
  for(i in 2:n){
    y[i]<-phi[i]*y[i-1]+sigma[i]*z[i]
  }
  return(y)
}

sim.dat.lsar.2 <- function(n,y0=0,alpha,beta,z){
  u <- (1:n)/n
  phi <- alpha[1] + alpha[2]*u
  sigma<- beta[1] + beta[2]*u
  y   <- c()
  y[1]<-y0
  for(i in 2:n){
    y[i]<- phi[i] * y[i-1] + sigma[i]*z[i]
  }
  return(y)
}

var.ar.alpha_caso.short <- function(alpha3,Gamma,Subdivisions1=100){
  f <- function(u, alpha3, Gamma, Subdivisions = 100){
    z <- vector("numeric")
    for(k in 1:length(u)){
      z[k]<-(u[k]^2)*(((Gamma[1]+Gamma[2]*u[k]+Gamma[3]*u[k]*u[k])^2)/(1-(alpha3[1]+alpha3[2]*cos(2*pi*u[k]))^2))
    }
    z
  }
  M <- NULL
  M <- integrate(f,lower=0,upper=1,alpha=alpha3,Gamma=Gamma,subdivisions=Subdivisions1)$value
  (M)
}
