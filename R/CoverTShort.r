#' Calculate the coverage for several short-memory models
#'
#' Generates coverage metrics for a parameter of interest using a specified short-memory model.
#'
#' @param n (type: numeric) size of the simulated series.
#' @param R (type: numeric) number of realizations of the Monte Carlo experiments.
#' @param N (type: numeric) sample size of each block.
#' @param S (type: numeric) shifting places from block to block. Observe that the number of blocks M is determined by the following formula \eqn{M = \left\lfloor \frac{T-N}{S} + 1 \right\rfloor}, where \eqn{\left\lfloor . \right\rfloor} takes a single numeric argument \code{x} and returns a numeric vector containing the integers formed by truncating the values in \code{x} toward \code{0}.
#' @param mu (type: numeric) trend coefficient of the regression model.
#' @param dist (type: character) white noise distribution for calculating coverage, it includes the \code{"Gaussian"}, \code{"exponential"} and \code{"uniform"} univariate distributions.
#' @param method (type: character) methods are asymptotic (\code{"asym"}), bootstrap percentile (\code{"boot"}), bootstrap-t (\code{"boott"}) and bootstrap-SP (\code{"boot-SP"}).
#' @param alpha (type: numeric) numeric vector with values to simulate the time varying autoregressive parameters of model LSAR(1), \eqn{\phi(u)}.
#' @param beta (type: numeric) numeric vector with values to simulate the time varying scale factor parameters of model LSAR(1), \eqn{\sigma(u)}.
#' @param start (type: numeric) numeric vector, initial values for parameters to run the model.
#' @param Subdivisions (type: numeric) the number of subintervals produced in the subdivision (integration) process; only required in the asymptotic method.
#' @param B (type: numeric) the number of bootstrap replicates, NULL indicates the asymptotic method.
#' @param case (type: character) nonlinear (\code{"no-linear"}) and linear cases (\code{"linear"}).
#' @param m (type: numeric) parameter that allows to remove the first m observations when simulating the LSAR process.
#' @param NN (type: numeric) parameter that allows to remove the first NN observations of noise from the LSAR model.
#' @param sign nominal significance level
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item \code{n}: Size of each simulated series.
#'   \item \code{method}: Statistical method used for simulation.
#'   \item \code{coverage}: Proportion of true parameter values within the intervals.
#'   \item \code{avg_width}: Average width of the intervals.
#'   \item \code{sd_width}: Standard deviation of the interval widths.
#' }
#'
#' @details
#'
#' This function estimates the parameters in the linear regression model for \eqn{t = 1, ..., T},
#' \deqn{Y_{t,T} = X_{t,T} \beta + \epsilon_{t,T},}
#' where a locally stationary autoregressive process of order one (LSAR(1)) is described by the equation:
#' \deqn{\epsilon_{t,T} = \phi(u) \epsilon_{t-1,T} + \sigma(u) \eta_t}
#' where u=t/T ∈ \[0,1\], with
#' \eqn{\phi(u)} is the autoregressive coefficient which is modeled as a linear polynomial,
#' \eqn{\sigma(u)} is modeled as a quadratic polynomial, and {\eqn{\eta_t}} is a white noise sequence
#' with zero mean and unit variance.
#' This setup is referred to as a locally stationary autoregressive model (LSAR(1)).
#'
#' Resampling methods evaluated:
#'
#' - asym: Asymptotic method that uses the asymptotic variance of the estimator, based
#'   on the Central Limit Theorem, to construct confidence intervals under the
#'   assumption of normality in large samples.
#'
#' - boot: Standard bootstrap that generates replicas of the estimator \eqn{\hat{\beta}} by resampling
#'   the adjusted residuals \eqn{\hat{\epsilon}_t}. It approximates the distribution of the estimator by
#'   the variability observed in the bootstrap replicas of \eqn{\hat{\beta}}.
#'
#' - boot-t: Adjusted bootstrap that scales the bootstrap replicas of the estimator
#'   \eqn{\hat{\beta}} by its standard error, aiming to refine the precision of the confidence interval
#'   and adjust for the variability in the parameter estimation.
#'
#' - bootSP: Symmetrized Percentile-t method, a variation of the boot-t that symmetrizes the
#'   bootstrap distribution around zero to handle skewed distributions or outliers more effectively.
#'   This method enhances the accuracy of confidence intervals by adjusting for asymmetries in the
#'   bootstrap replicas.
#'
#' For more details, see references.
#'
#' @references Ferreira G., Mateu J., Vilar J.A., Muñoz J. (2020). Bootstrapping regression models with locally stationary disturbances. TEST, 30, 341-363.
#'
#' @examples
#' Coverageshortmemory(n=100,R=100,N=60,S=40,mu=0.5,dist="normal",method="asym",alpha=c(0.25,0.2),
#' beta=c(1,1,-0.5),start=c(0.15,0.15,1,1,-0.5),case="no-linear")
#'
#' @export

Coverageshortmemory<-function(n,R,N,S,mu,dist,method,alpha,beta,start,Subdivisions=100,m=10,NN=10,B,case,sign=1.64)
{
if(case=="no-linear"){
  if(method=="asym"){
    if(dist=="normal"){
    k<-Sem.ruid(K=10000,mu=mu,n=n,l=1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
    u<-1:n/n
    coverT<-rep(0, R)
    LI<-rep(0, R)
    LS<-rep(0, R)
    coverage<-rep(0, R)
    leng<-rep(0,R)
    for(i in 1:R){
      set.seed(k)
      zz<-rnorm(n)
      e<-sim.dat.lsar.short(n=n,alp=alpha,bet=beta,z=zz,w=2*pi)
      Y<-mu*u+e
      fit<-lsfn.whittle.short(Y,N=N,S=S,start2=start)
      fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
      VarCov<-(9/n)*(var.ar.alpha_caso.short(alpha3=c(fit2[1],fit2[2]),Gamma=c(fit2[3],fit2[4],fit2[5]),Subdivisions1=Subdivisions))
      LI[i]<-fit2[6] - sign*sqrt(VarCov)
      LS[i]<-fit2[6] + sign*sqrt(VarCov)
      coverage[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
      leng[i]<-(LS[i]-LI[i])
      s<-Sem.ruid(K=100000,mu=mu,n=n,l=k+1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
      k<-s
    }
    return(data.frame(n=n,method=method,distribution=dist,R=R,coverage=mean(coverage),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      u<-1:n/n
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      coverage<- rep(0, R)
      leng<-rep(0,R)
      for(i in 1:R){
        set.seed(k)
        zz<-rexp(n)
        e<-sim.dat.lsar.short(n=n,alp=alpha,bet=beta,z=(zz-mean(zz))/sd(zz),w=2*pi)
        Y<-mu*u+e
        fit<-lsfn.whittle.short(Y,N=N,S=S,start2=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<- (9/n)*(var.ar.alpha_caso.short(alpha3=c(fit2[1],fit2[2]),Gamma=c(fit2[3],fit2[4],fit2[5]),Subdivisions1=Subdivisions))
        LI[i]<-fit2[6] - sign*sqrt(VarCov)
        LS[i]<-fit2[6] + sign*sqrt(VarCov)
        coverage[i] <-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s <- Sem.ruidexp(K=100000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,coverage = mean(coverage),avg_width= mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      u<-1:n/n
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      coverage<- rep(0, R)
      leng<-rep(0,R)
      for(i in 1:R){
        set.seed(k)
        zz<-runif(n)
        e<-sim.dat.lsar.short(n=n,alp=alpha,bet=beta,z=(zz-mean(zz))/sd(zz),w=2*pi)
        Y<-mu*u+e
        fit<-lsfn.whittle.short(Y,N=N,S=S,start2=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<- (9/n)*(var.ar.alpha_caso.short(alpha3=c(fit2[1],fit2[2]), Gamma=c(fit2[3], fit2[4], fit2[5]),Subdivisions1=Subdivisions))
        LI[i]<-fit2[6] - sign*sqrt(VarCov)
        LS[i]<-fit2[6] + sign*sqrt(VarCov)
        coverage[i] <-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidunif(K=100000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R=R,coverage=mean(coverage),avg_width=mean(leng),sd_width=sd(leng)))
    }
}
  else if(method=="boot"){
    if(dist=="normal"){
    k<-Sem.ruid(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1=start,N=N,S=S)
    coverT<-rep(0, R)
    LI<-rep(0, R)
    LS<-rep(0, R)
    leng<-rep(0, R)
    for(i in 1:R){
      param<-ruidof(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha,beta1=beta,start1=start)
      bootbeta<-Boot01(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
      LI[i]<-quantile(bootbeta, probs=0.05)
      LS[i]<-quantile(bootbeta, probs=0.95)
      coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
      leng[i]<-(LS[i]-LI[i])
      s<-Sem.ruid(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
      k<-s
    }
    return(data.frame(n=n,method=method,distribution=dist,R=R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R){
        param<-ruidofexp(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha,beta1=beta,start1= start)
        bootbeta<-Boot01(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        LI[i]<-quantile(bootbeta, probs=0.05)
        LS[i]<-quantile(bootbeta, probs=0.95)
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R=R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R){
        param<-ruidofunif(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbeta<-Boot01(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        LI[i]<-quantile(bootbeta, probs=0.05)
        LS[i]<-quantile(bootbeta, probs=0.95)
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidunif(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R=R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
  }
  else if(method=="boott"){
    if(dist=="normal"){
    k<-Sem.ruid(K=10000,mu=mu,n=n,l=1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
    coverT<-rep(0, R)
    LI<-rep(0, R)
    LS<-rep(0, R)
    leng<-rep(0, R)
    for(i in 1:R)
    {
      param<-ruidof(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha,beta1=beta,start1= start)
      bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
      bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
      LI[i]<-param$hat.beta+quantile(bootbetat2, probs=0.05)*param$hat.se
      LS[i]<-param$hat.beta+quantile(bootbetat2, probs=0.95)*param$hat.se
      coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
      leng[i]<-(LS[i]-LI[i])
      s<-Sem.ruid(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      k<-s
    }
    return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidofexp(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        LI[i]<-param$hat.beta+quantile(bootbetat2, probs=0.05)*param$hat.se
        LS[i]<-param$hat.beta+quantile(bootbetat2, probs=0.95)*param$hat.se
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng), sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidofunif(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        LI[i]<-param$hat.beta+quantile(bootbetat2, probs=0.05)*param$hat.se
        LS[i]<-param$hat.beta+quantile(bootbetat2, probs=0.95)*param$hat.se
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
  }
  else if(method=="bootSP"){
    if(dist=="normal"){
    k<-Sem.ruid(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
    coverT<-rep(0, R)
    LI<-rep(0, R)
    LS<-rep(0, R)
    leng<-rep(0, R)
    for(i in 1:R)
    {
      param<-ruidof(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
      bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
      bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
      alfa<-0.05
      t_2alfa <- quantile(abs(bootbetat2), probs = 1 - 2*alfa)
      icBoot.t.sim <- param$hat.beta - c(t_2alfa, -t_2alfa)*param$hat.se
      coverT[i]<-((sum((icBoot.t.sim[1]<=mu & mu<= icBoot.t.sim[2])*1)))
      leng[i]<-(icBoot.t.sim[2]-icBoot.t.sim[1])
      s<-Sem.ruid(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      k<-s
    }
    return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidofexp(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        alfa<-0.05
        t_2alfa <- quantile(abs(bootbetat2), probs = 1 - 2*alfa)
        icBoot.t.sim <- param$hat.beta - c(t_2alfa, -t_2alfa)*param$hat.se
        coverT[i]<-((sum((icBoot.t.sim[1]<=mu & mu<= icBoot.t.sim[2])*1)))
        leng[i]<-(icBoot.t.sim[2]-icBoot.t.sim[1])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidofunif(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        alfa<-0.05
        t_2alfa <- quantile(abs(bootbetat2), probs = 1 - 2*alfa)
        icBoot.t.sim <- param$hat.beta - c(t_2alfa, -t_2alfa)*param$hat.se
        coverT[i]<-((sum((icBoot.t.sim[1]<=mu & mu<= icBoot.t.sim[2])*1)))
        leng[i]<-(icBoot.t.sim[2]-icBoot.t.sim[1])
        s<-Sem.ruidunif(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
}
     else{return("Error")}
}
else if(case=="linear")
  {
  if(method=="asym"){
    if(dist=="normal"){
      k<-Sem.ruidlnorm(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      u<-1:n/n
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      coverage<- rep(0, R)
      leng<-rep(0,R)
      for(i in 1:R){
        set.seed(k)
        zz<-rnorm(n)
        e<-sim.dat.lsar.short(n=n,alp=alpha,bet=beta, z=zz,w=2*pi)
        Y<-mu*u+e
        fit<-lsfn.whittle.short(Y,N=N,S=S,start2=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<- (9/n)*(var.ar.alpha_caso.short(alpha3=c(fit2[1],fit2[2]),Gamma=c(fit2[3],fit2[4],fit2[5]),Subdivisions1=Subdivisions))
        LI[i]<-fit2[6] - sign*sqrt(VarCov)
        LS[i]<-fit2[6] + sign*sqrt(VarCov)
        coverage[i] <-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruid(K=100000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1=start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,coverage = mean(coverage),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidlexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      u<-1:n/n
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      coverage<- rep(0, R)
      leng<-rep(0,R)
      for(i in 1:R){
        set.seed(k)
        zz<-rexp(n)
        e<-sim.dat.lsar.short(n=n,alp=alpha,bet=beta,z=(zz-mean(zz))/sd(zz),w=2*pi)
        Y<-mu*u+e
        fit<-lsfn.whittle.short(Y,N=N,S=S,start2=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<- (9/n)*(var.ar.alpha_caso.short(alpha3=c(fit2[1],fit2[2]),Gamma=c(fit2[3],fit2[4],fit2[5]),Subdivisions1=Subdivisions))
        LI[i]<-fit2[6] - sign*sqrt(VarCov)
        LS[i]<-fit2[6] + sign*sqrt(VarCov)
        coverage[i] <-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=100000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,coverage = mean(coverage),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidlunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      u<-1:n/n
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      coverage<- rep(0, R)
      leng<-rep(0,R)
      for(i in 1:R){
        set.seed(k)
        zz<-runif(n)
        e<-sim.dat.lsar.short(n=n,alp=alpha,bet=beta, z=(zz-mean(zz))/sd(zz),w=2*pi)
        Y<-mu*u+e
        fit<-lsfn.whittle.short(Y,N=N,S=S,start2=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<- (9/n)*(var.ar.alpha_caso.short(alpha3=c(fit2[1],fit2[2]), Gamma=c(fit2[3], fit2[4], fit2[5]),Subdivisions1=Subdivisions))
        LI[i]<-fit2[6] - sign*sqrt(VarCov)
        LS[i]<-fit2[6] + sign*sqrt(VarCov)
        coverage[i] <-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidunif(K=100000,mu=mu,n=n,l=k+1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R=R,coverage=mean(coverage),avg_width=mean(leng),sd_width=sd(leng)))
    }
  }
  else if(method=="boot"){
    if(dist=="normal"){
      k<-Sem.ruidlnorm(K=10000,mu=mu,n=n,l=1,alpha1=alpha,beta1=beta,start1=start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflnorm(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1=start)
        bootbeta<-Boot01(ruido=param$`ruido`,phi=param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        LI[i]<-quantile(bootbeta, probs=0.05)
        LS[i]<-quantile(bootbeta, probs=0.95)
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruid(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1=start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method=method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidlexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflexp(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbeta<-Boot01(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        LI[i]<-quantile(bootbeta, probs=0.05)
        LS[i]<-quantile(bootbeta, probs=0.95)
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidlunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflunif(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbeta<-Boot01(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        LI[i]<-quantile(bootbeta, probs=0.05)
        LS[i]<-quantile(bootbeta, probs=0.95)
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidunif(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R=R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
  }

  else if(method=="boott"){
    if(dist=="normal"){
      k<-Sem.ruidlnorm(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflnorm(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        LI[i]<-param$hat.beta+quantile(bootbetat2, probs=0.05)*param$hat.se
        LS[i]<-param$hat.beta+quantile(bootbetat2, probs=0.95)*param$hat.se
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruid(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidlexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflexp(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        LI[i]<-param$hat.beta+quantile(bootbetat2, probs=0.05)*param$hat.se
        LS[i]<-param$hat.beta+quantile(bootbetat2, probs=0.95)*param$hat.se
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidlunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflunif(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        LI[i]<-param$hat.beta+quantile(bootbetat2, probs=0.05)*param$hat.se
        LS[i]<-param$hat.beta+quantile(bootbetat2, probs=0.95)*param$hat.se
        coverT[i]<-((sum((LI[i]<=mu & mu<= LS[i])*1)))
        leng[i]<-(LS[i]-LI[i])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n = n,method = method,distribution=dist,R = R,B=B,coverage = mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
  }
  else if(method=="bootSP"){
    if(dist=="normal"){
      k<-Sem.ruidlnorm(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflnorm(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        alfa<-0.05
        t_2alfa <- quantile(abs(bootbetat2), probs = 1 - 2*alfa)
        icBoot.t.sim <- param$hat.beta - c(t_2alfa, -t_2alfa)*param$hat.se
        coverT[i]<-((sum((icBoot.t.sim[1]<=mu & mu<= icBoot.t.sim[2])*1)))
        leng[i]<-(icBoot.t.sim[2]-icBoot.t.sim[1])
        s<-Sem.ruid(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R = R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="exponential"){
      k<-Sem.ruidlexp(K=10000,mu=mu,n=n,l=1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflexp(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        alfa<-0.05
        t_2alfa <- quantile(abs(bootbetat2), probs = 1 - 2*alfa)
        icBoot.t.sim <- param$hat.beta - c(t_2alfa, -t_2alfa)*param$hat.se
        coverT[i]<-((sum((icBoot.t.sim[1]<=mu & mu<= icBoot.t.sim[2])*1)))
        leng[i]<-(icBoot.t.sim[2]-icBoot.t.sim[1])
        s<-Sem.ruidexp(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha,beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R=R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
    else if(dist=="uniform"){
      k<-Sem.ruidlunif(K=10000,mu=mu,n=n,l=1,alpha1=alpha,beta1=beta,start1 = start,N=N,S=S)
      coverT<-rep(0, R)
      LI<-rep(0, R)
      LS<-rep(0, R)
      leng<-rep(0, R)
      for(i in 1:R)
      {
        param<-ruidoflunif(s.seed=k,n=n,mu=mu,N=N,S=S,alpha1=alpha, beta1=beta,start1= start)
        bootbetat<-Boot01t(ruido=param$`ruido`,phi= param$phi,sigma=param$sigma,Trend=param$Trend,NN=NN,B=B,m=m,n=n)
        bootbetat2<-as.matrix(((as.matrix(bootbetat[[1]])-param$hat.beta))/as.matrix(bootbetat[[2]]))
        alfa<-0.05
        t_2alfa <- quantile(abs(bootbetat2), probs = 1 - 2*alfa)
        icBoot.t.sim <- param$hat.beta - c(t_2alfa, -t_2alfa)*param$hat.se
        coverT[i]<-((sum((icBoot.t.sim[1]<=mu & mu<= icBoot.t.sim[2])*1)))
        leng[i]<-(icBoot.t.sim[2]-icBoot.t.sim[1])
        s<-Sem.ruidunif(K=10000,mu=mu,n=n,l=k+1,alpha1=alpha, beta1=beta,start1 = start,N=N,S=S)
        k<-s
      }
      return(data.frame(n=n,method=method,distribution=dist,R=R,B=B,coverage=mean(coverT),avg_width=mean(leng),sd_width=sd(leng)))
    }
  }
  else{return("Error")}
}
  }

