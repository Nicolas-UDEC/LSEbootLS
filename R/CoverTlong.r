#' Calculate the coverage of several long-memory models
#'
#' Generates coverage metrics for a parameter of interest using a specified long-memory model.
#'
#' @param n (type: numeric) size of the simulated series.
#' @param R (type: numeric) number of realizations of the Monte Carlo experiments.
#' @param N (type: numeric) sample size of each block.
#' @param S (type: numeric) shifting places from block to block. Observe that the number of blocks M is determined by the following formula \eqn{M=\left\lfloor \frac{T-N}{S} + 1 \right\rfloor}, where \eqn{\left\lfloor . \right\rfloor} takes a single numeric argument \code{x} and returns a numeric vector containing the integers formed by truncating the values in \code{x} toward \code{0}.
#' @param mu (type: numeric) trend coefficient of the regression model.
#' @param alpha (type: numeric) numeric vector with values to simulate the time varying autoregressive parameters of model LSAR(1), \eqn{\phi(u)}.
#' @param beta (type: numeric) numeric vector with values to simulate the time varying scale factor parameters of model LSAR(1), \eqn{\sigma(u)}.
#' @param start (type: numeric) numeric vector, initial values for parameters to run the model.
#' @param dist (type: character) white noise distribution for calculating coverage, it includes the \code{"normal"}, \code{"exponential"} and \code{"uniform"} univariate distributions.
#' @param method (type: character) methods are asymptotic (\code{"asym"}), bootstrap percentile (\code{"boot"}) and bootstrap-t (\code{"boott"}).
#' @param B (type: numeric) the number of bootstrap replicates, NULL indicates the asymptotic method.
#' @param seed (type: numeric) random number generator seed to generate the bootstrap samples.
#' @param nr.cores (type: numeric) number of CPU cores to be used for parallel processing. 1 by default.
#' @param sign nominal significance level
#'
#' @details
#'
#' This function estimates the parameters in the linear regression model for \eqn{t = 1, ..., T},
#' \deqn{Y_{t,T} = X_{t,T} \beta + \epsilon_{t,T},}
#' where a locally stationary fractional noise process (LSFN) is described by the equation:
#' \deqn{\epsilon_{t,T} = \sum_{j=0}^\infty \psi_j(u) \eta_{t-j}}
#' for u=t/T in \[0,1\], where \eqn{\psi_j(u) = \frac{\Gamma[j + d(u)]}{\Gamma[j+1] \Gamma[d(u)]}} and \eqn{d(u)} is the
#' smoothly varying long-memory coefficient. This model is referred to as locally stationary fractional noise (LSFN).
#'
#' In this particular case, \eqn{d(u)} is modeled as a linear polynomial, and \eqn{\sigma(u)} as a quadratic polynomial.
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
#' - boott: Adjusted bootstrap that scales the bootstrap replicas of the estimator
#'   \eqn{\hat{\beta}} by its standard error, aiming to refine the precision of the confidence interval
#'   and adjust for the variability in the parameter estimation.
#'
#' For more details, see references.
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
#' @references Ferreira G., Mateu J., Vilar J.A., Muñoz J. (2020). Bootstrapping regression models with locally stationary disturbances. TEST, 30, 341-363.
#'
#' @examples
#' Coveragelongmemory(n=500,R=100,N=60,S=40,mu=0.5,dist="normal",method="asym",
#' beta=c(0.1,-2),alpha=c(0.15,0.25, 0.1),start = c(0.1,-2,0.15,0.2, 0.1))
#'
#' @export
#'
Coveragelongmemory<-function(n,R,N,S,mu=0,dist,method,B=NULL,nr.cores=1,seed=123,alpha,beta,start,sign=0.05){
  if(method=="asym"){
    sign <- qnorm(1-sign/2)
    u<-1:n/n
    coverT<-rep(0,R)
    LI<-rep(0,R)
    LS<-rep(0,R)
    coverage<-rep(0,R)
    leng<-rep(0,R)
    trials<-R
    if(dist=="normal"){
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      set.seed(seed)
      covert0<-foreach(icount(trials) ,.combine=rbind,.inorder=T)%dorng%{
        z<-rnorm(n)
        e<-lsfn.innov.long(n=n,beta=beta,alpha=alpha,z=z)
        Y<-mu*u+e
        fit<-lsfn.long(Y,N=N,S=S,start=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<-abs(VarAsintotica(n, param=c(fit2[1],fit2[2])))
        LI<-fit2[6] - sign*sqrt(VarCov)
        LS<-fit2[6] + sign*sqrt(VarCov)
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert02<-as.matrix.data.frame(covert0)
      return(data.frame(n = n,method = method,R = R,coverage = mean(covert02[,1]),avg_width= mean(covert02[,2]),sd_width= sd(covert02[,2])))
    }
    else if(dist=="exponential"){
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      set.seed(seed)
      covert00<-foreach(icount(trials) ,.combine=rbind,.inorder=T)%dorng%{
        zz<-rexp(n)
        e<-lsfn.innov.long(n = n, beta=beta, alpha = alpha, z=(zz-mean(zz))/sd(zz))
        Y<-mu*u+e
        fit<-lsfn.long(Y,N=N,S=S,start=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<-abs(VarAsintotica(n, param=c(fit2[1],fit2[2])))
        LI<-fit2[6] - sign*sqrt(VarCov)
        LS<-fit2[6] + sign*sqrt(VarCov)
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert02<-as.matrix.data.frame(covert00)
      return(data.frame(n = n,method = method,R = R,coverage = mean(covert02[,1]),avg_width= mean(covert02[,2]),sd_width= sd(covert02[,2])))
    }
    else if(dist=='uniform'){
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      set.seed(seed)
      covert000<-foreach(icount(trials) ,.combine=rbind,.inorder=T)%dorng%{
        zz<-runif(n)
        e<-lsfn.innov.long(n=n,beta=beta,alpha=alpha,z=(zz-mean(zz))/sd(zz))
        Y<-mu*u+e
        fit<-lsfn.long(Y,N=N,S=S,start=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        VarCov<-abs(VarAsintotica(n, param=c(fit2[1],fit2[2])))
        LI<-fit2[6] - sign*sqrt(VarCov)
        LS<-fit2[6] + sign*sqrt(VarCov)
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert02<-as.matrix.data.frame(covert000)
      return(data.frame(n = n,method = method,R = R,coverage = mean(covert02[,1]),avg_width = mean(covert02[,2]),sd_width= sd(covert02[,2])))
    }
  }
  else if(method=="boot"){
    u=1:n/n
    h=u
    beta_B<-NULL
    coverage<- rep(0, R)
    leng<-rep(0, R)
    trials <- R
    if(dist == "normal"){
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      registerDoRNG(seed)
      covert<-foreach(icount(trials),.combine=rbind) %dopar% {
        z<-rnorm(n)
        e<-lsfn.innov.long(n=n,beta=beta,alpha=alpha,z=z)
        Y<-mu*u+e
        fit<-lsfn.long(Y,N=N,S=S,start=start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        ruido<-lsfn.kalman.filter_reg(param=fit2, Y, h, m=10)$res.stand
        for(k in 1:B){
          ruido_B<-c()
          set.seed(k)
          REsB<-sample(ruido, size=n, replace = TRUE, prob = NULL)
          ruido_B<-lsfn.innov.long(n = n, beta = c(fit2[1],fit2[2]), alpha = c(fit2[3],fit2[4],fit2[5]), z=REsB)
          YB<-fit2[6]*u + ruido_B
          model<-lm(YB~u-1)
          beta_B[k]<-model$coeff
        }
        LI <- quantile(beta_B, probs = sign / 2)
        LS <- quantile(beta_B, probs = 1 - sign / 2)
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert03<-as.matrix.data.frame(covert)
      return(data.frame(n=n,method=method,R=R,B=B,coverage=mean(covert03[,1]),avg_width=mean(covert03[,2]),sd_width=sd(covert03[,2])))
    }
    else if(dist == "exponential")
    {
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      registerDoRNG(seed)
      covert11<-foreach(icount(trials),.combine=rbind) %dopar% {
        z<-rexp(n)
        e<-lsfn.innov.long(n=n,beta=beta,alpha=alpha, z=(z-mean(z))/sd(z))
        Y<-mu*u+e
        fit<-lsfn.long(Y, N=N, S=S, start =start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        ruido<-lsfn.kalman.filter_reg(param=fit2, Y, h, m=10)$res.stand
        for(k in 1:B){
          ruido_B<-c()
          set.seed(k)
          REsB<-sample(ruido,size=n,replace=TRUE,prob=NULL)
          ruido_B<-lsfn.innov.long(n = n, beta = c(fit2[1],fit2[2]), alpha = c(fit2[3],fit2[4],fit2[5]), z=REsB )
          YB<-fit2[6]*u + ruido_B
          model<-lm(YB~u-1)
          beta_B[k]<-model$coeff
        }
        LI <- quantile(beta_B, probs = sign / 2)
        LS <- quantile(beta_B, probs = 1 - sign / 2)
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert03<-as.matrix.data.frame(covert11)
      return(data.frame(n=n,method=method,R=R,B=B,coverage = mean(covert03[,1]),avg_width= mean(covert03[,2]),sd_width= sd(covert03[,2])))
    }
    else if(dist == "uniform")
    {
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      registerDoRNG(seed)
      covert111<-foreach(icount(trials),.combine=rbind) %dopar% {
        z<-runif(n)
        e<-lsfn.innov.long(n = n, beta=beta, alpha =alpha,z=(z-mean(z))/sd(z))
        Y<-mu*u+e
        fit<-lsfn.long(Y, N=N, S=S, start =start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        ruido<-lsfn.kalman.filter_reg(param=fit2, Y, h, m=10)$res.stand
        for(k in 1:B){
          ruido_B<-c()
          set.seed(k)
          REsB<-sample(ruido, size=n, replace = TRUE, prob = NULL)
          ruido_B<-lsfn.innov.long(n = n, beta = c(fit2[1],fit2[2]), alpha = c(fit2[3],fit2[4],fit2[5]), z=REsB )
          YB<-fit2[6]*u + ruido_B
          model<-lm(YB~u-1)
          beta_B[k]<-model$coeff
        }
        LI <- quantile(beta_B, probs = sign / 2)
        LS <- quantile(beta_B, probs = 1 - sign / 2)
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert03<-as.matrix.data.frame(covert111)
      return(data.frame(n = n,method = method,R = R,B=B,coverage = mean(covert03[,1]),avg_width= mean(covert03[,2]),sd_width= sd(covert03[,2])))
    }
  }
  else if(method=="boott"){
    u<-1:n/n
    h<-u
    beta_B<-NULL
    sdbeta_B<-NULL
    coverage<- rep(0, R)
    leng<-rep(0, R)
    trials<-R
    if(dist == "normal"){
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      registerDoRNG(seed)
      covert2<-foreach(icount(trials),.combine=rbind) %dopar% {
        z<-rnorm(n)
        e<-lsfn.innov.long(n = n, beta = beta, alpha =alpha, z=z)
        Y<-mu*u+e
        model<-lm(Y~u-1)
        hat.beta<-model$coeff
        res2<-summary(model)
        hat.se<-res2$coefficients[2]
        fit<-lsfn.long(Y, N=N, S=S, start =start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        ruido<-lsfn.kalman.filter_reg(param=fit2, Y, h, m=10)$res.stand
        for(k in 1:B){
          ruido_B<-c()
          set.seed(k)
          REsB<-sample(ruido, size=n, replace = TRUE, prob = NULL)
          ruido_B<-lsfn.innov.long(n = n, beta = c(fit2[1],fit2[2]), alpha = c(fit2[3],fit2[4],fit2[5]), z=REsB )
          YB<-fit2[6]*u + ruido_B
          model<-lm(YB~u-1)
          beta_B[k]<-model$coeff
          reg<-summary(model)
          sdbeta_B[k]<-reg$coefficients[2]
        }
        bootbetat2<-as.matrix(((as.matrix(beta_B)-hat.beta))/as.matrix(sdbeta_B))
        LI <- hat.beta + quantile(bootbetat2, probs = alpha / 2) * hat.se
        LS <- hat.beta + quantile(bootbetat2, probs = 1 - alpha / 2) * hat.se
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert04<-as.matrix.data.frame(covert2)
      return(data.frame(n = n,method = method,R = R,B=B,coverage = mean(covert04[,1]),avg_width= mean(covert04[,2]),sd_width= sd(covert04[,2])))
    }
    else if(dist == "exponential")
    {
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      registerDoRNG(seed)
      covert22<-foreach(icount(trials),.combine=rbind) %dopar% {
        z<-rexp(n)
        e<-lsfn.innov.long(n = n, beta = beta, alpha =alpha, z=(z-mean(z))/sd(z))
        Y<-mu*u+e
        model<-lm(Y~u-1)
        hat.beta<-model$coeff
        res2<-summary(model)
        hat.se<-res2$coefficients[2]
        fit<-lsfn.long(Y, N=N, S=S, start =start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        ruido<-lsfn.kalman.filter_reg(param=fit2, Y, h, m=10)$res.stand
        for(k in 1:B){
          ruido_B<-c()
          set.seed(k)
          REsB<-sample(ruido, size=n, replace = TRUE, prob = NULL)
          ruido_B<-lsfn.innov.long(n = n, beta = c(fit2[1],fit2[2]), alpha = c(fit2[3],fit2[4],fit2[5]), z=REsB )
          YB<-fit2[6]*u + ruido_B
          model<-lm(YB~u-1)
          beta_B[k]<-model$coeff
          reg<-summary(model)
          sdbeta_B[k]<-reg$coefficients[2]
        }
        bootbetat2<-as.matrix(((as.matrix(beta_B)-hat.beta))/as.matrix(sdbeta_B))
        LI <- hat.beta + quantile(bootbetat2, probs = alpha / 2) * hat.se
        LS <- hat.beta + quantile(bootbetat2, probs = 1 - alpha / 2) * hat.se
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert04<-as.matrix.data.frame(covert22)
      return(data.frame(n = n,method = method,R = R,B=B,coverage = mean(covert04[,1]),avg_width= mean(covert04[,2]),sd_width= sd(covert04[,2])))
    }
    else if(dist == "uniform")
    {
      cl = makeCluster(nr.cores)
      registerDoParallel(cl)
      registerDoRNG(seed)
      covert222<-foreach(icount(trials),.combine=rbind) %dopar% {
        z<-runif(n)
        e<-lsfn.innov.long(n = n, beta =beta,alpha=alpha, z=(z-mean(z))/sd(z))
        Y<-mu*u+e
        model<-lm(Y~u-1)
        hat.beta<-model$coeff
        res2<-summary(model)
        hat.se<-res2$coefficients[2]
        fit<-lsfn.long(Y, N=N, S=S, start =start)
        fit2<-as.vector(as.vector(c(fit[[1]],fit[[2]])))
        ruido<-lsfn.kalman.filter_reg(param=fit2, Y, h, m=10)$res.stand
        for(k in 1:B){
          ruido_B<-c()
          set.seed(k)
          REsB<-sample(ruido, size=n, replace = TRUE, prob = NULL)
          ruido_B<-lsfn.innov.long(n = n, beta = c(fit2[1],fit2[2]), alpha = c(fit2[3],fit2[4],fit2[5]), z=REsB )
          YB<-fit2[6]*u + ruido_B
          model<-lm(YB~u-1)
          beta_B[k]<-model$coeff
          reg<-summary(model)
          sdbeta_B[k]<-reg$coefficients[2]
        }
        bootbetat2<-as.matrix(((as.matrix(beta_B)-hat.beta))/as.matrix(sdbeta_B))
        LI <- hat.beta + quantile(bootbetat2, probs = alpha / 2) * hat.se
        LS <- hat.beta + quantile(bootbetat2, probs = 1 - alpha / 2) * hat.se
        list(((as.numeric((LI<=mu & mu<= LS)))),(LS-LI))
      }
      stopCluster(cl)
      covert04<-as.matrix.data.frame(covert222)
      return(data.frame(n = n,method=method,R=R,B=B,coverage=mean(covert04[,1]),avg_width=mean(covert04[,2]),sd_width= sd(covert04[,2])))
    }
  }
  else{
    return(stop("Error"))
  }
}
