#' Calculate the bootstrap LSE for a long memory model
#'
#' @description
#' Bootstrap procedure to approximate the sampling distribution of the LSE for
#' time series linear regression with errors following a Locally Stationary process.
#'
#
#' @param formula (type: formula) an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param data (type: data.frame) data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param N (type: numeric) sample size of each block.
#' @param S (type: numeric) shifting places from block to block. Observe that the number of blocks M is determined by the following formula \eqn{M = \left\lfloor \frac{T-N}{S} + 1 \right\rfloor}, where \eqn{\left\lfloor . \right\rfloor} takes a single numeric argument \code{x} and returns a numeric vector containing the integers formed by truncating the values in \code{x} toward \code{0}.
#' @param B (type: numeric) bootstrap replicates, 1 by default.
#' @param start	(type: numeric) numeric vector, initial values for parameters to run the model.
#' @param d.order (type: numeric) polynomial order, where d is the ARFIMA parameter.
#' @param s.order (type: numeric) polynomial order noise scale factor.
#' @param nr.cores (type: numeric) number of CPU cores to be used for parallel processing. 1 by default.
#' @param seed (type: numeric) random number generator seed to generate the bootstrap samples.
#'
#' @return
#' A list with the following elements:
#'
#' - \code{coeff}: A tibble of estimated model coefficients, including intercepts, regression coefficients (\eqn{\beta}), and coefficients of the \eqn{\delta} and \eqn{\alpha} polynomials. Contains columns for coefficient name, estimate, t-value and p-value.
#'
#' - \code{estimation}: A matrix of bootstrap replicates for regression coefficients (\eqn{\beta}).
#'
#' - \code{delta}: A matrix of bootstrap replicates for the \eqn{\delta} polynomial coefficients.
#'
#' - \code{alpha}: A matrix of bootstrap replicates for the \eqn{\alpha} polynomial coefficients.
#'
#' @references Ferreira G., Mateu J., Vilar J.A., Muñoz J. (2020). Bootstrapping regression models with locally stationary disturbances. TEST, 30, 341-363.
#'
#' @details
#' This function estimates the parameters in the linear regression model for \eqn{t = 1, ..., T},
#'
#' \deqn{Y_{t,T} = X_{t,T} \beta + \epsilon_{t,T},}
#'
#' where the error term \eqn{\epsilon_{t,T}} follows a Locally Stationary Autoregressive Fractionally Integrated Moving Average (LS-ARFIMA) structure, given by:
#'
#' \deqn{\epsilon_{t,T} =(1 - B)^{-d(u)} \sigma(u)\eta_t,}
#'
#' where u=t/T ∈ \[0,1\], \eqn{d(u)} represents the long-memory parameter, \eqn{\sigma(u)} is the noise scale factor, and \eqn{\{\eta_t\}} is a white noise sequence with zero mean and unit variance.
#'
#' Particularly, we model \eqn{d(u)} and \eqn{\sigma(u)} as polynomials of order \eqn{d.order} and \eqn{s.order} respectively.
#'
#' \deqn{d(u) = \sum_{i=0}^{d.order} \delta_i u^i,}
#' \deqn{\sigma(u) = \sum_{j=0}^{s.order} \alpha_j u^j,}
#'
#' For more details, see references.
#'
#' @examples
#' n    <- length(USinf)
#' shift<-201
#' u1<-c((1:shift)/shift,rep(0, n-shift))
#' u2<-c(rep(0, shift),(1:(n-shift))/(n-shift))
#' u<-(1:n)/n
#' switch <- c(rep(1,shift), rep(0, n-shift))
#' x1<-switch*u
#' x2<-(1-switch)*u
#'
#' test <- data.frame(USinf, x1=x1, x2=x2)
#'
#' application(formula=USinf~x1+x2,data=test,N=150,S=50,B=10,
#' start = c(0.16,2.0,-7,8,-3,0.25,-0.25,0.01),
#' d.order=4,s.order=2,nr.cores=1)
#'
#' @export

application<-function(formula,data,start,d.order,s.order,N,S,B=1,nr.cores=1,seed=123){
  checkInput(formula, data, start, d.order, s.order, N, S, B, nr.cores, seed)
  s.aux <- d.order+1+s.order+1
  set.seed(seed)
  data<-as.data.frame(data)
  n<-nrow(data)
  model<-lm(formula)
  res<-c(model$residuals)
  predictor_names<-names(model$model)[-1]
  fit2<-lsfn.whittle(res,N=N,S=S,start=start,d.order=d.order,s.order=s.aux)
  ruido<-LS.kalman(series=res,start=fit2,include.d=TRUE, d.order= d.order,sd.order=s.order)$residuals
  betas<-model$coefficients
  X<-as.matrix(data[,predictor_names,drop = FALSE])
  Trend<-(cbind(1,X))%*%betas
  f2<-reformulate(termlabels = predictor_names, response = "YB")

  cl <- makeCluster(nr.cores)
  registerDoParallel(cl)
  registerDoRNG(seed)

  results <- foreach(k = 1:B, .combine = 'rbind',.inorder=T) %dorng% {
    set.seed(seed)
    ruido_B    <- c()
    REsB       <- sample(ruido, size=n, replace = TRUE, prob = NULL)
    ruido_B    <- lsfn.innov.sim(n = n, beta = c(fit2[1:(d.order+1)]), alpha = c(fit2[(d.order+2):s.aux]) , z=REsB)
    YB         <- Trend + ruido_B
    df         <- data.frame(YB = YB, X)
    model      <- lm(f2,data=df)
    residuos   <- model$residuals
    fit2       <- lsfn.whittle(residuos, N=N, S=S, start = fit2, d.order = d.order, s.order = s.aux)

    as.vector(c(model$coeff,fit2))
  }

  stopCluster(cl)

  rownames(results) <- paste("boot", 1:nrow(results), sep ="")
  beta_B <-     matrix(NA, ncol = length(betas), nrow = B)
  delta  <-     matrix(NA, ncol = (d.order+1), nrow = B)
  alfa <- matrix(NA, ncol = (s.order+1), nrow = B)
  beta_B <- results[, 1:length(betas)]
  delta  <- results[, (length(betas) + 1):(length(betas) + 1 +d.order)]
  alfa <- results[,(length(betas) +d.order+2):ncol(results)]
  colnames(beta_B) <- c("intercept",predictor_names)
  colnames(delta) <-  c(paste(intToUtf8(0x03B4), 0:d.order, sep=""))
  colnames(alfa) <-   c(paste(intToUtf8(0x03B1), 0:s.order, sep=""))

  pvals <- sapply(1:nrow(t(results)),function(x) {
    distribution <- ecdf(t(results)[x,])
    qt0 <- distribution(0)
    if(qt0 < 0.5){
      return(2*qt0)
    } else {
      return(2*(1-qt0))
    }
  })

  resultados <- bootstrap.summary(c(colMeans(beta_B),colMeans(delta),colMeans(alfa)),
                                  c(test_t(beta_B),test_t(delta),test_t(alfa)),
                                  c(pvals))

  return(list(coeff = resultados,estimation = beta_B,delta = delta,alpha = alfa))
}

