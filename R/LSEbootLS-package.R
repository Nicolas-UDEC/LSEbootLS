#' Bootstrap Methods for Regression Models with Locally Stationary Errors
#'
#' Implements bootstrap methods for linear regression models with errors following a time-varying process, focusing on approximating the distribution of the least-squares estimator for regression models with locally stationary errors. It enables the construction of bootstrap and classical confidence intervals for regression coefficients, leveraging intensive simulation studies and real data analysis. The methodology is based on the approach described in Ferreira et al. (2020), allowing errors to be locally approximated by stationary processes.
#'
#' @importFrom tibble tibble
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom doRNG %dorng% registerDoRNG
#' @importFrom snow clusterExport
#' @importFrom stats rnorm rexp runif quantile coef nlminb na.exclude lm fft sd acf integrate as.formula reformulate ecdf qnorm
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators icount
#' @import rlecuyer
#' @importFrom LSTS LS.kalman
#' @keywords internal
"_PACKAGE"

NULL
