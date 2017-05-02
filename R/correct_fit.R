#' Fit a time series model with correct data size
#'
#' @param y data
#' @param p number of lags
#' @param k_max max lag length within the set of models
#'
#' @return predict residuals
#' @export
#'
#' @examples
correct_fit <- function(y,p,k_max) {
  T <- length(y)
  y_effective <- y[(k_max+1):T,1]
  data <- matrix(NaN,nrow = T-k_max,ncol = p+1)
  data[,1] = t(y_effective)
  index <- k_max
  index2 <- 2
  while (index2 <= (p+1) ) {
    data[,index2] = y[index:(T+1-index2),1]
    index = index - 1
    index2 = index2 + 1
  }
  X <- data[,2:(p+1)]
  Y <- data[,1]
  beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y
  predict_residual <- data[,1] - data[,2:(p+1)]%*%beta_hat
  return(predict_residual)
}
