#' criterion function for hv cross validation in Racine (2000), JoE
#'
#' @param y data
#' @param p number of lags
#' @param h
#' @param v size of validation set = 2v+1
#'
#' @return value of criterion function
#' @export
#'
#' @examples
cv_hv <- function(y,p,h,v) {
  T <- length(y)
  error <- matrix(NaN,nrow = T-p-2*v,ncol=1)
  y_effective <- y[(p+1):T,1]
  data <- matrix(NaN,nrow = T-p,ncol = p+1)
  data[,1] = t(y_effective)
  index <- p
  index2 <- 2
  while (index >=1 ) {
    data[,index2] = y[index:(T+1-index2),1]
    index = index - 1
    index2 = index2 + 1
  }
  for (ii in (v+1):(T-p-v)) {
    validation_set = data[(ii-v):(ii+v),]
    if (ii <= (h + v+ 1) ) {
      training_set = data[(ii+v+h+1):(T-p),]
    }
    else if (ii <= (T-p-v-h-1) ) {
      training_set <- matrix(NaN,nrow = T-p-2*v-2*h-1,ncol = p+1)
      training_set[(ii-1-v-h+1):(T-p-2*v-2*h-1),] = data[(ii+v+h+1):(T-p),]
      training_set[1:(ii-1-v-h),] = data[1:(ii-1-v-h),]
    }
    else {
      training_set = data[1:(ii-1-v-h),]
    }
    X <- training_set[,2:(p+1)]
    Y <- training_set[,1]
    beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y
    predict_residual <- validation_set[,1] - validation_set[,2:(p+1)]%*%beta_hat
    error[ii-v,1] = t(predict_residual)%*%predict_residual
  }
  criterion <- sum(error)/(T-p-2*v)/(2*v+1)
  return(criterion)
}
