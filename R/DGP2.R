#' Data Generating Process: MA(1) with std normal error
#'
#' @param sample_size sample size
#' @param lag1 MA coefficient
#' @param e0 initial value of epsilon
#'
#' @return y of length sample_size
#' @export
#'
#' @examples
dgp2 <- function(sample_size, lag1, e0) {
  T = sample_size;
  y <- matrix(NaN,sample_size,1)
  epsilon <- rnorm(T,0,1);
  for (ii in 1:T) {
    if (ii == 1 ) {
      y[1,1] = lag1*e0 + epsilon[ii];
    }
    else {
      y[ii,1] <- lag1*epsilon[ii-1] + epsilon[ii]
    }
  }
  return(y)
}
