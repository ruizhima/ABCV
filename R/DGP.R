#' Data Generating Process: AR(1) with std normal error
#'
#' @param sample_size sample size
#' @param lag1 AR coefficient
#' @param y0 initial value of y
#'
#' @return y of length sample_size
#' @export
#'
#' @examples
dgp1 <- function(sample_size, lag1, y0) {
  T = sample_size;
  y <- matrix(NaN,sample_size,1)
  epsilon <- rnorm(T,0,1);
  for (ii in 1:T) {
    if (ii ==1 ) {
      y[1,1] = lag1*y0 + epsilon[ii];
    }
    else {
      y[ii,1] <- lag1*y[ii-1,1] + epsilon[ii]
    }
  }
  return(y)
}
