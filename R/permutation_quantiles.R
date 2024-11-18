#' @title permutation_quantiles: this function performs the permutation test for the quantiles of two samples
#'
#' @param x1 a vector with the first sample
#' @param x2 a vector with the second sample
#' @param N number of resamples to approximate the permutation distribution under the null
#'
#' @return a list containing the permutation distribution, the observed difference in quantiles and the p value
#'
#'
#' @examples
#'
#' # This function performs the following test: H_0: quantiles are equal against H_1: not equal
#'
#' N <- 10000
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#'
#' permutation_quantiles(x1, x2, N)
#'
#' @export
permutation_quantiles <- function(x1, x2, quantiles = c(0.025, 0.975), N = 10000){
  dat <- c(x1, x2)
  output <- rep(0, N)
  n <- length(x1); m <- length(x2)
  #running the permutation resampling
  output <- replicate(N,{
    perm <- sample(n + m, replace = F)
    meanx1 <- quantile(dat[perm[1:n]], quantiles, na.rm = T)
    meanx2 <- quantile(dat[perm[(n+1):(n+m)]], quantiles, na.rm = T)
    meanx1 - meanx2
  })
  #P-value, we need one for each quantile
  obs_diff <- quantile(x1, quantiles, na.rm = T) - quantile(x2, quantiles, na.rm = T)
  pvalue <- sapply(1:length(quantiles), function(i) (1 + sum(output[i,] > obs_diff[i]))/(N+1))

  return(list(perm_dist = output,
              obs_diff = obs_diff,
              pvalue = pvalue))
}
