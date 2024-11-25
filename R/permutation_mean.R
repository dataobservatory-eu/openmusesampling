#' @title permutation_mean: this function performs the permutation test for the mean of two samples
#'
#' @param x1 a vector with the first sample
#' @param x2 a vector with the second sample
#' @param N number of resamples to approximate the permutation distribution under the null
#'
#' @return a list containing the permutation distribution, the observed difference in means and the p value
#'
#'
#' @examples
#'
#' # This function performs the following test: H_0: mean(x1) = mean(x2) against H_1: mean(x1) different from mean(x2)
#'
#' N <- 10000
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#'
#' permutation_mean(x1, x2, N)
#'
#' @export
permutation_mean <- function(x1, x2, N = 10000){
  dat <- c(x1, x2)
  output <- rep(0, N)
  n <- length(x1); m <- length(x2)
  #running the permutation resampling
  output <- replicate(N,{
    perm <- sample(n + m, replace = F)
    meanx1 <- mean(dat[perm[1:n]], na.rm = T)
    meanx2 <- mean(dat[perm[(n+1):(n+m)]], na.rm = T)
    meanx1 - meanx2
  })
  #P-value
  obs_diff <- mean(x1, na.rm = T) - mean(x2, na.rm = T)
  pvalue <- (1 + sum(output > obs_diff))/(N+1)

  return(list(perm_dist = output,
              obs_diff = obs_diff,
              pvalue = pvalue))
}
