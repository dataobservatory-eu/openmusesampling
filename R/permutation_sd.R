#' @title permutation_sd: this function performs the permutation test for the standard deviation of two samples
#'
#' @param x1 a vector with the first sample
#' @param x2 a vector with the second sample
#' @param N number of resamples to approximate the permutation distribution under the null
#'
#' @return a list containing the permutation distribution, the observed ratio in sd's and the p value
#'
#'
#' @examples
#'
#' # This function performs the following test: H_0: sd's are equal against H_1: not equal
#'
#' N <- 10000
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#'
#' permutation_sd(x1, x2, N)
#'
#' @export
permutation_sd <- function(x1, x2, N = 10000){
  dat <- c(x1, x2)
  output <- rep(0, N)
  n <- length(x1); m <- length(x2)
  #running the permutation resampling
  output <- replicate(N,{
    perm <- sample(n + m, replace = F)
    sdx1 <- sd(dat[perm[1:n]], na.rm = T)
    sdx2 <- sd(dat[perm[(n+1):(n+m)]], na.rm = T)
    sdx1/sdx2
  })
  #P-value
  obs_diff <- sd(x1, na.rm = T)/sd(x2, na.rm = T)
  pvalue <- (1 + sum(output >= obs_diff))/(N+1)

  return(list(perm_dist = output,
              obs_ratio = obs_diff,
              pvalue = pvalue))
}
