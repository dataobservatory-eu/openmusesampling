#' @title sampling: function to generate sample mean statistics using a uniform Metropolist-Hastings Random-Walk algorithm
#'
#' @param x a string containing the name of the variable you wish to compute the mean
#' @param data a data frame containing ISRCs, Registrant and variable defined in the x parameter
#' @param B number of samples to construct the sampling distribution (default is 1000)
#' @param d the probability of selecting a non-neighbour at each step (default is 0.5)
#' @param size the desired sample size
#' @return a data frame with the sample mean calculated from B samples and the acceptance rate of the MHRW algorithm
#'
#' @examples
#'
#' example <- openmusesampling("popularity", data = sample_data)
#' example
#'
#' The sample mean calculated above should be close to the true sample mean and this true mean should be contained in the 95% CI
#' mean(sample_data$popularity)
#'
#' @export
openmusesampling <- function(x, data, B = 1000, d = 0.5, size = 100){
  #computing adjacency matrix
  A <- A_compute(data)
  #computing transition matrix
  Q <- Q_compute(A, d = d)
  #computing adjusted transition matrix for the jumps
  QMH <- Q_compute_MH(A, Q, d = d)

  samp <- replicate(B,{
    samples <- samplerMH(A, QMH, size = size, d = d)
    c(
      mean(data[,which(names(data) == x)][unique(samples$sample)]),
      samples$acceptance_rate
    )
  })

  samp <- data.frame(t(samp))
  names(samp) <- c(paste0("sample_mean_", x), "acceptance_rate")
  output <- list(mean = mean(samp[,1]),
                 acceptance_rate = mean(samp[,2]),
                 CI = stats::quantile(samp[,1], probs = c(0.025, 0.975)),
                 samples = samp,
                 variable = x,
                 B = B,
                 size = size)
  attr(output, "class") <- "openmusesampling"
  return(output)
}
