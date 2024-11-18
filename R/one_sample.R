#' @title one_sample: it returns only one sample using a uniform Metropolist-Hastings Random-Walk algorithm
#'
#' @param data a data frame containing ISRCs, Registrant and variable defined in the x parameter
#' @param d the probability of selecting a non-neighbour at each step (default is 0.5)
#' @param size the desired sample size
#' @return a vector with one sample obtained from the variable specified by x samples and the acceptance rate of the MHRW algorithm
#'
#' @examples
#'
#' example <- one_sample("popularity", data = sample_data)
#' example
#'
#'
#' @export
one_sample <- function(data, d = 0.5, size = 100){
  #computing adjacency matrix
  A <- A_compute(data)
  #computing transition matrix
  Q <- Q_compute(A, d = d)
  #computing adjusted transition matrix for the jumps
  QMH <- Q_compute_MH(A, Q, d = d)
  sampleout <- samplerMH(A, QMH, size = size, d = d)
  return(sampleout)
}
