#' @title Q_compute: constructs the transition matrix Q
#' 
#' @param A an adjacency matrix 
#' @param d the probability to sample a non conected node in the random walk
#' 
#' @return returns the transition matrix of the Markov chain
#' 
#' @export
Q_compute <- function(A, d = 0.5){
  N <- nrow(A) #number of nodes
  q <- A
  A_i <- rowSums(A)
  for(i in 1:nrow(A)){
    if(A_i[i] > 0){
      q[i,] <- (1 - d)/N + d * A[i,]/sum(A_i[i])
    } else {
      q[i,] <- 1/N
    }
  }
  #diag(q) <- 0
  return(q)
}