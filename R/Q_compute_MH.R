#' @title Q_compute_MH: computes the adjusted transition matrix for the Random Walk with jumps
#' 
#' @param A the network adjacency matrix
#' @param Q the network transition matrix
#' @param d the probability to jump to a non connected node
#' @return returns the adjusted transition matrix
#' 
#' @export
Q_compute_MH <- function(A, Q, d = 0.5){
  N <- nrow(A) #number of nodes
  q <- A
  A_i <- rowSums(A) #computed the node degrees
  alpha <- matrix(0, N, N)
  for(i in 1:N){
    for(j in 1:N){
      if(i == j){next}
      if(A[i,j] == 1){
        alpha[i,j] <- min(((1 - d)/N + d/sum(A_i[j]))/((1 - d)/N + d/sum(A_i[i])), 1)
        Q[i,j] <- Q[i,j] * alpha[i,j]
      } else {
        alpha[i,j] <- min(Q[j,i]/Q[i,j], 1)
        Q[i,j] <- Q[i,j] * alpha[i,j]
      }
    }
  }
  diag(Q) <- 1 - sapply(1:N, function(i) sum(Q[i,-i]))
  return(Q)
}