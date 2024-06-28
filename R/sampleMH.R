#' @title samplerMH a Metropolis-Hastings algorithm to run the Random Walk in the network
#'
#' @param A network adjacency matrix
#' @param Q network transition matrix
#' @param d the probability to jump to a non connected node
#' @param size the size of the sample
#' @return returns a list with random samples from network nodes and the acceptance rate of the jumps
#'
#' @export
samplerMH <- function(A, Q, d = 0.5, size = 100){
  N <- nrow(A)
  nodes <- 1:N
  A_i <- rowSums(A)/N #Computing the degrees of each node
  samp <- numeric(size) #store the sample
  #Initialize the algorithm
  samp[1] <- sample(nodes, size = 1, prob = A_i) #We sample one node uniformly to start up the algorithm
  #Now creating the sample
  acceptance <- c() #tracking the acceptance rate
  for(i in 2:size){
    #With probability d we sample a neighbour
    neighbour_or_not <- sample(1:0, size = 1, prob = c(1-d, d))
    Z <- stats::runif(1)
    if(neighbour_or_not == 1 & sum(A[samp[i-1],] == 1) > 0){
      neighbours <- which(A[samp[i-1],] == 1)
      if(length(neighbours) == 1){
        if(Z > 1-d){
          samp[i] <- samp[i-1]
          acceptance <- c(acceptance, 0)
        } else {
          samp[i] <- nodes[neighbours]
          acceptance <- c(acceptance, 1)
        }
      } else {
        proposal <- sample(nodes[neighbours], size = 1, prob = Q[samp[i-1], neighbours])
        if(Z > 1-d){
          samp[i] <- samp[i-1]
          acceptance <- c(acceptance, 0)
        } else {
          samp[i] <- proposal
          acceptance <- c(acceptance, 1)
        }
      }
    } else {
      not_neighbours <- which(A[samp[i-1],] == 0)
      if(length(not_neighbours) == 1){
        samp[i] <- nodes[not_neighbours]
      } else {
        samp[i] <- sample(nodes[not_neighbours], size = 1, prob = Q[samp[i-1], not_neighbours])
      }
    }
  }
  return(
    list(sample = samp,
         acceptance_rate = sum(acceptance)/size)
  )
}
