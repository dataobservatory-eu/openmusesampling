#' @title A_compute: Compute adjacency matrix to construct a network
#' 
#' @param df a data frame containing a column ISRC codes, and a column with the registrant of each ISRC
#' @return returns an adjacency matrix
#' 
#' @export 
A_compute <- function(df){
  df$ident <- 1:nrow(df)
  A <- matrix(0, nrow = nrow(df), ncol = nrow(df))
  for(i in 1:nrow(A)){
    ids <- which(df$registrant == df$registrant[i])
    ids <- ids[-which(ids == i)]
    A[i,ids] <- 1
  }
  return(A)
}