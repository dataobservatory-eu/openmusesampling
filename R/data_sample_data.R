#' @title Returned data of sample_data
#'
#' @description A data frame containing 1024 ISRCs
#'
#' @details
#' This is an example data to show how the Metropolis-Hastings uniform random-walk algorithm works
#'
#' @format A data frame containing 1204 rows and 6 columns
#' \describe{
#'   \item{id}{Spotify track IDs}
#'   \item{isrc}{Track ISRCs}
#'   \item{registrant}{The code of the registrant that registered the ISRC}
#'   \item{year}{The year the ISRC was registered}
#'   \item{designation}{The last five digist of the ISRC}
#'   \item{popularity}{The popularity of the tracks on Spotify}
#' }
"sample_data"
