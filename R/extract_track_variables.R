#' @title Extract variable data from tracks
#' @param df A data frame that may contain the \code{external_ids} column.
#' @return The external_ids column.
#' @param track_df Must be a data frame that contains track information, not artists,
#' playlists or shows.
#' @param time_query the time of the query.
#'
#' @export


extract_track_variables <- function(track_df, time_query) {

  if ( "id" %in% names(track_df) ) {
    id <- track_df$id  } else {
      id <- NA_integer_  }

  if ( "popularity" %in% names(track_df) ) {
    popularity <- as.integer(track_df$popularity) } else {
      popularity <- NA_integer_  }

  if ( "available_markets" %in% names(track_df)) {
    available_markets <- paste(track_df$available_markets, collapse = "|")
  } else { available_markets  <- NA_character_ }

  if ( "is_local" %in% names(track_df)) {
    is_local <- as.logical(track_df$is_local)
  } else { is_local  <- NA }

  data.frame ( id = id,
               time_query = time_query,
               popularity = popularity,
               is_local = is_local,
               available_markets = available_markets )
}
