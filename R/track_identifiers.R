#' @title Extract data that is required for the identification of tracks
#' @param df A data frame that may contain the \code{external_ids} column.
#' @return The external_ids column.
#' @param track Must be a data frame that contains track information, not artists,
#' playlists or shows.
#' @export

track_identitfiers <- function(track) {
  data.frame ( spotify_track_id = track$id,
               isrc = extract_isrc(track),
               duration_ms = track$duration_ms,
               spotify_artist_uri = extract_artist_uri(track))
}
