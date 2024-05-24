#' @title Extract artist URI
#' @param df A data frame
#' @return Artist URIs as a single character string.
#' @export

extract_artist_uri <- function(df) {
  if ("artists" %in% names(df)) {
    if ("uri" %in% names(df$artists)) {
      uri <- paste(df$artists$uri, collapse = "|")
    } else { uri <- NA_character_}
  } else {
    uri <- NA_character_
  }
  uri
}
