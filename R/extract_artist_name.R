#' @title Extract artist name
#' @param df A data frame
#' @return Artist names as a single character string.
#' @export

extract_artist_name <- function(df) {
  if ("artists" %in% names(df)) {
    if ("name" %in% names(df$artists)) {
      name <- paste(df$artists$name, collapse = "|")
    } else { name <- NA_character_}
  } else {
    name <- NA_character_
  }
  name
}
