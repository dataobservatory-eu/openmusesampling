#' @title Extract ISRC from a data frame
#' @param df A data frame that may contain the \code{external_ids} column.
#' @return The external_ids column.
#' @export

extract_isrc <- function(df) {
  if ("external_ids" %in% names(df)) {
    if ("isrc" %in% names(df$external_ids)) {
      isrc <- paste(df$external_ids$isrc, collapse = "|")
    } else { isrc <- NA_character_}
  } else {
    isrc <- NA_character_
  }
  isrc
}
