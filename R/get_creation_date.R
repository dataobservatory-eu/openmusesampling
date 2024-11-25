#' @title get_creation_date: get the creation date of an article
#'
#' @param language_code the language code
#' @param page_title the title of the article on wikipedia
#'
#' @export
get_creation_date <- function(language_code, page_title) {
  url <- paste0("https://", language_code, ".wikipedia.org/w/api.php?action=query&titles=",
                utils::URLencode(page_title), "&prop=revisions&rvlimit=1&rvdir=newer&rvprop=timestamp&format=json")

  response <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
  creation_date <- data$query$pages[[1]]$revisions$timestamp
  return(as.Date(creation_date))
}
