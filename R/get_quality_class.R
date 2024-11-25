#' @title get_quality_class: get page's quality class
#'
#' @param language_code the language code
#' @param page_title the title of the article on wikipedia
#'
#' @export
get_quality_class <- function(language_code, article_title) {
  url <- paste0("https://", language_code, ".wikipedia.org/w/api.php?action=query&prop=pageassessments&titles=",
                utils::URLencode(article_title), "&format=json")

  response <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
  assessments <- data$query$pages[[1]]$pageassessments
  return(ifelse(length(assessments) > 0, assessments[[1]]$class, "Unassessed"))
}
