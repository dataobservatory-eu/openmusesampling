#' @title get_article_length: extract article length (in bytes)
#'
#' @param language_code the language code
#' @param page_title the title of the article on wikipedia
#'
#' @export
get_article_length <- function(language_code, page_title) {
  # Define the Wikipedia API endpoint and parameters using the provided language_code
  url <- paste0("https://", language_code, ".wikipedia.org/w/api.php")
  params <- list(
    action = "query",
    format = "json",
    prop = "info",       # Use 'info' to get page metadata like size
    titles = page_title,
    inprop = "size"      # Retrieve the page size (in bytes)
  )

  response <- httr::GET(url, query = params)
  data <- jsonlite::fromJSON(httr::content(response, "text"))

  # Check for errors in the API response
  if (!"query" %in% names(data)) {
    stop("Error: ", data$error$message)
  }

  # Extract the size of the article (in bytes)
  page_data <- data$query$pages[[1]]
  article_size <- page_data$length  # Length in bytes

  # Return the article size in bytes
  return(article_size)
}
