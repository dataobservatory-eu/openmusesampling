#' @title get_unique_editors: extract the number of unique editors
#'
#' @param language_code the language code
#' @param page_title the title of the article on wikipedia
#'
#' @export
get_unique_editors <- function(language_code, page_title) {
  # Define the Wikipedia API endpoint and parameters using the provided language_code
  url <- paste0("https://", language_code, ".wikipedia.org/w/api.php")
  params <- list(
    action = "query",
    format = "json",
    prop = "revisions",
    titles = page_title,
    rvprop = "user",  # Get the editor's username
    rvlimit = "max"   # Set limit to maximum (or specify a number)
  )

  # Make the API request
  response <- httr::GET(url, query = params)
  data <- jsonlite::fromJSON(httr::content(response, "text"))

  # Check for errors in the API response
  if (!"query" %in% names(data)) {
    stop("Error: ", data$error$message)
  }

  # Extract the page revisions
  page_data <- data$query$pages[[1]]$revisions

  # Extract the usernames of all editors
  editors <- unique(page_data$user)

  # Return the number of unique editors
  return(length(editors))
}
