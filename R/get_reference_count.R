#' @title get_reference_count: get the number of references in a page
#'
#' @param language_code the language code
#' @param page_title the title of the article on wikipedia
#'
#' @export
get_reference_count <- function(language_code, page_title) {
  # Define the Wikipedia API endpoint and parameters using the provided language_code
  url <- paste0("https://", language_code, ".wikipedia.org/w/api.php")
  params <- list(
    action = "parse",        # Use 'parse' to get the content of the page
    format = "json",
    page = page_title,       # The title of the Wikipedia page
    prop = "externallinks"            # Get the full content (HTML) of the page
  )

  # Make the API request using httr::GET
  response <- httr::GET(url, query = params)

  # Parse the response as JSON using jsonlite::fromJSON
  data <- jsonlite::fromJSON(httr::content(response, "text"))

  # Check for errors in the API response
  if (!"parse" %in% names(data)) {
    stop("Error: ", data$error$message)
  }

  # Extract the references
  references <- data$parse$externallinks

  # Return the number of references
  return(length(references))
}
