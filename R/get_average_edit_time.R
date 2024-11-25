#' @title get_average_edit_time: get time between successive edits
#'
#' @param language_code the language code
#' @param page_title the title of the article on wikipedia
#'
#' @export
get_average_edit_time <-  function(language_code, page_title) {
  # Define the Wikipedia API endpoint
  url <- paste0("https://", language_code, ".wikipedia.org/w/api.php")

  # Initialize parameters for the API request
  params <- list(
    action = "query",
    format = "json",
    prop = "revisions",    # Get the revisions of the page
    titles = page_title,
    rvprop = "timestamp",  # Get the timestamp of each revision
    rvlimit = 500          # Fetch up to 500 revisions per request
  )

  # Initialize an empty list to store all timestamps
  all_timestamps <- list()

  # Loop to handle pagination (continue fetching revisions)
  repeat {
    response <- httr::GET(url, query = params)
    data <- jsonlite::fromJSON(httr::content(response, "text"))

    # Check for errors in the API response
    if (!"query" %in% names(data)) {
      stop("Error: ", data$error$message)
    }

    # Extract the timestamps from the revisions
    page_data <- data$query$pages[[1]]$revisions
    timestamps <- page_data$timestamp
    # Append the new timestamps to the list
    all_timestamps <- c(all_timestamps, timestamps)

    # Check if there is a "continue" parameter in the response
    if ("continue" %in% names(data)) {
      # If so, update the parameters with the "continue" value to fetch more revisions
      params$rvcontinue <- data$continue$rvcontinue
    } else {
      # No more results, exit the loop
      break
    }
  }

  # Convert the list of timestamps to POSIXct objects using lubridate::ymd_hms
  all_timestamps <- lubridate::ymd_hms(all_timestamps)

  # Calculate time differences between successive edits using base R's diff()
  time_diffs <- diff(sort(all_timestamps))

  # Calculate the average time difference (in seconds)
  avg_time_diff <- mean(time_diffs)

  # Return the average time between edits in days
  return(avg_time_diff)
}
