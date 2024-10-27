#' @title search_tracks_by_term: Extracts data from Spotify API using a search term
#'
#' @param search_term a string containing the term to search for on Spotify.
#' @param wasAssociatedWith an ORCID to write a provenance statement to keep track of who extracted the data set.
#' @return returns a data frame with tracks extracted from Spotify API.
#'
#' @examples
#' tracks <- search_tracks_by_term("Jana", "https://orcid.org/0000-0003-4253-1953")
#'
#'
#' @export
search_tracks_by_term <- function(search_term, wasAssociatedWith) {
  startedAtTime <- Sys.time()

  uri_to_id <- function(uri) gsub("spotify:artist:|spotify:album:", "", uri)
  potential_artists <- search_spotify(q=search_term, type="artist") %>%
    dplyr::select ( uri, popularity )

  spotify_artist_ids <- gsub("spotify:artist:", "", potential_artists$uri)

  potential_artists_albums  <- lapply(spotify_artist_ids, get_artist_albums)
  albums_df <- purrr::reduce(potential_artists_albums, rbind)
  tracks    <- lapply(albums_df$id,  get_album_tracks)
  tracks_df <- purrr::reduce(lapply ( tracks, function(x) x %>% dplyr::select (name, duration_ms, id, uri)), rbind)

  get_track_data <- function(id) {

    df <- get_track(id)
    data.frame (
      spotify_track_id = df$id,
      spotify_track_uri=  df$uri,
      popularity      =  df$popularity,
      duration_ms     =  df$duration_ms,
      time_query     = Sys.time()) %>%
      dplyr::bind_cols(df$artists %>% dplyr::select (spotify_artist_name = name, spotify_artist_id = id, spotify_artist_uri = uri)
      )
  }

  all_tracks <- lapply(tracks_df$id, get_track_data)
  all_tracks_df <- purrr::reduce(all_tracks, rbind)

  #Extracting ISRCs
  #Let's get in batches of 50 so we make less queries
  # batches <- seq(1, nrow(all_tracks_df), by = 50)
  # batches <- c(batches, nrow(all_tracks_df))
  # isrcs <- lapply(2:length(batches), function(x) get_tracks(ids = all_tracks_df$spotify_track_id[batches[x-1]:batches[x]]))
  isrcs <- lapply(1:nrow(all_tracks_df), function(i) get_track(id = all_tracks_df$spotify_track_id[i]))
  isrcs <- data.frame(spotify_track_id = sapply(isrcs, function(i) i$id),
                      isrc = sapply(isrcs, function(i) i$external_ids$isrc))

  if(all.equal(isrcs$spotify_track_id, all_tracks_df$spotify_track_id)){
    all_tracks_df$isrc <- isrcs$isrc
  } else {
    stop("Some track ids were not extracted.")
  }
  #Selecting country code
  all_tracks_df$isrc_country <- substr(all_tracks_df$isrc, 1, 2)

  #Selecting registrant ocde
  all_tracks_df$isrc_registrant <- substr(all_tracks_df$isrc, 3, 5)

  #Selecting year of reference
  all_tracks_df$isrc_year <- substr(all_tracks_df$isrc, 6, 7)

  prov <- provenance_statement(startedAtTime = startedAtTime, endedAtTime = Sys.time(), wasAssociatedWith = wasAssociatedWith)

  attr(all_tracks_df, "provenance") <- prov

  return(all_tracks_df)
}
