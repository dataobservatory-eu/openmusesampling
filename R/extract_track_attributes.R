#' @title Extract attribute data from tracks
#' @param df A data frame that may contain the \code{external_ids} column.
#' @return data frame with track attributes
#' @param track_df Must be a data frame that contains track information, not artists,
#' playlists or shows.
#'
#' @export
#'

extract_track_attributes <- function(track_df){

  #Extracting ISRC
  isrc <- extract_isrc(track_df)

  #Extracting track id
  if ( "id" %in% names(track_df) ) {
    id <- track_df$id  } else {
      id <- NA_integer_  }

  #Extracting track name
  if( "name" %in% names(track_df)){
    name <- track_df$name
  } else {
    name <- NA_character_
  }

  #duration
  if( "duration_ms" %in% names(track_df) ){
    duration_ms <- track_df$duration_ms
  } else {
    duration_ms <- NA_integer_
  }

  #album
  if( "album" %in% names(track_df) ){
    #name
    if("name" %in% names(track_df$album)){
      album_name <- track_df$album$name
    } else{
      album_name <- NA_character_
    }
    #uri
    if("uri" %in% names(track_df$album)){
      album_uri <- track_df$album$uri
    } else {
      album_uri <- NA_character_
    }
    #release date
    if("release_date" %in% names(track_df$album)){
      release_date <- track_df$album$release_date
    }
  }

  #artist
  #name
  artist <- extract_artist_name(track_df)
  #uri
  artist_uri <- extract_artist_uri(track_df)

  return(data.frame(isrc = isrc,
                    id = id,
                    name = name,
                    duration_ms = duration_ms,
                    album_name = album_name,
                    album_uri = album_uri,
                    release_date = release_date,
                    artist = artist,
                    artist_uri = artist_uri))
}
