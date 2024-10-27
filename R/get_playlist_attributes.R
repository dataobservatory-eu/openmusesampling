#' @title Extract artist URI
#' @param spotify_playlist_id For example, \code{"4JYQxezQQ85RDg7tASGsi7"}.
#' @param wasAssociatedWith Your ORiD ID.
#' @importFrom spotifyr search_spotify get_artist_albums get_track
#' @importFrom dplyr select left_join relocate
#' @importFrom spotifyr get_playlist get_playlist_audio_features
#' @return A data frame with the playlist content's attributes.
#' @export

get_playlist_attributes <- function( spotify_playlist_id, language_code ="", wasAssociatedWith) {
  
  startedAtTime <- Sys.time()
  
  playlist_attributes <- function(playlist, language_code) {
    data.frame(
      recording_title = playlist$tracks$items$track.name,
      language_code = language_code,
      recording_artist = sapply(playlist$tracks$items$track.album.artists, 
                                function(x) paste(x$name, collapse="|")),
      release_name = playlist$tracks$items$track.album.name,
      release_type = playlist$tracks$items$track.album.type,
      release_date = playlist$tracks$items$track.album.release_date,
      release_date_precision = playlist$tracks$items$track.album.release_date_precision,
      playlist_name = playlist$name,
      duration_ms = playlist$tracks$items$track.duration_ms,
      track_number= playlist$tracks$items$track.track_number,
      disc_number = playlist$tracks$items$track.disc_number,
      playlist_owner = playlist$owner$display_name,
      added_to_playlist_date = playlist$tracks$items$added_at,
      added_to_playlist_by = playlist$tracks$items$added_by.uri,
      spotify_track_id = playlist$tracks$items$track.id,
      spotify_album_id = playlist$tracks$items$track.album.id,
      spotify_playlist_id = playlist$id,
      spotify_playlist_owner_id = playlist$owner$id,
      isrc_id = playlist$tracks$items$track.external_ids.isrc,
      spotify_artist_id = sapply(playlist$tracks$items$track.album.artists, 
                                 function(x) paste(x$id, collapse="|"))
    )
  }
  
  playlist_audio_features <- function(af){
    af %>%
      select ( spotify_track_id = track.id, key_mode, tempo,valence, 
               acousticness, instrumentalness, speechiness, liveness,
               danceability, loudness, energy,  key_name, mode_name )
  }
  
  this_playlist <-  spotifyr::get_playlist(spotify_playlist_id)
  these_audio_features <- spotifyr::get_playlist_audio_features(playlist_uris = spotify_playlist_id)
  
  playlist_attributes <- playlist_attributes(this_playlist, language_code=language_code)
  playlist_af <- playlist_audio_features(these_audio_features)
  
  return_df <- playlist_attributes  %>% 
    dplyr::left_join (playlist_af, by = join_by(spotify_track_id) ) %>%
    dplyr::relocate( added_to_playlist_date:spotify_artist_id, .after=everything())
  
  prov <- provenance_statement(startedAtTime = startedAtTime,
                               endedAtTime = Sys.time(),
                               wasAssociatedWith = wasAssociatedWith)
  
  attr(return_df, "provenance") <- prov
  
  return_df
}
