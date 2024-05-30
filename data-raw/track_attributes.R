## code to prepare `one_track` dataset goes here
library(here)
library(spotifyr)
if (file.exists(here("_not_included", "myenviron.R"))) {
  source(here("_not_included", "myenviron.R"))
}

#Getting some track ids from Spotify
track_ids <- c("52NGJPcLUzQq5w7uv4e5gf",
               "0SjuHX81R1S1NoAJ6OeUEx",
               "7rl3oe00C0iWzsdmRoIsuG",
               "7AEHU8IhxfKPBP9uyhimWs",
               "5HQ639Z3ms3hnZx0KfWnkp",
               "3D7OzI7dH5pDxr8NqkH3AR",
               "3PFu0tBuWz1wA4EioKAawF",
               "3dS8RW1e5gd5rbSmNjDAPe",
               "0kKsMonfmfuEphvukUwBtD",
               "4kmfBIElvpaQZAa5hCdp9p")

#Extracting the tracks
get_tracks_return_raw <- lapply(
  1:length(track_ids),
  function(x) get_track(track_ids[x])
)

#Extracting attributes
track_attributes <- lapply(
  1:length(track_ids),
  function(x) extract_track_attributes(get_tracks_return_raw[[x]])
)

tracks_attributes <- do.call(rbind, tracks_attributes)

#Saving the data
usethis::use_data(track_attributes, overwrite = TRUE)
