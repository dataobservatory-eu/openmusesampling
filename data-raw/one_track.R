## code to prepare `one_track` dataset goes here
library(here)
library(spotifyr)
if (file.exists(here("_not_included", "myenviron.R"))) {
  source(here("_not_included", "myenviron.R"))
}
one_track <- get_track("14ngWWxvUSnIMXgF6rzSk1")

usethis::use_data(one_track, overwrite = TRUE)
