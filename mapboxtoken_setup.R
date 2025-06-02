mapbox_token <- Sys.getenv("MAPBOX_ACCESS_TOKEN", unset = "")

if (mapbox_token == "") {
  stop(
    "MAPBOX_ACCESS_TOKEN is not set. ",
    "Add it to ~/.Renviron using usethis::edit_r_environ() and add MAPBOX_ACCESS_TOKEN=YOUR TOKEN"
  )
}