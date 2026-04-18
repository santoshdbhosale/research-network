# =============================================================================
# build_map.R
# Builds an interactive Leaflet world map of co-author institutions.
# =============================================================================

library(leaflet)
library(tidygeocoder)
library(dplyr)
library(stringr)

source("R/globals.R")

#' Build interactive world collaboration map
#'
#' @param coauthor_institutions  tibble with columns: author, institution, country
#' @param home_lat  Latitude of researcher's current location
#' @param home_lon  Longitude of researcher's current location
#' @param home_inst Name of researcher's current institution
#' @return A leaflet widget
build_map <- function(coauthor_institutions,
                      home_lat  = 18.5204,
                      home_lon  = 73.8567,
                      home_inst = "Current institution") {

  message("Geocoding institutions...")

  # Geocode unique institutions
  institutions_geo <- coauthor_institutions |>
    distinct(institution, country) |>
    mutate(address = paste(institution, country, sep = ", ")) |>
    geocode(address, method = "osm", lat = lat, long = lon) |>
    # Apply manual coordinate fallbacks
    mutate(
      lat = apply_geo_fallback(address, lat, "lat"),
      lon = apply_geo_fallback(address, lon, "lon")
    ) |>
    filter(!is.na(lat))

  coauthor_geo <- coauthor_institutions |>
    left_join(institutions_geo, by = c("institution", "country"))

  # Add home location
  home <- tibble(
    author      = "You",
    institution = home_inst,
    country     = detect_country(home_lat, home_lon),
    lat         = home_lat,
    lon         = home_lon
  )

  all_locations <- bind_rows(home, coauthor_geo) |>
    filter(!is.na(lat))

  # Count collaborators per institution
  inst_counts <- all_locations |>
    group_by(institution, lat, lon, country) |>
    summarise(n_authors = n(), .groups = "drop") |>
    mutate(
      radius     = pmax(8, pmin(n_authors * 6, 30)),
      node_color = unname(COUNTRY_COLORS[country]),
      node_color = ifelse(is.na(node_color), "#888780", node_color)
    )

  # Build colorFactor with explicit level ordering
  country_pal <- colorFactor(
    palette  = unname(COUNTRY_COLORS),
    levels   = names(COUNTRY_COLORS),
    ordered  = TRUE,
    na.color = "#888780"
  )

  leaflet(inst_counts, options = leafletOptions(minZoom = 2)) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    setView(lng = 20, lat = 30, zoom = 2) |>
    addCircleMarkers(
      lng         = ~lon,
      lat         = ~lat,
      radius      = ~radius,
      color       = ~node_color,
      fillColor   = ~node_color,
      fillOpacity = 0.85,
      stroke      = TRUE,
      weight      = 1.5,
      opacity     = 1,
      popup       = ~paste0(
        "<b>", institution, "</b><br>",
        country, "<br>",
        "<i>", n_authors, " collaborator(s)</i>"
      ),
      label = ~institution
    ) |>
    addLegend(
      position = "bottomleft",
      colors   = unname(COUNTRY_COLORS),
      labels   = names(COUNTRY_COLORS),
      title    = "Country",
      opacity  = 0.9
    )
}


#' Apply manual coordinate fallbacks after OSM geocoding
apply_geo_fallback <- function(address, coord_col, which = c("lat", "lon")) {
  which <- match.arg(which)
  result <- coord_col
  for (fb in GEO_FALLBACKS) {
    idx <- is.na(result) & str_detect(address, fb$pattern)
    result[idx] <- if (which == "lat") fb$lat else fb$lon
  }
  result
}


#' Simple country detection from coordinates (coarse, for home location)
detect_country <- function(lat, lon) {
  if (lat > 8 & lat < 37 & lon > 68 & lon < 97)  return("India")
  if (lat > 59 & lat < 71 & lon > 20 & lon < 32)  return("Finland")
  if (lat > 54 & lat < 58 & lon > 8  & lon < 15)  return("Denmark")
  if (lat > 24 & lat < 50 & lon > -125 & lon < -66) return("USA")
  "Unknown"
}
