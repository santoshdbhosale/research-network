# =============================================================================
# globals.R — shared constants used across all modules
# =============================================================================

# Country colour palette — single source of truth
# Used by world map, co-author network, and career Gantt
COUNTRY_COLORS <- c(
  "Finland" = "#1D9E75",   # teal
  "Denmark" = "#178ADD",   # blue
  "USA"     = "#7F77DD",   # purple
  "India"   = "#F5C400",   # yellow
  "Spain"   = "#3B6D11",   # dark green
  "Brazil"  = "#E07B00"    # orange
)

# Research theme colours — darker shades, distinct from country palette
THEME_COLORS <- c(
  "Biomarkers"    = "#2D6A4F",   # forest green
  "T cell biology"= "#5E3A8C",   # deep violet
  "Methods"       = "#B5580A",   # burnt orange
  "COVID-19"      = "#1B6CA8",   # steel blue
  "AI/ML"         = "#8B1A1A"    # dark crimson
)

# Geocode coordinate fallbacks for institutions that fail OSM
GEO_FALLBACKS <- list(
  list(pattern = "Cadiz.*Spain",      lat =  36.5271, lon =  -6.2886),
  list(pattern = "Cordoba.*Spain",    lat =  37.8882, lon =  -4.7794),
  list(pattern = "Sao Paulo.*Brazil", lat = -23.5614, lon = -46.7252),
  list(pattern = "CSIR.*Chemical",    lat =  18.5089, lon =  73.8102)
)
