# =============================================================================
# build_timeline.R
# Builds the publication timeline bar chart and career Gantt chart.
# =============================================================================

library(plotly)
library(dplyr)
library(purrr)
library(stringr)

source("R/globals.R")

#' Build stacked bar chart of publications by year and theme
#'
#' @param pub_data  tibble with columns: year, theme
#' @return plotly widget
build_pub_timeline <- function(pub_data) {

  timeline_data <- pub_data |>
    group_by(year, theme) |>
    summarise(n = n(), .groups = "drop")

  plot_ly(
    data          = timeline_data,
    x             = ~year,
    y             = ~n,
    color         = ~theme,
    colors        = THEME_COLORS,
    type          = "bar",
    text          = ~paste0(theme, ": ", n, " paper(s)"),
    hovertemplate = "%{text}<extra></extra>"
  ) |>
    layout(
      barmode = "stack",
      xaxis   = list(title = "", tickmode = "linear", tick0 = min(pub_data$year),
                     dtick = 1, tickangle = -45, showgrid = FALSE),
      yaxis   = list(title = "Publications", showgrid = TRUE,
                     gridcolor = "#F1EFE8", dtick = 1),
      legend  = list(orientation = "h", x = 0, y = 1.15, font = list(size = 12)),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 60, b = 60, l = 50, r = 20),
      font   = list(family = "sans-serif", size = 12, color = "#2C2C2A")
    ) |>
    config(displayModeBar = FALSE)
}


#' Build interactive career Gantt chart
#'
#' @param career_data  tibble with columns:
#'   role, org, country, start (YYYY-MM-DD), end (YYYY-MM-DD), pubs
#' @return plotly widget
build_career_gantt <- function(career_data) {

  origin_date <- as.Date("2008-01-01")

  career <- career_data |>
    mutate(
      start_d  = as.Date(start),
      end_d    = as.Date(end),
      x_start  = as.numeric(start_d - origin_date),
      x_end    = as.numeric(end_d   - origin_date),
      duration = x_end - x_start,
      y_label  = paste0(role, "  ·  ", org),
      color    = unname(COUNTRY_COLORS[country]),
      color    = ifelse(is.na(color), "#888780", color),
      hover    = paste0(
        "<b>", role, "</b><br>", org, "<br>",
        "<span style='color:#888'>", country, "</span><br>",
        format(start_d, "%b %Y"), " \u2013 ", format(end_d, "%b %Y"),
        ifelse(pubs > 0, paste0("<br><i>", pubs, " publication(s)</i>"), "")
      )
    ) |>
    arrange(desc(start_d))

  traces <- map(seq_len(nrow(career)), function(i) {
    row <- career[i, ]
    list(
      type          = "bar",
      orientation   = "h",
      name          = row$country,
      x             = list(row$duration),
      y             = list(row$y_label),
      base          = row$x_start,
      marker        = list(color = row$color, line = list(width = 0)),
      text          = list(row$hover),
      textposition  = "none",
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = FALSE
    )
  })

  tick_years  <- seq(as.integer(format(min(career$start_d), "%Y")),
                     as.integer(format(max(career$end_d),   "%Y")) + 1, by = 2)
  tick_vals   <- as.numeric(as.Date(paste0(tick_years, "-01-01")) - origin_date)

  layout <- list(
    barmode       = "stack",
    bargap        = 0.35,
    xaxis = list(
      title    = "",
      tickvals = tick_vals,
      ticktext = as.character(tick_years),
      showgrid = TRUE, gridcolor = "#F1EFE8", zeroline = FALSE,
      range    = c(-200, as.numeric(as.Date(paste0(max(tick_years) + 1, "-01-01"))
                                    - origin_date))
    ),
    yaxis = list(title = "", automargin = TRUE,
                 tickfont = list(size = 11), type = "category"),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    margin = list(t = 20, b = 30, l = 10, r = 20),
    height = 320,
    font   = list(family = "sans-serif", size = 12, color = "#2C2C2A")
  )

  plotly::plotly_build(list(data = traces, layout = layout)) |>
    config(displayModeBar = FALSE)
}
