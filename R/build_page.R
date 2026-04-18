# =============================================================================
# build_page.R
# Assembles all widgets into a single self-contained HTML page.
# =============================================================================

library(htmltools)

#' Assemble all widgets into one HTML page
#'
#' @param name         Researcher name
#' @param n_pubs       Total publications count
#' @param n_coauthors  Total co-authors count
#' @param n_countries  Countries count
#' @param total_cites  Total citations
#' @param years_active Number of active research years
#' @param career_fig   Plotly Gantt widget
#' @param timeline     Plotly timeline widget
#' @param pub_table    DT table widget
#' @param world_map    Leaflet map widget
#' @param network_plot visNetwork widget
#' @return tagList HTML object (save with htmltools::save_html)
build_page <- function(name         = "Researcher",
                       n_pubs       = 0,
                       n_coauthors  = 0,
                       n_countries  = 0,
                       total_cites  = 0,
                       years_active = 0,
                       career_fig,
                       timeline,
                       pub_table,
                       world_map,
                       network_plot) {
  tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(paste("Research Network —", name)),
      tags$style(HTML("
        body  { font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
                margin:0; padding:2rem 1.5rem; background:#fff; color:#2C2C2A; }
        h1    { font-size:1.6rem; font-weight:500; margin-bottom:0.25rem; }
        .sub  { color:#5F5E5A; font-size:0.95rem; margin-bottom:2rem; }
        .stats{ display:flex; gap:1.5rem; margin-bottom:2.5rem; flex-wrap:wrap; }
        .stat { background:#F1EFE8; border-radius:8px;
                padding:0.75rem 1.25rem; min-width:90px; }
        .stat-n{ font-size:1.5rem; font-weight:500; color:#D85A30; }
        .stat-l{ font-size:0.8rem; color:#5F5E5A; margin-top:2px; }
        h2    { font-size:1.1rem; font-weight:500; margin:2.5rem 0 0.4rem; }
        .note { font-size:0.8rem; color:#888780;
                margin-top:0.3rem; margin-bottom:0.75rem; }
        .widget-wrap{ border:0.5px solid #D3D1C7; border-radius:10px;
                      overflow:hidden; margin-bottom:0.5rem; }
        table.dataTable thead th {
          font-size:12px; font-weight:500; background:#F1EFE8;
          border-bottom:1px solid #D3D1C7 !important; }
        table.dataTable td { font-size:13px; vertical-align:middle; }
        .dataTables_filter input{ border:0.5px solid #D3D1C7; border-radius:6px;
                                   padding:4px 10px; font-size:13px; }
        .dataTables_info,.dataTables_paginate{ font-size:12px; color:#888780; }
      "))
    ),
    tags$body(
      tags$h1(paste("Research Network —", name)),
      tags$p(class = "sub", paste0(
        n_pubs, " peer-reviewed publications · ",
        n_coauthors, " co-authors · ",
        n_countries, " countries · ",
        total_cites, " total citations"
      )),

      # Stat cards
      tags$div(class = "stats",
        stat_card(n_pubs,       "Publications"),
        stat_card(n_coauthors,  "Co-authors"),
        stat_card(n_countries,  "Countries"),
        stat_card(total_cites,  "Total citations"),
        stat_card(years_active, "Years active")
      ),

      # Career timeline
      tags$h2("Career timeline"),
      tags$p(class = "note",
        "Colour = country of institution. Hover a bar for role details."),
      tags$div(class = "widget-wrap", career_fig),

      # Publication timeline
      tags$h2("Publication timeline"),
      tags$p(class = "note", "Coloured by research theme. Hover a bar for details."),
      tags$div(class = "widget-wrap", timeline),

      # Publication table
      tags$h2("All publications"),
      tags$p(class = "note",
        "Click a title to open the paper. Use the search box to filter."),
      tags$div(style = "margin-bottom:2rem;", pub_table),

      # World map
      tags$h2("Global collaboration map"),
      tags$p(class = "note",
        "Circle size = collaborators at that institution. Click for details."),
      tags$div(class = "widget-wrap", world_map),

      # Network
      tags$h2("Co-author network"),
      tags$p(class = "note",
        "Node size = shared publications. Edge width = collaboration strength. ",
        "Hover to highlight. Search box to find a collaborator."),
      tags$div(class = "widget-wrap", network_plot)
    )
  )
}

# Helper: single stat card
stat_card <- function(value, label) {
  tags$div(class = "stat",
    tags$div(class = "stat-n", value),
    tags$div(class = "stat-l", label)
  )
}
