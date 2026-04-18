# =============================================================================
# build_table.R
# Builds the interactive DT publication table with PubMed/DOI links.
# =============================================================================

library(DT)
library(dplyr)
library(stringr)

source("R/globals.R")

#' Build interactive publication table
#'
#' @param pub_data  tibble with columns:
#'   year, title, journal, theme, citations, pmid
#' @return DT datatable widget
build_pub_table <- function(pub_data) {

  table_data <- pub_data |>
    arrange(desc(year)) |>
    mutate(
      Title = case_when(
        !is.na(pmid) & pmid == "sciencedirect" ~ paste0(
          '<a href="https://www.sciencedirect.com/science/chapter/referencework/abs/pii/B9780128216187000316?via%3Dihub" ',
          'target="_blank" title="Open on ScienceDirect">', title, '</a>'
        ),
        !is.na(pmid) & pmid != "" ~ paste0(
          '<a href="https://pubmed.ncbi.nlm.nih.gov/', pmid,
          '" target="_blank" title="Open on PubMed">', title, '</a>'
        ),
        TRUE ~ title
      ),
      Theme = paste0(
        '<span style="background:', THEME_COLORS[theme], '22; color:',
        THEME_COLORS[theme], '; border:1px solid ', THEME_COLORS[theme],
        '55; padding:2px 8px; border-radius:4px; font-size:11px; white-space:nowrap;">',
        theme, '</span>'
      ),
      Citations = citations
    ) |>
    select(Year = year, Title, Journal = journal, Theme, Citations)

  datatable(
    table_data,
    escape    = FALSE,
    rownames  = FALSE,
    class     = "stripe hover compact",
    options   = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50),
      dom        = "ftp",
      order      = list(list(0, "desc")),
      columnDefs = list(
        list(width = "40%", targets = 1),
        list(width = "18%", targets = 2),
        list(width = "14%", targets = 3),
        list(width = "8%",  targets = 4),
        list(className = "dt-center", targets = c(0, 4))
      ),
      language = list(search = "Filter publications:")
    ),
    filter = "none"
  )
}
