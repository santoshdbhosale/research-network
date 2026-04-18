# =============================================================================
# fetch_scholar.R
# Fetches and cleans publication data from Google Scholar via the scholar pkg.
# =============================================================================

library(scholar)
library(dplyr)
library(stringr)

#' Fetch cited peer-reviewed publications from Google Scholar
#'
#' @param scholar_id  Google Scholar user ID (from profile URL)
#' @return tibble with columns: title, author, journal, year, cites
fetch_scholar_pubs <- function(scholar_id) {
  message("Fetching publications from Google Scholar...")

  tryCatch({
    pubs <- get_publications(scholar_id)

    pubs <- pubs |>
      filter(
        !is.na(author),
        journal != "",
        cites > 0,
        !str_detect(str_to_lower(journal), "biorxiv|bio rxiv|preprint|arxiv")
      ) |>
      mutate(
        year  = as.integer(year),
        title = str_to_title(str_trim(title))
      ) |>
      arrange(desc(year))

    message(paste("Found", nrow(pubs),
                  "cited peer-reviewed publications (preprints excluded)"))
    pubs

  }, error = function(e) {
    warning("Could not fetch from Google Scholar: ", conditionMessage(e),
            "\nProceeding with PDF data only.")
    NULL
  })
}


#' Fetch researcher profile info from Google Scholar
#'
#' @param scholar_id  Google Scholar user ID
#' @return list with name, affiliation, total_cites, h_index
fetch_scholar_profile <- function(scholar_id) {
  tryCatch({
    profile <- get_profile(scholar_id)
    list(
      name        = profile$name,
      affiliation = profile$affiliation,
      total_cites = profile$total_cites,
      h_index     = profile$h_index,
      i10_index   = profile$i10_index
    )
  }, error = function(e) {
    warning("Could not fetch Scholar profile: ", conditionMessage(e))
    list(name = "Researcher", affiliation = "", 
         total_cites = NA, h_index = NA, i10_index = NA)
  })
}
