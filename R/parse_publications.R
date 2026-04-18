# =============================================================================
# parse_publications.R
# Robust parser for researcher publication list PDFs.
#
# Supported formats:
#   Format A — Year heading + Title / Authors | Journal  (Santosh's format)
#   Format B — Numbered list: 1. Authors. Title. Journal. year.
#   Format C — APA style: Authors (year). Title. Journal, vol, pages.
#   Format D — Vancouver: Authors. Title. Journal. Year;vol:pages.
#   Format E — Generic fallback
#
# Returns tibble: title, authors, journal, year, theme
# =============================================================================

library(pdftools)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

#' Parse a publication list PDF into a structured tibble
#'
#' @param pdf_path   Path to PDF file
#' @param your_name  Optional - researcher name to skip in co-author detection
#' @return tibble: title, authors, journal, year, theme
parse_publications <- function(pdf_path, your_name = NULL) {

  message("Reading PDF...")
  raw  <- pdf_text(pdf_path)
  text <- paste(raw, collapse = "\n")

  # Normalise whitespace
  text <- str_replace_all(text, "\r\n|\r", "\n")
  text <- str_replace_all(text, "[ \t]+", " ")
  text <- str_replace_all(text, "\n{3,}", "\n\n")
  text <- str_trim(text)

  fmt <- detect_format(text)
  message(paste("Detected format:", fmt))

  pubs <- switch(fmt,
    "A" = parse_format_a(text),
    "B" = parse_format_b(text),
    "C" = parse_format_c(text),
    "D" = parse_format_d(text),
         parse_format_e(text)
  )

  # If primary parser fails try all and pick best
  if (length(pubs) == 0 || nrow(bind_rows(pubs)) == 0) {
    message("Trying all parsers...")
    all_results <- list(
      parse_format_a(text), parse_format_b(text),
      parse_format_c(text), parse_format_d(text),
      parse_format_e(text)
    )
    counts <- map_int(all_results, ~nrow(bind_rows(.x)))
    pubs   <- all_results[[which.max(counts)]]
    message(paste("Best result:", max(counts), "publications"))
  }

  result <- bind_rows(pubs) |>
    filter(!is.na(title), nchar(str_trim(title)) > 15) |>
    mutate(
      title   = str_squish(str_to_title(title)),
      year    = as.integer(year),
      journal = str_squish(coalesce(journal, "")),
      theme   = guess_theme(title)
    ) |>
    filter(!is.na(year),
           year >= 1990,
           year <= as.integer(format(Sys.Date(), "%Y")) + 1) |>
    arrange(desc(year)) |>
    distinct(title, .keep_all = TRUE)

  message(paste("Parsed", nrow(result), "publications"))
  result
}


# ── Format detection ----------------------------------------------------------

detect_format <- function(text) {
  if (str_count(text, "\n(19|20)\\d{2}\n") >= 2 && str_detect(text, "\\|"))
    return("A")
  if (str_detect(text, "(^|\n)\\s*\\d+[.)]\\ +[A-Z]"))
    return("B")
  if (str_detect(text, "\\((19|20)\\d{2}\\)\\."))
    return("C")
  if (str_detect(text, "\\. (19|20)\\d{2};"))
    return("D")
  "E"
}


# ── Format A: Year heading + Title + Authors | Journal -----------------------
# Santosh's own format — most reliable

parse_format_a <- function(text) {
  pubs <- list()

  # Split into year-headed sections
  sections <- str_split(text, "(?=\n(19|20)\\d{2}\n)")[[1]]

  for (section in sections) {
    year_match <- str_extract(section, "(19|20)\\d{2}")
    if (is.na(year_match)) next

    body   <- str_trim(str_remove(section, "^.*?(19|20)\\d{2}\\s*\n"))
    blocks <- str_split(body, "\n\n+")[[1]]
    blocks <- blocks[nchar(str_trim(blocks)) > 10]

    for (block in blocks) {
      lines    <- str_trim(str_split(str_trim(block), "\n")[[1]])
      lines    <- lines[nchar(lines) > 0]
      pipe_idx <- which(str_detect(lines, "\\|"))

      if (length(pipe_idx) > 0) {
        pipe_line   <- paste(lines[pipe_idx], collapse = " ")
        parts       <- str_split(pipe_line, "\\s*\\|\\s*")[[1]]
        authors     <- str_trim(parts[1])
        journal     <- str_trim(paste(parts[-1], collapse = " "))
        title_lines <- lines[seq_len(pipe_idx[1] - 1)]
        if (length(title_lines) == 0) next
        title <- str_trim(paste(title_lines, collapse = " "))
        if (nchar(title) > 10) {
          pubs <- c(pubs, list(tibble(
            title, authors, journal, year = as.integer(year_match)
          )))
        }
      }
    }
  }
  pubs
}


# ── Format B: Numbered list ---------------------------------------------------

parse_format_b <- function(text) {
  pubs    <- list()
  entries <- str_split(text, "(?=\n\\s*\\d+[.)]\\ )")[[1]]
  entries <- entries[str_detect(entries, "\\d+[.)]")]

  for (entry in entries) {
    entry <- str_trim(str_remove(entry, "^\\d+[.)]\\ *"))
    if (nchar(entry) < 20) next
    year <- str_extract(entry, "(19|20)\\d{2}")
    if (is.na(year)) next

    clean <- str_remove_all(entry, "\\(?(19|20)\\d{2}\\)?[;,.]?\\s*")
    sents <- str_trim(str_split(clean, "(?<=[.!?])\\s+(?=[A-Z])")[[1]])
    sents <- sents[nchar(sents) > 5]

    if (length(sents) >= 3) {
      pubs <- c(pubs, list(tibble(
        title   = sents[2],
        authors = sents[1],
        journal = str_extract(sents[length(sents)], "^[^,;]+"),
        year    = as.integer(year)
      )))
    } else if (length(sents) == 2) {
      pubs <- c(pubs, list(tibble(
        title   = sents[1],
        authors = NA_character_,
        journal = sents[2],
        year    = as.integer(year)
      )))
    }
  }
  pubs
}


# ── Format C: APA -------------------------------------------------------------

parse_format_c <- function(text) {
  pubs    <- list()
  entries <- str_split(text, "(?=\n[A-Z][a-zA-Z-]+,)")[[1]]

  for (entry in entries) {
    entry <- str_trim(entry)
    year  <- str_extract(entry, "\\((19|20)\\d{4}\\)")
    year  <- str_extract(year, "\\d{4}")
    if (is.na(year)) next

    authors    <- str_trim(str_extract(entry, "^.*?(?=\\s*\\(\\d{4}\\))"))
    after_year <- str_trim(str_remove(entry, paste0("^.*?\\(", year, "\\)\\.\\s*")))
    title      <- str_trim(str_extract(after_year, "^[^.]+"))
    journal    <- str_trim(str_extract(
      str_remove(after_year, paste0("^", fixed(title), "\\. *")), "^[^,.]+"
    ))

    if (!is.na(title) && nchar(title) > 10) {
      pubs <- c(pubs, list(tibble(
        title, authors,
        journal = coalesce(journal, NA_character_),
        year    = as.integer(year)
      )))
    }
  }
  pubs
}


# ── Format D: Vancouver -------------------------------------------------------

parse_format_d <- function(text) {
  pubs    <- list()
  entries <- str_split(text, "\n\n+")[[1]]

  for (entry in entries) {
    entry <- str_squish(str_trim(entry))
    if (nchar(entry) < 30) next
    year  <- str_extract(entry, "(19|20)\\d{2}(?=[;:\\s])")
    if (is.na(year)) year <- str_extract(entry, "(19|20)\\d{2}")
    if (is.na(year)) next

    sents <- str_trim(str_split(entry, "(?<=\\.)\\s+(?=[A-Z])")[[1]])
    sents <- sents[nchar(sents) > 5]

    if (length(sents) >= 3) {
      pubs <- c(pubs, list(tibble(
        title   = sents[2],
        authors = sents[1],
        journal = str_extract(sents[3], "^[^.;]+"),
        year    = as.integer(year)
      )))
    }
  }
  pubs
}


# ── Format E: Generic fallback -----------------------------------------------

parse_format_e <- function(text) {
  pubs  <- list()
  lines <- str_trim(str_split(text, "\n")[[1]])
  lines <- lines[nchar(lines) > 5]
  n     <- length(lines)
  i     <- 1

  while (i <= n) {
    year <- str_extract(paste(lines[i:min(i+2, n)], collapse = " "),
                        "(19|20)\\d{2}")
    if (!is.na(year) && as.integer(year) >= 1990 && nchar(lines[i]) > 15) {
      pubs <- c(pubs, list(tibble(
        title   = lines[i],
        authors = NA_character_,
        journal = NA_character_,
        year    = as.integer(year)
      )))
      i <- min(i + 3, n + 1)
    } else {
      i <- i + 1
    }
  }
  pubs
}


# ── Theme guesser -------------------------------------------------------------

guess_theme <- function(titles) {
  t <- str_to_lower(titles)
  case_when(
    str_detect(t, "covid|hdl|ecfc|cardiovascular|sars|endothelial colony") ~ "COVID-19",
    str_detect(t, "th17|treg|t cell|foxp3|fosl|batf|cip2a|regulatory t|immune|differentiation") ~ "T cell biology",
    str_detect(t, "method|itraq|tmt|label.free|quantif|phospho|dia[^bet]|dda|mass spec|isobaric|orbitrap|astral") ~ "Methods",
    str_detect(t, "ai|machine learn|llm|drug discov|biomarker module|large language") ~ "AI/ML",
    TRUE ~ "Biomarkers"
  )
}
