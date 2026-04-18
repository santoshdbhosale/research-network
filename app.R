# =============================================================================
# app.R — Research Network Visualiser
# A Shiny app that generates a personalised interactive research network
# from a Google Scholar ID and publication list PDF.
#
# Usage:
#   shiny::runApp()                          # local
#   shiny::runGitHub("research-network",     # from GitHub
#                    "santoshdbhosale")
# =============================================================================

library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(purrr)

# Load all modules
source("R/globals.R")
source("R/parse_publications.R")
source("R/fetch_scholar.R")
source("R/build_map.R")
source("R/build_network.R")
source("R/build_timeline.R")
source("R/build_table.R")
source("R/build_page.R")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Research Network Visualiser",
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#D85A30",
    base_font   = font_google("Inter")
  ),

  sidebar = sidebar(
    width = 310,

    h5("Your details", style = "font-weight:600; margin-top:0;"),

    textInput("scholar_id", "Google Scholar ID",
              placeholder = "e.g. sIxWbx0AAAAJ",
              width = "100%"),
    tags$small(class = "text-muted",
      "Find it in your Scholar profile URL: ",
      tags$code("scholar.google.com/citations?user=XXXXXXXX")),

    br(), br(),

    fileInput("pub_pdf", "Publication list (PDF)",
              accept      = ".pdf",
              buttonLabel = "Choose PDF",
              placeholder = "No file selected",
              width       = "100%"),
    tags$small(class = "text-muted",
      "Your CV or publication list as PDF. ",
      "See ", tags$a("example format", href = "example/Santosh_Publications.pdf",
                     target = "_blank"), "."),

    hr(),

    h6("Optional — personalise your network",
       style = "font-weight:600; color:#5F5E5A;"),

    textInput("your_name", "Your name (as on papers)",
              placeholder = "e.g. Bhosale SD", width = "100%"),
    textInput("your_inst", "Current institution",
              placeholder = "e.g. Partex.ai", width = "100%"),
    textInput("your_country", "Current country",
              placeholder = "e.g. India", width = "100%"),

    hr(),

    h6("Career history", style = "font-weight:600; color:#5F5E5A;"),
    tags$small(class = "text-muted",
      "Paste your career history as CSV (role,org,country,start,end,pubs). ",
      "Leave blank to skip the Gantt chart."),
    br(), br(),
    textAreaInput("career_csv",
                  label   = NULL,
                  rows    = 5,
                  width   = "100%",
                  placeholder = paste(
                    "PhD Researcher,University of Turku,Finland,2012-01-01,2018-12-01,8",
                    "Postdoc,Turku Bioscience,Finland,2018-11-01,2019-12-01,4",
                    sep = "\n"
                  )),

    hr(),

    actionButton("generate", "Generate network",
                 class = "btn-primary w-100",
                 icon  = icon("network-wired")),
    br(), br(),
    uiOutput("download_ui"),
    br(),
    verbatimTextOutput("status_log")
  ),

  # Main panel
  uiOutput("main_content")
)


# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  result_html <- reactiveVal(NULL)
  log_msgs    <- reactiveVal(character(0))

  log <- function(msg) {
    log_msgs(c(log_msgs(), paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)))
  }

  output$status_log <- renderText({
    paste(tail(log_msgs(), 8), collapse = "\n")
  })

  # Show landing state before generation
  output$main_content <- renderUI({
    if (is.null(result_html())) {
      div(
        style = "text-align:center; padding:4rem 2rem; color:#888780;",
        tags$i(class = "fa fa-project-diagram fa-3x",
               style = "margin-bottom:1.5rem; color:#D3D1C7;"),
        h4("Enter your details and click Generate",
           style = "font-weight:400; color:#5F5E5A;"),
        p("Your personalised research network will appear here."),
        br(),
        p(style = "font-size:13px;",
          "Try it with the example: Scholar ID ",
          tags$code("sIxWbx0AAAAJ"),
          " and the ",
          tags$a("example PDF", href = "example/Santosh_Publications.pdf",
                 target = "_blank"), ".")
      )
    } else {
      tagList(
        tags$iframe(
          id     = "result_frame",
          src    = session$clientData$url_pathname,   # served below
          width  = "100%",
          height = "950px",
          style  = "border:none;"
        )
      )
    }
  })

  observeEvent(input$generate, {

    req(input$scholar_id, input$pub_pdf)
    result_html(NULL)

    withProgress(message = "Building your network...", value = 0, {

      tryCatch({

        # 1 — Parse PDF
        log("Parsing publication PDF...")
        setProgress(0.1, detail = "Parsing PDF")
        pub_data <- parse_publications(
          input$pub_pdf$datapath,
          your_name = input$your_name
        )

        # 2 — Fetch Scholar profile
        log("Fetching Google Scholar data...")
        setProgress(0.25, detail = "Google Scholar")
        scholar_pubs    <- fetch_scholar_pubs(input$scholar_id)
        scholar_profile <- fetch_scholar_profile(input$scholar_id)

        # Merge citations from Scholar into parsed pub_data
        if (!is.null(scholar_pubs)) {
          pub_data <- pub_data |>
            left_join(
              scholar_pubs |> select(title, cites) |>
                mutate(title = str_to_title(str_trim(title))),
              by = "title"
            ) |>
            mutate(
              citations = ifelse(!is.na(cites), cites, 0L),
              pmid      = NA_character_
            )
        } else {
          pub_data <- pub_data |>
            mutate(citations = 0L, pmid = NA_character_)
        }

        # 3 — Build co-author data from PDF parse
        log("Extracting co-author institutions...")
        setProgress(0.4, detail = "Co-author data")
        coauthor_data <- extract_coauthors(pub_data, input$your_name)

        # 4 — Build world map
        log("Geocoding institutions...")
        setProgress(0.55, detail = "World map")
        world_map <- build_map(
          coauthor_institutions = coauthor_data,
          home_inst = ifelse(nchar(input$your_inst) > 0,
                             input$your_inst, "Current institution")
        )

        # 5 — Build network
        log("Building co-author network...")
        setProgress(0.65, detail = "Network graph")

        your_name <- ifelse(nchar(input$your_name) > 0,
                            input$your_name, "You")

        pub_counts <- coauthor_data |>
          count(author, institution, country, name = "shared_pubs")

        network_plot <- build_network(
          pub_counts       = pub_counts,
          your_name        = your_name,
          your_inst        = ifelse(nchar(input$your_inst) > 0,
                                   input$your_inst, "Current institution"),
          career_countries = COUNTRY_COLORS[
            unique(coauthor_data$country)[
              unique(coauthor_data$country) %in% names(COUNTRY_COLORS)
            ][1:min(4, length(unique(coauthor_data$country)))]
          ]
        )

        # 6 — Publication timeline
        log("Building publication timeline...")
        setProgress(0.75, detail = "Timeline")
        timeline <- build_pub_timeline(pub_data)

        # 7 — Career Gantt
        career_fig <- NULL
        if (nchar(str_trim(input$career_csv)) > 0) {
          log("Building career Gantt...")
          career_data <- read_career_csv(input$career_csv)
          if (!is.null(career_data)) {
            career_fig <- build_career_gantt(career_data)
          }
        }

        # 8 — Publication table
        log("Building publication table...")
        setProgress(0.85, detail = "Publication table")
        pub_table <- build_pub_table(pub_data)

        # 9 — Assemble page
        log("Assembling HTML page...")
        setProgress(0.95, detail = "Assembling")

        total_cites  <- sum(pub_data$citations, na.rm = TRUE)
        n_countries  <- n_distinct(coauthor_data$country)
        years_active <- max(pub_data$year, na.rm = TRUE) -
                        min(pub_data$year, na.rm = TRUE)

        page <- build_page(
          name         = scholar_profile$name %||% your_name,
          n_pubs       = nrow(pub_data),
          n_coauthors  = nrow(pub_counts),
          n_countries  = n_countries,
          total_cites  = total_cites,
          years_active = years_active,
          career_fig   = career_fig %||%
                         tags$p(class = "note",
                                "No career data provided — add via the sidebar."),
          timeline     = timeline,
          pub_table    = pub_table,
          world_map    = world_map,
          network_plot = network_plot
        )

        result_html(page)
        log("Done! Scroll down to see your network. Download button active.")
        setProgress(1)

      }, error = function(e) {
        log(paste("Error:", conditionMessage(e)))
        showNotification(
          paste("Error:", conditionMessage(e)),
          type     = "error",
          duration = 15
        )
      })
    })
  })

  # Download handler
  output$download_ui <- renderUI({
    req(result_html())
    downloadButton("download_html", "Download HTML",
                   class = "btn-outline-primary w-100")
  })

  output$download_html <- downloadHandler(
    filename = function() "research_network.html",
    content  = function(file) {
      save_html(result_html(), file = file)
    }
  )

  # Serve the generated HTML for the iframe
  output$result_page <- renderUI({ result_html() })
}


# ── Helpers ───────────────────────────────────────────────────────────────────

#' Extract co-author institution data from parsed publications
#' Returns a minimal tribble — institution/country guessed from common patterns
extract_coauthors <- function(pub_data, your_name = "You") {
  if (!"authors" %in% names(pub_data)) return(tibble())

  # Split author strings and flatten
  all_authors <- pub_data |>
    filter(!is.na(authors)) |>
    mutate(author_list = str_split(authors, ",\\s*")) |>
    unnest(author_list) |>
    rename(author = author_list) |>
    mutate(author = str_trim(author)) |>
    filter(
      nchar(author) > 2,
      !str_detect(str_to_lower(author), str_to_lower(your_name %||% "xxxxx"))
    ) |>
    count(author, name = "shared_pubs") |>
    # Without affiliation data we can't geocode — return minimal structure
    mutate(
      institution = "Unknown",
      country     = "Unknown"
    )

  all_authors
}


#' Parse career history from CSV text input
read_career_csv <- function(csv_text) {
  tryCatch({
    lines <- str_trim(str_split(csv_text, "\n")[[1]])
    lines <- lines[nchar(lines) > 0]
    rows  <- map(lines, ~str_split(.x, ",")[[1]])
    rows  <- rows[map_lgl(rows, ~length(.x) >= 6)]
    if (length(rows) == 0) return(NULL)

    tibble(
      role    = map_chr(rows, ~str_trim(.x[1])),
      org     = map_chr(rows, ~str_trim(.x[2])),
      country = map_chr(rows, ~str_trim(.x[3])),
      start   = map_chr(rows, ~str_trim(.x[4])),
      end     = map_chr(rows, ~str_trim(.x[5])),
      pubs    = map_int(rows, ~as.integer(str_trim(.x[6])))
    )
  }, error = function(e) {
    warning("Could not parse career CSV: ", conditionMessage(e))
    NULL
  })
}

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
