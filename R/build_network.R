# =============================================================================
# build_network.R
# Builds an interactive co-author network using visNetwork.
# Node size = shared publications. Edge width = collaboration strength.
# Centre node rendered as a career-journey pie SVG.
# =============================================================================

library(visNetwork)
library(dplyr)
library(stringr)
library(purrr)
library(htmlwidgets)

source("R/globals.R")

#' Build interactive co-author network
#'
#' @param pub_counts  tibble: author, shared_pubs, institution, country
#' @param your_name   Display name for centre node (e.g. "SD Bhosale")
#' @param your_inst   Current institution
#' @param career_countries Named vector of career countries for pie SVG
#'                    e.g. c("India"="#F5C400","Finland"="#1D9E75",...)
#' @return A visNetwork widget
build_network <- function(pub_counts,
                          your_name        = "You",
                          your_inst        = "Current institution",
                          career_countries = COUNTRY_COLORS[c("India","Finland",
                                                              "Denmark","USA")]) {

  # Centre node
  you_node <- tibble(
    author      = your_name,
    shared_pubs = NA_integer_,
    institution = your_inst,
    country     = names(career_countries)[length(career_countries)]
  )

  all_nodes_raw <- bind_rows(you_node, pub_counts)

  # Build career SVG for centre node
  career_svg <- make_career_svg(career_countries, your_name)

  nodes <- all_nodes_raw |>
    mutate(
      id          = row_number(),
      label       = author,
      shared_pubs = ifelse(is.na(shared_pubs), 0L, as.integer(shared_pubs)),
      title       = paste0(
        "<b>", author, "</b><br>",
        institution, "<br>", country,
        ifelse(author == your_name, "",
               paste0("<br><i>", shared_pubs, " shared paper(s)</i>"))
      ),
      value       = ifelse(author == your_name, 100,
                           pmax(20, shared_pubs * 10)),
      color       = ifelse(author == your_name, "#D85A30",
                           unname(COUNTRY_COLORS[country])),
      color       = ifelse(is.na(color), "#888780", color),
      font.size   = ifelse(author == your_name, 20, 13),
      font.color  = "#2C2C2A",
      shape       = ifelse(author == your_name, "image", "dot"),
      image       = ifelse(author == your_name,
                           paste0("data:image/svg+xml,", career_svg),
                           NA_character_),
      size        = ifelse(author == your_name, 45,
                           pmax(14, shared_pubs * 3.5))
    )

  your_id <- nodes |> filter(author == your_name) |> pull(id)

  edges <- nodes |>
    filter(author != your_name) |>
    mutate(
      from  = your_id,
      to    = id,
      width = pmax(1, shared_pubs),
      title = paste0(shared_pubs, " shared paper(s)")
    ) |>
    select(from, to, width, title)

  visNetwork(
    nodes = nodes |> select(id, label, title, value, color,
                            font.size, font.color, shape, image, size),
    edges = edges,
    width  = "100%",
    height = "820px"
  ) |>
    visOptions(
      highlightNearest  = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection  = list(enabled = TRUE, main = "Search co-author")
    ) |>
    visPhysics(
      solver    = "barnesHut",
      barnesHut = list(
        gravitationalConstant = -5000,
        centralGravity        = 0.4,
        springLength          = 120,
        springConstant        = 0.04,
        damping               = 0.2,
        avoidOverlap          = 0.8
      ),
      maxVelocity   = 50,
      minVelocity   = 1.0,
      stabilization = list(enabled = TRUE, iterations = 500,
                           updateInterval = 25, fit = TRUE)
    ) |>
    visLayout(randomSeed = 42, improvedLayout = TRUE) |>
    visInteraction(
      navigationButtons = TRUE, tooltipDelay = 80,
      hover = TRUE, zoomSpeed = 0.5,
      multiselect = TRUE, hideEdgesOnDrag = TRUE, keyboard = TRUE
    ) |>
    visNodes(shape = "dot", borderWidth = 2, shadow = FALSE,
             font = list(size = 13, strokeWidth = 3, strokeColor = "#ffffff")) |>
    visEdges(
      color   = list(color = "#D3D1C7", highlight = "#D85A30", hover = "#D85A30"),
      smooth  = list(enabled = TRUE, type = "curvedCW", roundness = 0.2),
      scaling = list(min = 1, max = 6)
    ) |>
    visEvents(stabilizationIterationsDone = JS(toolbar_js()))
}


#' Generate career-journey pie SVG for centre node
make_career_svg <- function(career_countries, initials = "You") {
  n      <- length(career_countries)
  cols   <- unname(career_countries)
  angle  <- 2 * pi / n
  cx     <- 40; cy <- 40; r <- 38

  paths <- map_chr(seq_len(n), function(i) {
    a1 <- (i - 1) * angle - pi / 2
    a2 <- i       * angle - pi / 2
    x1 <- cx + r * cos(a1); y1 <- cy + r * sin(a1)
    x2 <- cx + r * cos(a2); y2 <- cy + r * sin(a2)
    sprintf('<path d="M%g,%g L%g,%g A%g,%g 0 0,1 %g,%g Z" fill="%s"/>',
            cx, cy, x1, y1, r, r, x2, y2, cols[i])
  })

  ini <- paste0(substr(strsplit(initials, "[ .]")[[1]], 1, 1), collapse = "")
  ini <- substr(ini, 1, 3)

  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="80" height="80" viewBox="0 0 80 80">',
    paste(paths, collapse = ""),
    '<circle cx="40" cy="40" r="38" fill="none" stroke="white" stroke-width="2.5"/>',
    '<circle cx="40" cy="40" r="16" fill="white" fill-opacity="0.9"/>',
    '<text x="40" y="45" text-anchor="middle" font-size="10" ',
    'font-family="sans-serif" font-weight="bold" fill="#2C2C2A">', ini, '</text>',
    '</svg>'
  )
  URLencode(svg, reserved = FALSE)
}


#' JS string for layout switcher toolbar
toolbar_js <- function() {
  "function () {
    var network = this;
    var container = this.body.container;
    var bar = document.createElement('div');
    bar.style.cssText = [
      'position:absolute','top:10px','left:50%','transform:translateX(-50%)',
      'display:flex','gap:6px','z-index:10','background:rgba(255,255,255,0.95)',
      'border:0.5px solid #D3D1C7','border-radius:8px','padding:5px 10px',
      'font-family:-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif',
      'font-size:11px','box-shadow:0 1px 6px rgba(0,0,0,0.1)','align-items:center'
    ].join(';');
    var layouts = [
      { label: 'Barnes-Hut', solver: 'barnesHut',
        opts: { gravitationalConstant:-5000, centralGravity:0.4,
                springLength:120, springConstant:0.04, damping:0.2, avoidOverlap:0.8 } },
      { label: 'Hierarchical', solver: null }
    ];
    layouts.forEach(function(l, i) {
      var btn = document.createElement('button');
      btn.textContent = l.label;
      btn.style.cssText = [
        'padding:3px 10px','border-radius:5px','cursor:pointer',
        'font-size:11px','font-family:inherit',
        i === 0 ? 'background:#D85A30;color:#fff;border:none;'
                : 'background:transparent;color:#5F5E5A;border:0.5px solid #D3D1C7;'
      ].join(';');
      btn.onclick = function() {
        bar.querySelectorAll('button').forEach(function(b) {
          b.style.background='transparent'; b.style.color='#5F5E5A';
          b.style.border='0.5px solid #D3D1C7';
        });
        btn.style.background='#D85A30'; btn.style.color='#fff'; btn.style.border='none';
        if (l.solver === null) {
          network.setOptions({ layout:{ hierarchical:{ enabled:true, direction:'UD',
            sortMethod:'hubsize', nodeSpacing:100, levelSeparation:130 }},
            physics:{ enabled:false }});
          setTimeout(function(){ network.fit({ animation:true }); }, 300);
        } else {
          network.setOptions({ layout:{ hierarchical:{ enabled:false }},
            physics:{ enabled:true, solver:l.solver, [l.solver]:l.opts,
              stabilization:{ iterations:300, fit:true }}});
          network.startSimulation();
        }
      };
      bar.appendChild(btn);
    });
    var fitBtn = document.createElement('button');
    fitBtn.textContent = '↺ Fit';
    fitBtn.style.cssText = ['padding:3px 10px','border-radius:5px','cursor:pointer',
      'font-size:11px','font-family:inherit',
      'background:transparent;color:#5F5E5A;border:0.5px solid #D3D1C7;',
      'margin-left:4px'].join(';');
    fitBtn.onclick = function() {
      network.fit({ animation:{ duration:500, easingFunction:'easeInOutQuad' }});
    };
    bar.appendChild(fitBtn);
    container.style.position='relative';
    container.appendChild(bar);
    network.fit({ animation:{ duration:800, easingFunction:'easeInOutQuad' }});
  }"
}
