# Research Network Visualiser

An R-based tool that generates a **personalised interactive research network page** from your Google Scholar profile and publication list PDF — no coding required.

[![Live demo](https://img.shields.io/badge/Live%20demo-santoshdbhosale.github.io-D85A30?style=flat-square)](https://santoshdbhosale.github.io/network/)
[![R](https://img.shields.io/badge/Built%20with-R-276DC3?style=flat-square&logo=r)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-app-blue?style=flat-square)](https://shiny.posit.co/)

---

## What it generates

| Widget | Description |
|---|---|
| **Career timeline** | Horizontal Gantt chart of your roles, coloured by country |
| **Publication timeline** | Stacked bar chart by year and research theme |
| **Publication table** | Searchable table with PubMed links |
| **Global collaboration map** | Leaflet world map of co-author institutions |
| **Co-author network** | Interactive graph — node size = shared papers |

All widgets are interactive (hover, zoom, click, search) and assembled into a single self-contained HTML file you can host anywhere.

---

## Use it yourself

### Option 1 — Shiny app (no coding needed)

🚀 **[Launch the app](https://santoshdbhosale.shinyapps.io/research-network)**

1. Enter your **Google Scholar ID**
   - Find it in your profile URL: `scholar.google.com/citations?user=`**`XXXXXXXX`**
2. **Upload your publication list** as a PDF
   - Any CV or publication list format works — see the [example PDF](example/Santosh_Publications.pdf)
3. Optionally paste your **career history** as CSV (role, org, country, start, end, pubs)
4. Click **Generate** and wait ~60 seconds
5. **Download your HTML** and host it anywhere

### Option 2 — Run locally in R

```r
# 1. Clone the repo
git clone https://github.com/santoshdbhosale/research-network.git
cd research-network

# 2. Install dependencies (run once)
install.packages(c(
  "shiny", "bslib", "scholar", "leaflet", "visNetwork",
  "tidygeocoder", "plotly", "DT", "pdftools",
  "dplyr", "stringr", "htmltools", "htmlwidgets", "purrr"
))

# 3. Launch
shiny::runApp()
```

### Option 3 — Script only (advanced)

Edit `example/build_example.R` with your own data and run it directly without Shiny.

---

## PDF format

The parser handles the most common publication list formats:

```
# Format 1 — year as heading, papers below
2024
Title of paper. Author A, Author B, Author C. Journal Name. 2024.

2023
Title of another paper. Author A, Author D. Journal. 2023.

# Format 2 — numbered list
1. Title. Authors. Journal. Year.
2. Title. Authors. Journal. Year.

# Format 3 — pipe-separated
Title | Authors | Journal | Year
```

> **Tip:** Export from your reference manager (Zotero, Mendeley, EndNote) as a plain PDF for best results.

---

## Career CSV format

Paste directly into the sidebar text area — one role per line:

```
PhD Researcher,University of Turku,Finland,2012-01-01,2018-12-01,8
Postdoctoral Researcher,Turku Bioscience,Finland,2018-11-01,2019-12-01,4
Postdoctoral Researcher,Univ. of Southern Denmark,Denmark,2020-01-01,2022-12-01,5
Associate Biomedical Scientist,Cedars-Sinai Medical Center,USA,2023-02-01,2024-06-01,2
```

Columns: `role, organisation, country, start (YYYY-MM-DD), end (YYYY-MM-DD), publications`

---

## Repository structure

```
research-network/
├── app.R                     # Shiny application
├── R/
│   ├── globals.R             # Shared colour palettes and constants
│   ├── parse_publications.R  # PDF parser
│   ├── fetch_scholar.R       # Google Scholar API wrapper
│   ├── build_map.R           # Leaflet world map
│   ├── build_network.R       # visNetwork co-author graph
│   ├── build_timeline.R      # Plotly timeline + Gantt
│   ├── build_table.R         # DT publication table
│   └── build_page.R          # HTML page assembly
├── example/
│   ├── Santosh_Publications.pdf   # Example input PDF
│   └── build_example.R            # Script to regenerate the demo
└── README.md
```

---

## Design decisions

- **Barnes-Hut layout** for the co-author network: hub-spoke topologies (all nodes connect to one centre) need central gravity to stay readable. forceAtlas2 with zero gravity scatters 127 nodes off-screen at this scale.
- **Publication-time affiliations**: co-author institution reflects where they were when the paper was published, not their current position. This matters for map accuracy.
- **Shared colour palette** (`globals.R`): map markers, network nodes, and career Gantt bars all use the same country colours — a visitor can cross-reference between widgets without a separate legend for each.
- **One HTML file**: all widgets are bundled via `htmltools::save_html()`. No server needed to host the output — works on GitHub Pages, Netlify, or any static host.

---

## Tech stack

| Tool | Purpose |
|---|---|
| `scholar` | Google Scholar API |
| `pdftools` | PDF text extraction |
| `tidygeocoder` | OSM institution geocoding |
| `leaflet` | Interactive world map |
| `visNetwork` | Co-author network graph |
| `plotly` | Timeline + Gantt charts |
| `DT` | Interactive publication table |
| `htmltools` | HTML page assembly |
| `shiny` + `bslib` | Web application |

---

## Author

**Santosh D. Bhosale** — Proteomics Scientist · AI/ML Drug Discovery · Product Manager

[santoshdbhosale.github.io](https://santoshdbhosale.github.io) · [Google Scholar](https://scholar.google.com/citations?user=sIxWbx0AAAAJ) · [LinkedIn](https://www.linkedin.com/in/santoshdbhosale/)

---

## Licence

MIT — free to use, adapt, and share with attribution.
