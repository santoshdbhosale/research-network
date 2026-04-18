# =============================================================================
# example/build_example.R
# Generates the live demo output for santoshdbhosale.github.io/network/
# using Santosh's verified publication and career data.
#
# Run from the repo root: source("example/build_example.R")
# Output: network/research_network.html
# =============================================================================

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

source("R/globals.R")
source("R/fetch_scholar.R")
source("R/build_map.R")
source("R/build_network.R")
source("R/build_timeline.R")
source("R/build_table.R")
source("R/build_page.R")

library(dplyr)
library(tibble)
library(htmltools)

# ── 1. Verified publication data ----------------------------------------------
pub_data <- tribble(
  ~year, ~title,                                                                       ~journal,               ~theme,            ~citations, ~pmid,
  2024,  "Comparative proteomics of mouse mini-gut organoid: celiac disease markers",  "Front. Mol. Biosci",   "Biomarkers",       8,         "39263374",
  2024,  "Inflection Point in High-Throughput Proteomics with Orbitrap Astral",        "J Proteome Res",       "Methods",         22,         "39163279",
  2024,  "Serum proteomics of mother-infant dyads: HLA-conferred T1D risk",            "iScience",             "Biomarkers",       6,         "38883825",
  2023,  "HIC1 interacts with FOXP3: regulatory T cell differentiation",               "Immunol Lett",         "T cell biology",   5,         "37838026",
  2023,  "Serum APOC1 decreased in children progressing to type 1 diabetes",           "Sci Rep",              "Biomarkers",      12,         "37743383",
  2023,  "Cardiovascular proteomic changes in ECFCs: COVID-19 patients",               "Int J Biol Sci",       "COVID-19",        18,         "37063416",
  2022,  "FOSL1, FOSL2 and BATF transcriptional regulation in Th17 differentiation",   "Nucleic Acids Res",    "T cell biology",  14,         "35511484",
  2022,  "Phosphoproteomics: Methods and Challenges",                                  "Ref Module Life Sci",  "Methods",          9,         "sciencedirect",
  2021,  "HDL proteome remodeling associates with COVID-19 severity",                  "J Clin Lipidol",       "COVID-19",        52,         "34802985",
  2021,  "Interactome Networks of FOSL1 and FOSL2 in Human Th17 Cells",               "ACS Omega",            "T cell biology",  19,         "34604665",
  2020,  "CIP2A Constrains Th17 Differentiation via STAT3 Signaling",                 "iScience",             "T cell biology",  24,         "32171124",
  2020,  "Protein interactome of CIP2A in Th17 cells",                                "Curr Res Immunol",     "T cell biology",  11,         "33817627",
  2019,  "Quantitative Proteomics: Dynamic Protein Landscape in Th17 Polarization",   "iScience",             "T cell biology",  38,         "30641411",
  2018,  "Serum Proteomic Profiling: Biomarkers of Carotid Atherosclerosis",          "Sci Rep",              "Biomarkers",      31,         "29907817",
  2018,  "Quantitative proteomic comparison of T helper 17 and iTreg cells",          "PLoS Biol",            "T cell biology",  44,         "29851958",
  2018,  "Plasma proteome analysis using iTRAQ and TMT isobaric labeling",            "Mass Spectrom Rev",    "Methods",        107,         "29120501",
  2017,  "Mass Spectrometry-Based Serum Proteomics for Biomarker Discovery",          "Methods Mol Biol",     "Methods",         29,         "28674903",
  2017,  "Progress and potential of proteomic biomarkers for T1D in children",        "Expert Rev Proteomics","Biomarkers",      35,         "27997253",
  2015,  "Serum proteomes distinguish children developing T1D: HLA-risk cohort",      "Diabetes",             "Biomarkers",      90,         "25616278",
  2013,  "Proteome-wide reduction in AGE modification in diabetic mice",              "Sci Rep",              "Biomarkers",      42,         "24126953",
  2012,  "Zoom-In: targeted database search for glycation modifications",             "Eur J Mass Spectrom",  "Methods",         28,         "23654192",
  2011,  "Gatifloxacin deregulates enzymes in glucose metabolism",                    "J Toxicol Sci",        "Biomarkers",      23,         "22129742"
)

# ── 2. Co-author institution data ---------------------------------------------
coauthor_institutions <- tribble(
  ~author,              ~institution,                                        ~country,
  "R Lahesmaa",         "Turku Bioscience, University of Turku",             "Finland",
  "R Moulder",          "Turku Bioscience, University of Turku",             "Finland",
  "DR Goodlett",        "University of Maryland",                            "USA",
  "LL Elo",             "University of Turku",                               "Finland",
  "MR Larsen",          "University of Southern Denmark",                    "Denmark",
  "T Kang",             "University of Southern Denmark",                    "Denmark",
  "A Moradian",         "Cedars-Sinai Medical Center",                       "USA",
  "SM Mockus",          "Cedars-Sinai Medical Center",                       "USA",
  "JE Van Eyk",         "Cedars-Sinai Medical Center",                       "USA",
  "MJ Kulkarni",        "CSIR-National Chemical Laboratory",                 "India",
  "SK Kesavan",         "CSIR-National Chemical Laboratory",                 "India",
  "SK Tripathi",        "Turku Bioscience, University of Turku",             "Finland",
  "A Shetty",           "Turku Bioscience, University of Turku",             "Finland",
  "MM Khan",            "Turku Bioscience, University of Turku",             "Finland",
  "L Beltran-Camacho",  "University of Cadiz",                               "Spain",
  "MC Duran-Ruiz",      "University of Cadiz",                               "Spain",
  "D Sanchez-Morillo",  "University of Cadiz",                               "Spain",
  "MR Moreno-Luna",     "University of Cadiz",                               "Spain",
  "GE Ronseik",         "University of Sao Paulo",                           "Brazil",
  "G Palmisano",        "University of Sao Paulo",                           "Brazil",
  "M Oresic",           "University of Turku",                               "Finland",
  "M Knip",             "University of Helsinki",                            "Finland",
  "J Ilonen",           "University of Turku",                               "Finland",
  "R Veijola",          "University of Oulu",                                "Finland",
  "O Raitakari",        "University of Turku",                               "Finland",
  "I Mohammad",         "Turku Bioscience, University of Turku",             "Finland",
  "SBA Andrabi",        "Turku Bioscience, University of Turku",             "Finland"
)

# ── 3. Verified co-author counts ---------------------------------------------
pub_counts_verified <- tribble(
  ~author,              ~shared_pubs, ~institution,                                   ~country,
  "Moulder R",          15,           "Turku Bioscience, University of Turku",        "Finland",
  "Lahesmaa R",         14,           "Turku Bioscience, University of Turku",        "Finland",
  "Elo LL",              6,           "University of Turku",                          "Finland",
  "Goodlett DR",         6,           "University of Maryland",                       "USA",
  "Rasool O",            5,           "Turku Bioscience, University of Turku",        "Finland",
  "Khan MM",             4,           "Turku Bioscience, University of Turku",        "Finland",
  "Ilonen J",            3,           "University of Turku",                          "Finland",
  "Knip M",              3,           "University of Helsinki",                       "Finland",
  "Khan MH",             3,           "Turku Bioscience, University of Turku",        "Finland",
  "Buchacher T",         3,           "Turku Bioscience, University of Turku",        "Finland",
  "Lähdesmäki H",        3,           "Aalto University",                             "Finland",
  "Larsen MR",           3,           "University of Southern Denmark",               "Denmark",
  "Shetty A",            3,           "Turku Bioscience, University of Turku",        "Finland",
  "Tripathi SK",         3,           "Turku Bioscience, University of Turku",        "Finland",
  "Galande S",           3,           "Indian Institute of Science Education",        "India",
  "Välikangas T",        3,           "University of Turku",                          "Finland",
  "Komsi E",             3,           "Turku Bioscience, University of Turku",        "Finland",
  "Kulkarni MJ",         3,           "CSIR-National Chemical Laboratory",            "India",
  "Vähä-Mäkilä M",       2,           "University of Turku",                          "Finland",
  "Toppari J",           2,           "University of Turku",                          "Finland",
  "Veijola R",           2,           "University of Oulu",                           "Finland",
  "Hyöty H",             2,           "University of Tampere",                        "Finland",
  "Biradar R",           2,           "Turku Bioscience, University of Turku",        "Finland",
  "Chen Z",              2,           "University of Turku",                          "Finland",
  "Westermarck J",       2,           "Turku Bioscience, University of Turku",        "Finland",
  "Kesavan SK",          2,           "CSIR-National Chemical Laboratory",            "India",
  "Thulasiram HV",       2,           "CSIR-National Chemical Laboratory",            "India",
  "Kalim UU",            3,           "Turku Bioscience, University of Turku",        "Finland",
  "Moulder R",           15,          "Turku Bioscience, University of Turku",        "Finland"
) |> distinct(author, .keep_all = TRUE)

# ── 4. Career data -----------------------------------------------------------
career_data <- tribble(
  ~role,                           ~org,                             ~country,  ~start,        ~end,          ~pubs,
  "Lecturer",                      "JSPMS JSCOPR, Pune",             "India",   "2008-07-01",  "2009-11-01",  0,
  "Project Assistant",             "NCL Pune",                       "India",   "2009-11-01",  "2011-12-01",  3,
  "PhD Researcher",                "University of Turku",            "Finland", "2012-01-01",  "2018-12-01",  8,
  "Postdoctoral Researcher",       "Turku Bioscience",               "Finland", "2018-11-01",  "2019-12-01",  4,
  "Postdoctoral Researcher",       "Univ. of Southern Denmark",      "Denmark", "2020-01-01",  "2022-12-01",  5,
  "Associate Biomedical Scientist","Cedars-Sinai Medical Center",    "USA",     "2023-02-01",  "2024-06-01",  2,
  "Associate Scientific Manager",  "Partex.ai, Pune",                "India",   "2024-08-01",  "2024-11-01",  0,
  "Product Manager I",             "Partex.ai, Pune",                "India",   "2025-01-01",  "2026-04-01",  0
)

# ── 5. Build all widgets ------------------------------------------------------
message("Building world map...")
world_map <- build_map(
  coauthor_institutions = coauthor_institutions,
  home_lat  = 18.5204,
  home_lon  = 73.8567,
  home_inst = "Partex.ai"
)

message("Building co-author network...")
network_plot <- build_network(
  pub_counts       = pub_counts_verified,
  your_name        = "SD Bhosale",
  your_inst        = "Partex.ai",
  career_countries = COUNTRY_COLORS[c("India", "Finland", "Denmark", "USA")]
)

message("Building timelines...")
timeline   <- build_pub_timeline(pub_data)
career_fig <- build_career_gantt(career_data)

message("Building publication table...")
pub_table <- build_pub_table(pub_data)

# ── 6. Assemble and write ----------------------------------------------------
message("Assembling page...")
page <- build_page(
  name         = "Santosh D. Bhosale",
  n_pubs       = nrow(pub_data),
  n_coauthors  = nrow(pub_counts_verified),
  n_countries  = n_distinct(coauthor_institutions$country),
  total_cites  = sum(pub_data$citations),
  years_active = 14,
  career_fig   = career_fig,
  timeline     = timeline,
  pub_table    = pub_table,
  world_map    = world_map,
  network_plot = network_plot
)

out_dir  <- "network"
out_file <- file.path(out_dir, "research_network.html")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
save_html(page, file = out_file, libdir = file.path(out_dir, "libs"))

message(paste("Done! Written to:", out_file))
