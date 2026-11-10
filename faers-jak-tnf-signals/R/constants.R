# Locked choices for the JAK versus TNF FAERS screen.
# Counts come from the openFDA drug/event endpoint. The search text is
# built in queries.R from the vectors below.

API_ENDPOINT <- "https://api.fda.gov/drug/event.json"
OPENFDA_DOCS_URL <- "https://open.fda.gov/apis/drug/event/"
FAERS_LANDING_URL <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"
ASCII_ZIP_URL <- "https://fis.fda.gov/content/Exports/faers_ascii_%s.zip"

# Normal 95% quantile used for the reporting-odds interval. These are
# report counts, not a sample from a survey design.
Z_95 <- 1.96

WINDOW_START <- "20190101"
WINDOW_END <- "20260930"
WARNING_DATE <- "20210901"

PERIODS <- list(
  full = c("20190101", "20260930"),
  pre = c("20190101", "20210831"),
  post = c("20210901", "20260930")
)

PERIOD_LABELS <- c(
  full = "Full window",
  pre = "Before 1 September 2021",
  post = "On or after 1 September 2021"
)

# Search token, originator brand token, and the name used in the write-up.
# The brand is included because the harmonized generic name misses a large
# share of Xeljanz and Olumiant reports. Salt forms and biosimilars that
# carry the ingredient token are included with it.
DRUGS <- list(
  upa = list(ingredient = "upadacitinib", brand = "rinvoq", label = "Upadacitinib", group = "jak"),
  tofa = list(ingredient = "tofacitinib", brand = "xeljanz", label = "Tofacitinib", group = "jak"),
  bari = list(ingredient = "baricitinib", brand = "olumiant", label = "Baricitinib", group = "jak"),
  ada = list(ingredient = "adalimumab", brand = "humira", label = "Adalimumab", group = "tnf"),
  inf = list(ingredient = "infliximab", brand = "remicade", label = "Infliximab", group = "tnf"),
  eta = list(ingredient = "etanercept", brand = "enbrel", label = "Etanercept", group = "tnf"),
  cert = list(ingredient = "certolizumab", brand = "cimzia", label = "Certolizumab pegol", group = "tnf"),
  goli = list(ingredient = "golimumab", brand = "simponi", label = "Golimumab", group = "tnf")
)

JAK_IDS <- c("upa", "tofa", "bari")
TNF_IDS <- c("ada", "inf", "eta", "cert", "goli")

# Phrase matches on the reported indication. FAERS stores the Crohn's term
# as Crohn^s disease. A caret is not a legal openFDA search character, and
# the token "crohn" is the match for that term (see queries.R).
INDICATION_PHRASES <- c(
  "rheumatoid arthritis",
  "psoriatic arthropathy",
  "psoriatic arthritis",
  "ankylosing spondylitis",
  "colitis ulcerative",
  "psoriasis",
  "dermatitis atopic"
)
CROHN_INDICATION_TOKEN <- "crohn"

# Venous thromboembolism screen. "thrombosis" is the nonspecific preferred
# term and is not limited to veins. It is included because the protocol
# names it.
VTE_PTS <- c(
  "deep vein thrombosis",
  "pulmonary embolism",
  "venous thrombosis",
  "thrombosis",
  "embolism venous"
)

# Custom infection preferred terms. This is not a MedDRA SMQ. A report
# counts only if one of these exact terms is present and the report is
# serious (serious:1).
INFECTION_PTS <- c(
  "pneumonia",
  "pneumonia bacterial",
  "pneumonia viral",
  "pneumonia pneumococcal",
  "covid-19 pneumonia",
  "covid-19",
  "sepsis",
  "septic shock",
  "cellulitis",
  "urinary tract infection",
  "herpes zoster",
  "influenza",
  "diverticulitis",
  "pyelonephritis",
  "pyelonephritis acute",
  "osteomyelitis",
  "tuberculosis",
  "pulmonary tuberculosis",
  "disseminated tuberculosis",
  "infection",
  "bacterial infection",
  "viral infection",
  "fungal infection",
  "opportunistic infection",
  "pneumocystis jirovecii pneumonia",
  "lower respiratory tract infection",
  "respiratory tract infection",
  "candidiasis",
  "oral candidiasis",
  "oesophageal candidiasis",
  "aspergillosis",
  "cytomegalovirus infection",
  "herpes simplex",
  "bacteraemia",
  "bacteremia",
  "urosepsis",
  "meningitis",
  "endocarditis",
  "appendicitis",
  "abscess",
  "clostridium difficile colitis",
  "clostridium difficile infection",
  "staphylococcal infection",
  "streptococcal infection",
  "necrotising fasciitis",
  "necrotizing fasciitis",
  "arthritis bacterial"
)

COMPARISONS <- list(
  list(id = "jak_vs_tnf", label = "All JAK inhibitors", exposed = "jak", both = "jak_and_tnf"),
  list(id = "upa_vs_tnf", label = "Upadacitinib", exposed = "upa", both = "upa_and_tnf"),
  list(id = "tofa_vs_tnf", label = "Tofacitinib", exposed = "tofa", both = "tofa_and_tnf"),
  list(id = "bari_vs_tnf", label = "Baricitinib", exposed = "bari", both = "bari_and_tnf")
)

OUTCOME_LABELS <- c(
  vte = "Venous thromboembolism",
  infection = "Serious infection"
)

OUTCOME_SENTENCE <- c(
  vte = "venous thromboembolism",
  infection = "serious infection"
)

# Bottom-to-top order on the forest plot.
DRUG_ORDER <- c("Baricitinib", "Tofacitinib", "Upadacitinib", "All JAK inhibitors")

COL <- list(
  ink = "#1c1917",
  muted = "#57534e",
  faint = "#e7e5e4",
  paper = "#ffffff",
  navy = "#0F4C5C",
  copper = "#C2410C",
  crimson = "#9F1239",
  tile = "#F4F1EC"
)

quarter_table <- function() {
  end_md <- c("0331", "0630", "0930", "1231")
  rows <- list()
  for (year in 2019:2026) {
    for (q in 1:4) {
      if (year == 2026 && q == 4) next
      start_month <- (q - 1L) * 3L + 1L
      mid_month <- start_month + 1L
      rows[[length(rows) + 1L]] <- data.frame(
        id = sprintf("%dQ%d", year, q),
        year = year,
        quarter = q,
        start = sprintf("%04d%02d01", year, start_month),
        end = sprintf("%04d%s", year, end_md[[q]]),
        mid = as.Date(sprintf("%04d-%02d-15", year, mid_month)),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

QUARTERS <- quarter_table()
