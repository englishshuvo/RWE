# Locked choices for the SEER blood-cancer trend study.
# Rates come from the public SEER*Explorer content endpoints.
# SEER*Stat case listings are not used.

EXPLORER_ORIGIN <- "https://seer.cancer.gov/statistics-network/explorer/"
EXPLORER_PAGE <- "https://seer.cancer.gov/statistics-network/explorer/application.html"
ARCHIVE_PAGE <- "https://seer.cancer.gov/statistics-network/explorer/archive.html"
REGION5_URL <- "https://seer.cancer.gov/statistics-network/explorer/source/content_writers/render_region_5.php"
CODEBOOK_URL <- "https://seer.cancer.gov/statistics-network/explorer/source/content_writers/get_var_formats.php"
FOOTNOTE_URL <- "https://seer.cancer.gov/statistics-network/explorer/source/content_writers/load_json_asset.php?asset=footnote-defines"
NOTES_URL <- "https://seer.cancer.gov/statistics-network/explorer/source/content_writers/load_json_asset.php?asset=content-notes"
SITE_DEFS_URL <- "https://seer.cancer.gov/statistics-network/explorer/cancer-sites.html"

# SEER does not publish a count below 16. This study uses the same cutoff
# and does not write the rate or the count for those cells.
MIN_COUNT <- 16L

# Diagnosis year left out of SEER's joinpoint fit. The rate is still shown.
COVID_YEAR <- 2020L

INCIDENCE_START <- 2000L
INCIDENCE_END <- 2023L

# Site codes from SEER*Explorer get_var_formats.php.
# Diffuse large B-cell lymphoma is not in that site list.
SITES <- list(
  myeloma = list(id = 89L, label = "Multiple myeloma", seer_label = "Myeloma", short = "Myeloma"),
  cll = list(id = 93L, label = "Chronic lymphocytic leukemia", seer_label = "Chronic Lymphocytic Leukemia (CLL)", short = "CLL"),
  nhl = list(id = 86L, label = "Non-Hodgkin lymphoma", seer_label = "Non-Hodgkin Lymphoma", short = "NHL")
)
SITE_ORDER <- c("myeloma", "cll", "nhl")

SEX_LABELS <- c("1" = "Both sexes", "2" = "Male", "3" = "Female")
RACE_LABELS <- c(
  "1" = "All races and ethnicities",
  "2" = "White (includes Hispanic)",
  "3" = "Black (includes Hispanic)",
  "4" = "Non-Hispanic Asian / Pacific Islander",
  "5" = "Non-Hispanic American Indian / Alaska Native",
  "6" = "Hispanic (any race)",
  "8" = "Non-Hispanic White",
  "9" = "Non-Hispanic Black"
)
AGE_LABELS <- c(
  "1" = "All ages",
  "9" = "Ages <50",
  "141" = "Ages 50-64",
  "157" = "Ages 65+"
)
RATE_TYPE_LABELS <- c(
  "1" = "Observed",
  "2" = "Delay-adjusted"
)

# Age bands kept because the survival trend offers these same bands.
# The incidence control also offers other bands. Those are not retained.
AGE_KEEP <- c(1L, 9L, 141L, 157L)
# Incidence race groups. 2 and 3 overlap Hispanic ethnicity. They are kept
# so the survival contrast has an incidence counterpart. 5 is PRCDA-limited
# and is often suppressed.
RACE_KEEP <- c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L)
SEX_KEEP <- c(1L, 2L, 3L)
# Survival race control only offers these three.
SURVIVAL_RACE_KEEP <- c(1L, 2L, 3L)

# Phrases that must be present in the footnote payload for this release.
# The pipeline stops if the live text no longer matches.
LOCK_RELEASE <- "November 2025 Submission (1975-2023)"
LOCK_INCIDENCE_REGISTRY <- "SEER 21 registries"
LOCK_SURVIVAL_REGISTRY <- "excluding Illinois"
LOCK_STD_POP <- "2000 US Std Population"
LOCK_JOINPOINT <- "Joinpoint Trend Analysis Software"
LOCK_JOINPOINT_VERSION <- "Version 6.0"
LOCK_AACS <- "average absolute change in survival"
LOCK_COVID <- "2020 incidence rate"

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

CANCER_COLORS <- c(
  myeloma = "#0F4C5C",
  cll = "#C2410C",
  nhl = "#9F1239"
)

# FDA dates used as context lines. on_figure is FALSE when the date falls
# outside the plotted diagnosis years or when the approval is recorded
# only so the write-up can cite it.
approval_table <- function() {
  data.frame(
    cancer = c(
      "myeloma", "myeloma", "myeloma",
      "cll", "cll",
      "nhl", "nhl", "nhl"
    ),
    date = as.Date(c(
      "2003-05-13", "2006-06-29", "2015-11-16",
      "2014-02-12", "2016-04-11",
      "1997-11-26", "2019-06-10", "2023-04-19"
    )),
    drug = c(
      "Bortezomib", "Lenalidomide", "Daratumumab",
      "Ibrutinib", "Venetoclax",
      "Rituximab", "Polatuzumab vedotin", "Polatuzumab vedotin"
    ),
    on_figure = c(
      TRUE, TRUE, TRUE,
      TRUE, TRUE,
      FALSE, TRUE, FALSE
    ),
    indication = c(
      "Multiple myeloma after at least two prior therapies, with progression on the last therapy",
      "With dexamethasone for multiple myeloma after at least one prior therapy",
      "Multiple myeloma after at least three prior lines, including a proteasome inhibitor and an immunomodulatory agent, or double-refractory",
      "Chronic lymphocytic leukemia after at least one prior therapy",
      "Chronic lymphocytic leukemia with 17p deletion after at least one prior therapy",
      "Relapsed or refractory low-grade or follicular, CD20-positive, B-cell non-Hodgkin lymphoma",
      "Relapsed or refractory diffuse large B-cell lymphoma after at least two prior therapies, with bendamustine and a rituximab product",
      "Previously untreated diffuse large B-cell lymphoma, with a rituximab product, cyclophosphamide, doxorubicin, and prednisone, for an International Prognostic Index of 2 or greater"
    ),
    source_url = c(
      "https://www.accessdata.fda.gov/scripts/opdlisting/oopd/detailedIndex.cfm?cfgridkey=163002",
      "https://www.accessdata.fda.gov/scripts/opdlisting/oopd/detailedIndex.cfm?cfgridkey=147701",
      "https://www.accessdata.fda.gov/drugsatfda_docs/appletter/2015/761036Orig1s000ltr.pdf",
      "https://www.accessdata.fda.gov/scripts/opdlisting/oopd/detailedIndex.cfm?cfgridkey=368012",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2016/208573Orig1s000Approv.pdf",
      "https://www.accessdata.fda.gov/scripts/opdlisting/oopd/detailedIndex.cfm?cfgridkey=79793",
      "https://www.accessdata.fda.gov/scripts/opdlisting/oopd/detailedIndex.cfm?cfgridkey=547416",
      "https://www.accessdata.fda.gov/scripts/opdlisting/oopd/detailedIndex.cfm?cfgridkey=547416"
    ),
    source_note = c(
      "FDA orphan-drug listing, marketing approval date 05/13/2003. The FDA verified-clinical-benefit table lists the same date.",
      "FDA orphan-drug listing, marketing approval date 06/29/2006. This is the myeloma indication, not the 27 December 2005 myelodysplastic syndrome approval.",
      "FDA approval letter for BLA 761036. The letter states that the effective approval date remains November 16, 2015.",
      "FDA orphan-drug listing, marketing approval date 02/12/2014, for CLL after at least one prior therapy. The first Imbruvica approval, on 13 November 2013, was for mantle cell lymphoma.",
      "FDA approval package for NDA 208573. The cover sheet gives the approval date as April 11, 2016.",
      "FDA orphan-drug listing, marketing approval date 11/26/1997. The date is before 2000, so it is not drawn.",
      "FDA orphan-drug listing, marketing approval date 06/10/2019. The Purple Book original approval date is 10 June 2019. The signed approval letter is dated 10 June 2019. The package cover also prints 6 June 2019. The figure uses 10 June 2019.",
      "Same FDA orphan-drug listing, a later marketing approval date of 04/19/2023. Five-year survival for 2023 diagnoses is not observed in this release, so the date is not drawn."
    ),
    stringsAsFactors = FALSE
  )
}

APPROVALS <- approval_table()
