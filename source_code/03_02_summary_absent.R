# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 2: By Absent/Non-Absent Status
# Last Updated  : February 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/2.Covariate_Summary_Absent.tex / .md


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

table_absent_data <- nlss_conflict_data %>%
  group_by(absent_label) %>%
  compute_group_stats()

# Shortcut helpers
absent_val    <- function(col) table_absent_data[[col]][table_absent_data$absent_label == "Absent"]
nonabsent_val <- function(col) table_absent_data[[col]][table_absent_data$absent_label == "Non-Absent"]

# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

table_absent_formatted <- build_covariate_table(list(
  "Absent"     = absent_val,
  "Non_Absent" = nonabsent_val
))

# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX ---
# Apply sanitize_latex() to Variable column before export
table_absent_latex <- build_covariate_table(list(
  "Absent"     = absent_val,
  "Non_Absent" = nonabsent_val
), latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))

latex_absent <- kable(table_absent_latex,
                      format    = "latex",
                      booktabs  = TRUE,
                      caption   = "Covariate Summary by Absent Status",
                      label     = "absent_summary",
                      col.names = c("Variable", "Absent", "Non-Absent"),
                      escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general          = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_absent),
           file.path(tables_summary, "2.Covariate_Summary_Absent.tex"))


# --- Markdown ---
# Use original unsanitized table_absent_formatted (plain text, no LaTeX escapes)
md_absent <- kable(table_absent_formatted,
                   format    = "markdown",
                   col.names = c("Variable", "Absent", "Non-Absent"),
                   caption   = "Covariate Summary by Absent Status")

writeLines(
  c(md_absent, "",
    "*Notes:*",
    "- Standard deviations in parentheses for continuous variables.",
    "- Absent from Household includes all absent individuals at the time of survey."),
  file.path(tables_summary, "2.Covariate_Summary_Absent.md")
)