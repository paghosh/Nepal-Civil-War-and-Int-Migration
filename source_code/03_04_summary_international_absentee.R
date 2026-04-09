# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 4: By International Absentee Status
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/4.Covariate_Summary_International_Absentee.tex / .md


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

table_intl_absentee_data <- nlss_conflict_data %>%
  filter(!is.na(international_absentee_only_label)) %>%
  group_by(international_absentee_only_label) %>%
  compute_group_stats()

# Shortcut helpers
intl_absentee_val     <- function(col) table_intl_absentee_data[[col]][table_intl_absentee_data$international_absentee_only_label == "International Absentee"]
non_intl_absentee_val <- function(col) table_intl_absentee_data[[col]][table_intl_absentee_data$international_absentee_only_label == "Non-International Absentee"]


# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

# Plain version — for Markdown
table_intl_absentee_formatted <- build_covariate_table(list(
  "International_Absentee"     = intl_absentee_val,
  "Non_International_Absentee" = non_intl_absentee_val
))

# LaTeX version — SD on new line, % and & sanitized
table_intl_absentee_latex <- build_covariate_table(list(
  "International_Absentee"     = intl_absentee_val,
  "Non_International_Absentee" = non_intl_absentee_val
), latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX ---
latex_intl_absentee <- kable(table_intl_absentee_latex,
                             format    = "latex",
                             booktabs  = TRUE,
                             caption   = "Covariate Summary by International Absentee Status",
                             label     = "intl_absentee_summary",
                             col.names = c("Variable", "International Absentee", "Non-International Absentee"),
                             escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general          = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_intl_absentee),
           file.path(tables_summary, "4.Covariate_Summary_International_Absentee.tex"))


# --- Markdown ---
md_intl_absentee <- kable(table_intl_absentee_formatted,
                          format    = "markdown",
                          col.names = c("Variable", "International Absentee", "Non-International Absentee"),
                          caption   = "Covariate Summary by International Absentee Status")

writeLines(
  c(md_intl_absentee, "",
    "*Notes:*",
    "- Standard deviations in parentheses for continuous variables.",
    "- International Absentee includes individuals abroad at the time of survey."),
  file.path(tables_summary, "4.Covariate_Summary_International_Absentee.md")
)