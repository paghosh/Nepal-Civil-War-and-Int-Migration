# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 6: By Internal Migrant Status
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/6.Covariate_Summary_Internal_Migrant.tex / .md


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

table_internal_migrant_data <- nlss_conflict_data %>%
  filter(!is.na(national_migrant_label)) %>%
  group_by(national_migrant_label) %>%
  compute_group_stats()

# Shortcut helpers
internal_migrant_val     <- function(col) table_internal_migrant_data[[col]][table_internal_migrant_data$national_migrant_label == "Internal Migrant"]
internal_nonmigrant_val  <- function(col) table_internal_migrant_data[[col]][table_internal_migrant_data$national_migrant_label == "Non-Internal Migrant"]


# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

# Plain version — for Markdown
table_internal_migrant_formatted <- build_covariate_table(list(
  "Internal_Migrant"     = internal_migrant_val,
  "Non_Internal_Migrant" = internal_nonmigrant_val
))

# LaTeX version — SD on new line, % and & sanitized
table_internal_migrant_latex <- build_covariate_table(list(
  "Internal_Migrant"     = internal_migrant_val,
  "Non_Internal_Migrant" = internal_nonmigrant_val
), latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX ---
latex_internal_migrant <- kable(table_internal_migrant_latex,
                                format    = "latex",
                                booktabs  = TRUE,
                                caption   = "Covariate Summary by Internal Migrant Status",
                                label     = "internal_migrant_summary",
                                col.names = c("Variable", "Internal Migrant", "Non-Internal Migrant"),
                                escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general          = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_internal_migrant),
           file.path(tables_summary, "6.Covariate_Summary_Internal_Migrant.tex"))


# --- Markdown ---
md_internal_migrant <- kable(table_internal_migrant_formatted,
                             format    = "markdown",
                             col.names = c("Variable", "Internal Migrant", "Non-Internal Migrant"),
                             caption   = "Covariate Summary by Internal Migrant Status")

writeLines(
  c(md_internal_migrant, "",
    "*Notes:*",
    "- Standard deviations in parentheses for continuous variables.",
    "- Internal Migrant includes individuals who migrated within the country at the time of survey."),
  file.path(tables_summary, "6.Covariate_Summary_Internal_Migrant.md")
)