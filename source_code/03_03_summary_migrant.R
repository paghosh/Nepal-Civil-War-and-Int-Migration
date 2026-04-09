# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 3: By International Migrant Status
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/3.Covariate_Summary_Migrant.tex / .md


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

table_migrant_data <- nlss_conflict_data %>%
  filter(!is.na(migrant_label)) %>%
  group_by(migrant_label) %>%
  compute_group_stats()

# Shortcut helpers
migrant_val    <- function(col) table_migrant_data[[col]][table_migrant_data$migrant_label == "Migrant"]
nonmigrant_val <- function(col) table_migrant_data[[col]][table_migrant_data$migrant_label == "Non-Migrant"]


# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

# Plain version — for Markdown
table_migrant_formatted <- build_covariate_table(list(
  "Migrant"     = migrant_val,
  "Non_Migrant" = nonmigrant_val
))

# LaTeX version — SD on new line, % and & sanitized
table_migrant_latex <- build_covariate_table(list(
  "Migrant"     = migrant_val,
  "Non_Migrant" = nonmigrant_val
), latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX ---
latex_migrant <- kable(table_migrant_latex,
                       format    = "latex",
                       booktabs  = TRUE,
                       caption   = "Covariate Summary by International Migrant Status",
                       label     = "migrant_summary",
                       col.names = c("Variable", "Migrant", "Non-Migrant"),
                       escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general          = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_migrant),
           file.path(tables_summary, "3.Covariate_Summary_Migrant.tex"))


# --- Markdown ---
md_migrant <- kable(table_migrant_formatted,
                    format    = "markdown",
                    col.names = c("Variable", "Migrant", "Non-Migrant"),
                    caption   = "Covariate Summary by International Migrant Status")

writeLines(
  c(md_migrant, "",
    "*Notes:*",
    "- Standard deviations in parentheses for continuous variables.",
    "- Migrant includes individuals abroad at time of survey and those who had been abroad for at least 3 months in the past."),
  file.path(tables_summary, "3.Covariate_Summary_Migrant.md")
)