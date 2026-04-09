# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 5: By Respondent Migrant Status
# Last Updated  : February 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/5.Covariate_Summary_Respondent_Migrant.tex / .md


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

table_respondent_migrant_data <- nlss_conflict_data %>%
  filter(!is.na(present_ind_migrant_label)) %>%
  group_by(present_ind_migrant_label) %>%
  compute_group_stats()

# Shortcut helpers
respondent_migrant_val     <- function(col) table_respondent_migrant_data[[col]][table_respondent_migrant_data$present_ind_migrant_label == "Respondent Migrant"]
respondent_nonmigrant_val  <- function(col) table_respondent_migrant_data[[col]][table_respondent_migrant_data$present_ind_migrant_label == "Respondent Non-Migrant"]


# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

# Plain version — for Markdown
table_respondent_migrant_formatted <- build_covariate_table(list(
  "Respondent_Migrant"     = respondent_migrant_val,
  "Respondent_Non_Migrant" = respondent_nonmigrant_val
))

# LaTeX version — SD on new line, % and & sanitized
table_respondent_migrant_latex <- build_covariate_table(list(
  "Respondent_Migrant"     = respondent_migrant_val,
  "Respondent_Non_Migrant" = respondent_nonmigrant_val
), latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX ---
latex_respondent_migrant <- kable(table_respondent_migrant_latex,
                                  format    = "latex",
                                  booktabs  = TRUE,
                                  caption   = "Covariate Summary by Respondent Migrant Status",
                                  label     = "respondent_migrant_summary",
                                  col.names = c("Variable", "Respondent Migrant", "Respondent Non-Migrant"),
                                  escape    = FALSE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  footnote(general          = "Standard deviations in parentheses for continuous variables.",
           footnote_as_chunk = TRUE)

writeLines(as.character(latex_respondent_migrant),
           file.path(tables_summary, "5.Covariate_Summary_Respondent_Migrant.tex"))


# --- Markdown ---
md_respondent_migrant <- kable(table_respondent_migrant_formatted,
                               format    = "markdown",
                               col.names = c("Variable", "Respondent Migrant", "Respondent Non-Migrant"),
                               caption   = "Covariate Summary by Respondent Migrant Status")

writeLines(
  c(md_respondent_migrant, "",
    "*Notes:*",
    "- Standard deviations in parentheses for continuous variables.",
    "- Respondent Migrant includes individuals who travelled abroad previously for at least 3 months."),
  file.path(tables_summary, "5.Covariate_Summary_Respondent_Migrant.md")
)