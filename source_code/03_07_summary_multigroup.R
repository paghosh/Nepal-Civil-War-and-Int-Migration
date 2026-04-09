# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 7: By All Migration Groups
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/7.Multigroup_Summary.tex / .md


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

stats_baseline <- nlss_conflict_data %>% filter(baseline == 1)                     %>% compute_group_stats()
stats_absent   <- nlss_conflict_data %>% filter(absent == 1)                        %>% compute_group_stats()
stats_intl     <- nlss_conflict_data %>% filter(international_absentee_only == 1)   %>% compute_group_stats()
stats_national <- nlss_conflict_data %>% filter(national == 1)                      %>% compute_group_stats()
stats_returnee <- nlss_conflict_data %>% filter(present_ind_migrant == 1)           %>% compute_group_stats()

# Shortcut helpers
baseline_val  <- function(col) g(stats_baseline,  col)
absent_val    <- function(col) g(stats_absent,     col)
intl_val      <- function(col) g(stats_intl,       col)
national_val  <- function(col) g(stats_national,   col)
returnee_val  <- function(col) g(stats_returnee,   col)


# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

# Plain version — for Markdown
table_multigroup_formatted <- build_covariate_table(list(
  "Baseline"        = baseline_val,
  "Total_Absent"    = absent_val,
  "Intl_Absentee"   = intl_val,
  "National_Absent" = national_val,
  "Returnees"       = returnee_val
))

# LaTeX version — SD on new line, % and & sanitized
table_multigroup_latex <- build_covariate_table(list(
  "Baseline"        = baseline_val,
  "Total_Absent"    = absent_val,
  "Intl_Absentee"   = intl_val,
  "National_Absent" = national_val,
  "Returnees"       = returnee_val
), latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

# --- LaTeX (landscape) ---
latex_multigroup <- kable(table_multigroup_latex,
                          format    = "latex",
                          booktabs  = TRUE,
                          caption   = "Covariate Summary by Migration Group",
                          label     = "multigroup_summary",
                          col.names = c("Variable", "Baseline", "Total Absent",
                                        "Intl. Absentee", "National Absent", "Returnees"),
                          escape    = FALSE,
                          align     = c("l", "c", "c", "c", "c", "c")) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 9
  ) %>%
  footnote(
    general = paste(
      "Standard deviations in parentheses for continuous variables.",
      "Baseline = present at survey and never migrated internationally.",
      "Total Absent = all absent individuals (abroad + internal).",
      "Intl. Absentee = absent and currently abroad.",
      "National Absent = absent and inside Nepal.",
      "Returnees = present at survey but was abroad for work $\\\\geq$3 months."
    ),
    footnote_as_chunk = TRUE,
    escape            = FALSE
  ) %>%
  landscape()     # ← renders table in landscape mode

writeLines(as.character(latex_multigroup),
           file.path(tables_summary, "7.Multigroup_Summary.tex"))


# --- Markdown ---
md_multigroup <- kable(table_multigroup_formatted,
                       format    = "markdown",
                       col.names = c("Variable", "Baseline", "Total Absent",
                                     "Intl. Absentee", "National Absent", "Returnees"),
                       align     = c("l", "c", "c", "c", "c", "c"))

writeLines(
  c(md_multigroup, "",
    "*Notes:*",
    "- Standard deviations in parentheses for continuous variables.",
    "- Baseline: present at survey and never migrated internationally.",
    "- Total Absent: all absent individuals (abroad + internal).",
    "- Intl. Absentee: absent and currently abroad.",
    "- National Absent: absent and inside Nepal.",
    "- Returnees: present at survey but was abroad for work >= 3 months."),
  file.path(tables_summary, "7.Multigroup_Summary.md")
)