# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Balance Table - Summary Statistics by Treatment and Control
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/11.Balance_Table.tex / .md


# ==============================================================================
# SECTION 1: DEFINE SUBSETS -----------------------------------------------
# ==============================================================================

all_df   <- nlss_conflict_data %>% filter(treatment %in% c(1, 0))
treat_df <- nlss_conflict_data %>% filter(treatment == 1)
ctrl_df  <- nlss_conflict_data %>% filter(treatment == 0)

cat("=== Group Size Check ===\n")
cat("All:       ", nrow(all_df),   "\n")
cat("Treatment: ", nrow(treat_df), "\n")
cat("Control:   ", nrow(ctrl_df),  "\n")


# ==============================================================================
# SECTION 2: HELPER FUNCTIONS -----------------------------------------------
# ==============================================================================

# Compute mean for a continuous variable
get_mean <- function(df, var) {
  round(mean(df[[var]], na.rm = TRUE), 2)
}

# Compute SD for a continuous variable
get_sd <- function(df, var) {
  paste0("(", round(sd(df[[var]], na.rm = TRUE), 2), ")")
}

# Compute mean % for a binary variable
get_mean_pct <- function(df, var) {
  round(mean(df[[var]], na.rm = TRUE) * 100, 2)
}

# Compute SD % for a binary variable
get_sd_pct <- function(df, var) {
  paste0("(", round(sd(df[[var]], na.rm = TRUE) * 100, 2), ")")
}

# Compute Diff (T-C) with significance stars
get_diff <- function(var, is_continuous = FALSE) {
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 | length(c_vals) < 2) return("")
  
  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  
  diff <- if (is_continuous) {
    round(mean(t_vals) - mean(c_vals), 2)
  } else {
    round((mean(t_vals) - mean(c_vals)) * 100, 2)
  }
  
  stars <- case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "*",
    TRUE                ~ ""
  )
  
  paste0(diff, stars)
}

# Compute SE of the difference
get_se <- function(var, is_continuous = FALSE) {
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 | length(c_vals) < 2) return("")
  
  se <- sqrt(var(t_vals) / length(t_vals) + var(c_vals) / length(c_vals))
  
  if (!is_continuous) se <- se * 100
  
  paste0("(", round(se, 2), ")")
}


# ==============================================================================
# SECTION 3: BUILD TABLE ROWS -----------------------------------------------
# ==============================================================================

build_balance_table <- function(latex = FALSE) {
  
  # Row builders
  row_cont <- function(label, var) {
    data.frame(
      Variable      = label,
      All_Mean      = as.character(get_mean(all_df,   var)),
      All_SD        = get_sd(all_df,   var),
      Treat_Mean    = as.character(get_mean(treat_df, var)),
      Treat_SD      = get_sd(treat_df, var),
      Control_Mean  = as.character(get_mean(ctrl_df,  var)),
      Control_SD    = get_sd(ctrl_df,  var),
      Diff_TC       = get_diff(var, is_continuous = TRUE),
      SE            = get_se(var,  is_continuous = TRUE),
      stringsAsFactors = FALSE
    )
  }
  
  row_bin <- function(label, var) {
    data.frame(
      Variable      = label,
      All_Mean      = as.character(get_mean_pct(all_df,   var)),
      All_SD        = get_sd_pct(all_df,   var),
      Treat_Mean    = as.character(get_mean_pct(treat_df, var)),
      Treat_SD      = get_sd_pct(treat_df, var),
      Control_Mean  = as.character(get_mean_pct(ctrl_df,  var)),
      Control_SD    = get_sd_pct(ctrl_df,  var),
      Diff_TC       = get_diff(var, is_continuous = FALSE),
      SE            = get_se(var,  is_continuous = FALSE),
      stringsAsFactors = FALSE
    )
  }
  
  row_blank <- function() {
    data.frame(Variable = "", All_Mean = "", All_SD = "",
               Treat_Mean = "", Treat_SD = "", Control_Mean = "",
               Control_SD = "", Diff_TC = "", SE = "",
               stringsAsFactors = FALSE)
  }
  
  row_panel <- function(label) {
    data.frame(Variable = label, All_Mean = "", All_SD = "",
               Treat_Mean = "", Treat_SD = "", Control_Mean = "",
               Control_SD = "", Diff_TC = "", SE = "",
               stringsAsFactors = FALSE)
  }
  
  # Assemble rows — all labels in plain text
  tbl <- bind_rows(
    
    # Panel A: Outcomes
    row_panel("Panel A: Outcomes"),
    row_bin("  International Migration (=1)", "international_migrant"),
    row_bin("  Currently Abroad (=1)",         "international_absentee_only"),
    row_bin("  Return Migrant (=1)",            "present_ind_migrant"),
    row_bin("  Internal Migration (=1)",        "national"),
    row_blank(),
    
    # Panel B: Conflict Exposure
    row_panel("Panel B: Exposure to Conflict"),
    row_cont("  Months of War (own district)",       "mwar_own_any"),
    row_cont("  Casualties (own district)",           "cas_own_any"),
    row_cont("  Months of War (own district, fatal)", "mwar_own_fatal"),
    row_cont("  Casualties (own district, fatal)",    "cas_own_fatal"),
    row_blank(),
    
    # Panel C: Demographics
    row_panel("Panel C: Demographics"),
    row_cont("  Current Age",           "age"),
    row_cont("  Age at Conflict Start", "age_at_conflict_start"),
    row_bin("  Male (=1)",              "male"),
    row_blank(),
    
    # Panel D: Education
    row_panel("Panel D: Education (%)"),
    row_bin("  No Education",     "edu_no_education"),
    row_bin("  Primary (1-5)",    "edu_primary"),
    row_bin("  Secondary (6-12)", "edu_secondary"),
    row_bin("  Tertiary",         "edu_tertiary"),
    row_blank(),
    
    # Panel E: Ethnicity
    row_panel("Panel E: Ethnicity (%)"),
    row_bin("  Hill High Caste", "eth_hill_high"),
    row_bin("  Hill Janajati",   "eth_janajati"),
    row_bin("  Terai/Madhesi",   "eth_terai"),
    row_bin("  Dalit",           "eth_dalit"),
    row_bin("  Muslim",          "eth_muslim"),
    row_blank(),
    
    # Panel F: Occupation
    row_panel("Panel F: Occupation (%)"),
    row_bin("  Agriculture",            "occ_agriculture"),
    row_bin("  High Skilled",           "occ_high_skilled"),
    row_bin("  Service & Clerical",     "occ_service"),
    row_bin("  Craft & Manufacturing",  "occ_craft"),
    row_bin("  Elementary/Low Skilled", "occ_elementary"),
    row_bin("  Armed Forces",           "occ_armed"),
    row_blank(),
    
    # N at bottom
    data.frame(
      Variable     = "N",
      All_Mean     = as.character(nrow(all_df)),
      All_SD       = "",
      Treat_Mean   = as.character(nrow(treat_df)),
      Treat_SD     = "",
      Control_Mean = as.character(nrow(ctrl_df)),
      Control_SD   = "",
      Diff_TC      = "",
      SE           = "",
      stringsAsFactors = FALSE
    )
  )
  
  # Apply sanitize_latex() to Variable column for LaTeX version
  if (latex) {
    tbl <- tbl %>% mutate(Variable = sanitize_latex(Variable))
  }
  
  return(tbl)
}


# ==============================================================================
# SECTION 4: FORMAT TABLES --------------------------------------------------
# ==============================================================================

# Plain version — for Markdown
table_balance_formatted <- build_balance_table(latex = FALSE)

# LaTeX version
table_balance_latex <- build_balance_table(latex = TRUE)


# ==============================================================================
# SECTION 5: EXPORT OUTPUTS -------------------------------------------------
# ==============================================================================

# --- LaTeX (landscape) ---

# Row numbers for panel headers (for row_spec bold+italic)
panel_rows <- which(table_balance_latex$Variable %in% c(
  "Panel A: Outcomes",
  "Panel B: Exposure to Conflict",
  "Panel C: Demographics",
  "Panel D: Education (\\%)",
  "Panel E: Ethnicity (\\%)",
  "Panel F: Occupation (\\%)"
))

latex_balance <- kable(table_balance_latex,
                       format    = "latex",
                       booktabs  = TRUE,
                       caption   = "Summary Statistics of Individuals (Age 0--40 at Conflict Start)",
                       label     = "balance_table",
                       col.names = c("Variable",
                                     "All Mean", "All (SD)",
                                     "Treatment Mean", "(SD)",
                                     "Control Mean",   "(SD)",
                                     "Diff (T-C)", "(SE)"),
                       escape    = FALSE,
                       align     = c("l", "r", "r", "r", "r", "r", "r", "r", "r")) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 9
  ) %>%
  add_header_above(c(" "       = 1,
                     "All"     = 2,
                     "Treatment (Age 0--17)" = 2,
                     "Control (Age 18--40)"  = 2,
                     " "       = 2)) %>%
  row_spec(panel_rows, bold = TRUE, italic = TRUE) %>%
  footnote(
    general = paste(
      "Treatment = aged 0--17 at conflict start (1996).",
      "Control = aged 18--40 at conflict start (1996).",
      "Standard deviations in parentheses.",
      "Standard errors of the difference in parentheses in SE column.",
      "Education, ethnicity, and occupation percentages conditional on non-missing values.",
      "*** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
      "Source: Nepal Labor Force Survey; conflict data from INSEC."
    ),
    footnote_as_chunk = TRUE,
    escape            = FALSE
  ) %>%
  landscape()

writeLines(as.character(latex_balance),
           file.path(tables_summary, "11.Balance_Table.tex"))


# --- Markdown ---
md_balance <- kable(table_balance_formatted,
                    format    = "markdown",
                    col.names = c("Variable",
                                  "All Mean", "All (SD)",
                                  "Treatment Mean", "(SD)",
                                  "Control Mean",   "(SD)",
                                  "Diff (T-C)", "(SE)"),
                    align     = c("l", "r", "r", "r", "r", "r", "r", "r", "r"))

writeLines(
  c(md_balance, "",
    "*Notes:*",
    "- Treatment = aged 0-17 at conflict start (1996).",
    "- Control = aged 18-40 at conflict start (1996).",
    "- Standard deviations in parentheses.",
    "- Standard errors of the difference in SE column.",
    "- *** p<0.01, ** p<0.05, * p<0.10.",
    "- Source: Nepal Labor Force Survey; conflict data from INSEC."),
  file.path(tables_summary, "11.Balance_Table.md")
)