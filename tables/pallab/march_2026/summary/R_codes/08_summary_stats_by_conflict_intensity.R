################################################################################
# 08_summary_stats_by_conflict_intensity.R
#
# Summary Statistics Tables by Conflict Intensity (Casualty-Median Treatment/Control)
#
# Creates tables with 3 panels:
#   Panel A: Outcomes (International Migration, Domestic Migration rates)
#   Panel B: Education (No Education, Primary, Secondary, Tertiary)
#   Panel C: Occupation (Agriculture, High Skilled, Service & Clerical,
#            Craft & Manufacturing, Elementary/Low Skilled, Armed Forces)
#
# Treatment = districts with casualty measure ABOVE median
# Control   = districts with casualty measure AT OR BELOW median
#
# 3 column panels: All | International Migration | Domestic Migration
# 7 age-group tabs: 0-17, 18-40, 0-5, 6-10, 11-18, 19-25, 26-40
# 4 casualty variables: cas_own_any, cas_own_fatal, mwar_own_any, mwar_own_fatal
#
# Output: .xlsx files saved to tables/pallab/march_2026/summary/results/
################################################################################

library(haven)
library(dplyr)
library(tidyr)
library(openxlsx)
library(kableExtra)

# ── Paths ────────────────────────────────────────────────────────────────────
base_dir <- "/Users/pallab.ghosh/Documents/GitHub/Nepal-Civil-War-and-Int-Migration"
data_path <- file.path(base_dir, "data/Modified_Data/1_conflict_present_absentee_data.dta")
out_dir   <- file.path(base_dir, "tables/pallab/march_2026/summary/results")

# ── Load data ────────────────────────────────────────────────────────────────
df <- read_dta(data_path)

SURVEY_YEAR    <- 2017
CONFLICT_START <- 1996

# ── Derived variables ────────────────────────────────────────────────────────
df <- df %>%
  mutate(
    male = as.numeric(sex == 1),
    age_num = as.numeric(age),
    birth_year = SURVEY_YEAR - age_num,
    age_at_conflict_start = CONFLICT_START - birth_year
  )

# Ethnicity
hill_high <- c(1,2,14,20,27,48,49)
hill_jan  <- c(3,5,6,10,11,13,24,29,32,36,45,46,60,61,62,66,67,69,74,77,78,79,80,81,
               89,90,91,92,94,97,98,100,110,119,120,121,124,125,126,127,130,131,132,
               133,134,135,136,137,138,139,140,992)
terai_set <- c(4,9,16,18,19,21,26,28,30,31,33,34,35,37,42,43,44,47,51,52,53,54,55,56,
               57,58,59,63,64,65,68,71,72,73,84,85,86,88,96,99,115,116,117,118,122,123,
               128,129,993)
dalit_set <- c(8,12,15,17,22,23,25,38,39,40,41,50,70,75,76,83,93,991)

df <- df %>%
  group_by(psu, hhld) %>%
  mutate(caste_num = as.numeric(caste)) %>%
  fill(caste_num, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    ethnicity = case_when(
      caste_num %in% hill_high ~ "Hill High Caste",
      caste_num %in% hill_jan  ~ "Hill Janajati",
      caste_num %in% terai_set ~ "Terai/Madhesi",
      caste_num %in% dalit_set ~ "Dalit",
      caste_num == 7           ~ "Muslim",
      TRUE                     ~ NA_character_
    )
  )

# Education
df <- df %>%
  mutate(
    grade_num = as.numeric(grade_comp),
    edu_cat = case_when(
      grade_num %in% c(16, 17) ~ "No Education",
      grade_num >= 0 & grade_num <= 5  ~ "Primary (1-5)",
      grade_num >= 6 & grade_num <= 12 ~ "Secondary (6-12)",
      grade_num >= 13                  ~ "Tertiary",
      TRUE                             ~ NA_character_
    )
  )

# Occupation
df <- df %>%
  mutate(
    occ_num = as.numeric(occupation_types),
    occ_cat = case_when(
      occ_num %in% c(110, 210, 310) ~ "Armed Forces",
      floor(ifelse(occ_num < 1000, occ_num / 100, occ_num / 1000)) %in% c(1,2,3) ~ "High Skilled",
      floor(ifelse(occ_num < 1000, occ_num / 100, occ_num / 1000)) %in% c(4,5)   ~ "Service & Clerical",
      floor(ifelse(occ_num < 1000, occ_num / 100, occ_num / 1000)) == 6           ~ "Agriculture",
      floor(ifelse(occ_num < 1000, occ_num / 100, occ_num / 1000)) %in% c(7,8)   ~ "Craft & Manufacturing",
      floor(ifelse(occ_num < 1000, occ_num / 100, occ_num / 1000)) == 9           ~ "Elementary/Low Skilled",
      TRUE ~ NA_character_
    )
  )

# Binary dummies
edu_cats <- c("No Education", "Primary (1-5)", "Secondary (6-12)", "Tertiary")
for (ed in edu_cats) {
  col_name <- paste0("edu_", ed)
  df[[col_name]] <- ifelse(is.na(df$edu_cat), NA, as.numeric(df$edu_cat == ed))
}

occ_cats <- c("Agriculture", "High Skilled", "Service & Clerical",
              "Craft & Manufacturing", "Elementary/Low Skilled", "Armed Forces")
for (oc in occ_cats) {
  col_name <- paste0("occ_", oc)
  df[[col_name]] <- ifelse(is.na(df$occ_cat), NA, as.numeric(df$occ_cat == oc))
}

# Outcome dummies (Panel A)
df$outcome_intl_mig <- df$international_absentee_only
df$outcome_dom_mig  <- df$national

# Casualty variables as numeric
cas_vars <- c("cas_own_any", "cas_own_fatal", "mwar_own_any", "mwar_own_fatal")
for (cv in cas_vars) {
  df[[cv]] <- as.numeric(df[[cv]])
}

# Fill missing casualty data for absentees using district-level mapping
dist_cas <- df %>%
  filter(!is.na(cas_own_any)) %>%
  group_by(dist) %>%
  summarise(across(all_of(cas_vars), ~first(.x)), .groups = "drop")

for (cv in cas_vars) {
  missing_idx <- is.na(df[[cv]])
  if (any(missing_idx)) {
    lookup <- setNames(dist_cas[[cv]], dist_cas$dist)
    df[[cv]][missing_idx] <- lookup[as.character(df$dist[missing_idx])]
  }
}

# ── Row variable definitions ─────────────────────────────────────────────────
row_defs <- list(
  list(var = "Panel A: Outcomes", label = NULL),
  list(var = "outcome_intl_mig",  label = "International Migration"),
  list(var = "outcome_dom_mig",   label = "Domestic Migration"),
  list(var = "Panel B: Education", label = NULL),
  list(var = "edu_No Education",   label = "No Education"),
  list(var = "edu_Primary (1-5)",  label = "Primary (1-5)"),
  list(var = "edu_Secondary (6-12)", label = "Secondary (6-12)"),
  list(var = "edu_Tertiary",       label = "Tertiary"),
  list(var = "Panel C: Occupation", label = NULL),
  list(var = "occ_Agriculture",           label = "Agriculture"),
  list(var = "occ_High Skilled",          label = "High Skilled"),
  list(var = "occ_Service & Clerical",    label = "Service & Clerical"),
  list(var = "occ_Craft & Manufacturing", label = "Craft & Manufacturing"),
  list(var = "occ_Elementary/Low Skilled", label = "Elementary/Low Skilled"),
  list(var = "occ_Armed Forces",          label = "Armed Forces")
)

# ── Age groups ───────────────────────────────────────────────────────────────
age_groups <- list(
  list(label = "0-17",  lo = 0,  hi = 17),
  list(label = "18-40", lo = 18, hi = 40),
  list(label = "0-5",   lo = 0,  hi = 5),
  list(label = "6-10",  lo = 6,  hi = 10),
  list(label = "11-18", lo = 11, hi = 18),
  list(label = "19-25", lo = 19, hi = 25),
  list(label = "26-40", lo = 26, hi = 40)
)

# ── Helper functions ─────────────────────────────────────────────────────────
compute_mean_pct <- function(data, var) {
  vals <- data[[var]]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals) * 100
}

compute_diff_pval <- function(treat_data, ctrl_data, var) {
  t_vals <- treat_data[[var]][!is.na(treat_data[[var]])]
  c_vals <- ctrl_data[[var]][!is.na(ctrl_data[[var]])]
  if (length(t_vals) < 2 || length(c_vals) < 2) return("")
  diff_val <- mean(t_vals) * 100 - mean(c_vals) * 100
  tt <- t.test(t_vals, c_vals, var.equal = FALSE)
  p <- tt$p.value
  stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
  sprintf("%.2f (p=%.3f)%s", diff_val, p, stars)
}

fmt_pct <- function(val) {
  if (is.na(val)) return("")
  sprintf("%.2f", val)
}

# ── Casualty variable metadata ───────────────────────────────────────────────
cas_meta <- list(
  list(var = "cas_own_any",    label = "Number of Casualties (Any)",   short = "cas_own_any"),
  list(var = "cas_own_fatal",  label = "Number of Fatal Casualties",   short = "cas_own_fatal"),
  list(var = "mwar_own_any",   label = "Months of War (Any)",          short = "mwar_own_any"),
  list(var = "mwar_own_fatal", label = "Months of War (Fatal)",        short = "mwar_own_fatal")
)

# ── Build one sheet ──────────────────────────────────────────────────────────
build_sheet_data <- function(df, cas_var, cas_label, age_lo, age_hi, median_val) {
  # Filter age cohort
  age_mask <- df$age_at_conflict_start >= age_lo & df$age_at_conflict_start <= age_hi
  sub <- df[age_mask & !is.na(age_mask), ]

  # Treatment / Control
  treat_all <- sub[sub[[cas_var]] > median_val & !is.na(sub[[cas_var]]), ]
  ctrl_all  <- sub[sub[[cas_var]] <= median_val & !is.na(sub[[cas_var]]), ]

  treat_intl <- treat_all[treat_all$international_absentee_only == 1 & !is.na(treat_all$international_absentee_only), ]
  ctrl_intl  <- ctrl_all[ctrl_all$international_absentee_only == 1 & !is.na(ctrl_all$international_absentee_only), ]

  treat_dom <- treat_all[treat_all$national == 1 & !is.na(treat_all$national), ]
  ctrl_dom  <- ctrl_all[ctrl_all$national == 1 & !is.na(ctrl_all$national), ]

  results <- list()
  for (rd in row_defs) {
    if (is.null(rd$label)) {
      results <- append(results, list(list(section = rd$var)))
      next
    }

    row_data <- list(
      label = rd$label,
      all_treat = fmt_pct(compute_mean_pct(treat_all, rd$var)),
      all_ctrl  = fmt_pct(compute_mean_pct(ctrl_all, rd$var)),
      all_diff  = compute_diff_pval(treat_all, ctrl_all, rd$var),
      intl_treat = fmt_pct(compute_mean_pct(treat_intl, rd$var)),
      intl_ctrl  = fmt_pct(compute_mean_pct(ctrl_intl, rd$var)),
      intl_diff  = compute_diff_pval(treat_intl, ctrl_intl, rd$var),
      dom_treat = fmt_pct(compute_mean_pct(treat_dom, rd$var)),
      dom_ctrl  = fmt_pct(compute_mean_pct(ctrl_dom, rd$var)),
      dom_diff  = compute_diff_pval(treat_dom, ctrl_dom, rd$var)
    )
    results <- append(results, list(row_data))
  }

  return(results)
}

# ── Generate XLSX ────────────────────────────────────────────────────────────
generate_xlsx <- function(cas_var, cas_label, short_name) {
  median_val <- median(df[[cas_var]], na.rm = TRUE)
  wb <- createWorkbook()

  for (ag in age_groups) {
    sheet_name <- paste0("Age ", ag$label)
    addWorksheet(wb, sheet_name)

    sheet_data <- build_sheet_data(df, cas_var, cas_label, ag$lo, ag$hi, median_val)

    # Write header rows
    writeData(wb, sheet_name, paste0("Treatment: Area where ", cas_label, " above the median (>", median_val, ")"), startRow = 1, startCol = 1)
    writeData(wb, sheet_name, paste0("Control: Area where ", cas_label, " below the median (<=", median_val, ")"), startRow = 2, startCol = 1)
    writeData(wb, sheet_name, paste0("Aged ", ag$label, " at conflict start (1996)"), startRow = 4, startCol = 3)

    # Panel headers
    writeData(wb, sheet_name, "All", startRow = 5, startCol = 3)
    writeData(wb, sheet_name, "International Migration", startRow = 5, startCol = 7)
    writeData(wb, sheet_name, "Domestic Migration", startRow = 5, startCol = 11)

    # Sub-headers
    for (col_start in c(3, 7, 11)) {
      writeData(wb, sheet_name, "Treatment", startRow = 6, startCol = col_start)
      writeData(wb, sheet_name, "Control", startRow = 6, startCol = col_start + 1)
      writeData(wb, sheet_name, "Diff (T-C)\n(p-value)", startRow = 6, startCol = col_start + 2)
    }

    # Data rows
    r <- 7
    for (item in sheet_data) {
      if (!is.null(item$section)) {
        writeData(wb, sheet_name, item$section, startRow = r, startCol = 1)
      } else {
        writeData(wb, sheet_name, item$label, startRow = r, startCol = 1)
        writeData(wb, sheet_name, item$all_treat,  startRow = r, startCol = 3)
        writeData(wb, sheet_name, item$all_ctrl,   startRow = r, startCol = 4)
        writeData(wb, sheet_name, item$all_diff,   startRow = r, startCol = 5)
        writeData(wb, sheet_name, item$intl_treat, startRow = r, startCol = 7)
        writeData(wb, sheet_name, item$intl_ctrl,  startRow = r, startCol = 8)
        writeData(wb, sheet_name, item$intl_diff,  startRow = r, startCol = 9)
        writeData(wb, sheet_name, item$dom_treat,  startRow = r, startCol = 11)
        writeData(wb, sheet_name, item$dom_ctrl,   startRow = r, startCol = 12)
        writeData(wb, sheet_name, item$dom_diff,   startRow = r, startCol = 13)
      }
      r <- r + 1
    }

    # Notes
    r <- r + 1
    writeData(wb, sheet_name, paste0("Notes: Treatment = districts with ", cas_label, " above the median (>", median_val, "). Control = districts at or below the median."), startRow = r, startCol = 1)
    writeData(wb, sheet_name, "Diff (T-C): Treatment minus Control with Welch's t-test p-value. *** p<0.01, ** p<0.05, * p<0.10", startRow = r + 1, startCol = 1)
    writeData(wb, sheet_name, paste0("Age ", ag$label, " cohort: individuals aged ", ag$label, " at conflict start in 1996."), startRow = r + 2, startCol = 1)
    writeData(wb, sheet_name, "All = full sample in age cohort. International = international absentees. Domestic = national absentees.", startRow = r + 3, startCol = 1)
  }

  out_path <- file.path(out_dir, paste0("summary_stats_", short_name, ".xlsx"))
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  XLSX:", out_path, "\n")
  return(median_val)
}

# ── Generate LaTeX ───────────────────────────────────────────────────────────
generate_tex <- function(cas_var, cas_label, short_name, median_val) {
  tex_parts <- c()

  for (ag in age_groups) {
    sheet_data <- build_sheet_data(df, cas_var, cas_label, ag$lo, ag$hi, median_val)

    lines <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      "\\small",
      paste0("\\caption{Summary Statistics by Conflict Intensity (", cas_label, "): Aged ", ag$label, " at Conflict Start}"),
      paste0("\\label{tab:", short_name, "_age", gsub("-", "_", ag$label), "}"),
      "\\begin{tabular}{l rrr c rrr c rrr}",
      "\\toprule",
      " & \\multicolumn{3}{c}{All} & & \\multicolumn{3}{c}{International Migration} & & \\multicolumn{3}{c}{Domestic Migration} \\\\",
      "\\cmidrule(lr){2-4} \\cmidrule(lr){6-8} \\cmidrule(lr){10-12}",
      " & Treat & Control & Diff & & Treat & Control & Diff & & Treat & Control & Diff \\\\",
      "\\midrule"
    )

    for (item in sheet_data) {
      if (!is.null(item$section)) {
        lines <- c(lines, paste0("\\textit{", item$section, "} & & & & & & & & & & & \\\\"))
      } else {
        row_line <- paste0(
          "\\quad ", item$label, " & ",
          item$all_treat, " & ", item$all_ctrl, " & ", item$all_diff, " & & ",
          item$intl_treat, " & ", item$intl_ctrl, " & ", item$intl_diff, " & & ",
          item$dom_treat, " & ", item$dom_ctrl, " & ", item$dom_diff, " \\\\"
        )
        lines <- c(lines, row_line)
      }
    }

    lines <- c(lines,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{tablenotes}",
      "\\small",
      paste0("\\item Treatment = districts with ", cas_label, " above median (>", median_val, "). Control = at or below median."),
      "\\item Diff: Treatment $-$ Control with Welch's t-test p-value. $^{***}$ p$<$0.01, $^{**}$ p$<$0.05, $^{*}$ p$<$0.10",
      "\\end{tablenotes}",
      "\\end{table}"
    )

    tex_parts <- c(tex_parts, paste(lines, collapse = "\n"))
  }

  full_tex <- paste(tex_parts, collapse = "\n\n\\clearpage\n\n")
  out_path <- file.path(out_dir, paste0("summary_stats_", short_name, ".tex"))
  writeLines(full_tex, out_path)
  cat("  TEX:", out_path, "\n")
}

# ── Main execution ───────────────────────────────────────────────────────────
for (cm in cas_meta) {
  cat("\n", strrep("=", 60), "\n")
  cat("  ", cm$label, "(median =", median(df[[cm$var]], na.rm = TRUE), ")\n")
  cat(strrep("=", 60), "\n")

  median_val <- generate_xlsx(cm$var, cm$label, cm$short)
  generate_tex(cm$var, cm$label, cm$short, median_val)
}

cat("\nAll files saved to:", out_dir, "\n")
cat("Done!\n")
