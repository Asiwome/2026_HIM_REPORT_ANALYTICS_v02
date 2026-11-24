# ============================================================
# TTH Ward-Level Indicator Pipeline
# Works for any indicator workbook that follows the structure of
# 2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx:
#   - one sheet per Department
#   - rows = Wards (+ a "Total" row)
#   - columns = Months (+ a "Total" column)
#   - one extra "TTH" sheet with department-level totals only
#
# Use the SAME functions for Admissions, Patient Days, Discharges,
# Available Beds, etc. Only the `indicator_name` argument changes.
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)

# ------------------------------------------------------------
# 1. GENERIC READER
#    Turns one indicator workbook into tidy long format:
#    Department | Ward | Month | Value | Indicator
# ------------------------------------------------------------
read_indicator_workbook <- function(path, indicator_name, summary_sheet = "TTH") {

  sheets <- excel_sheets(path)
  dept_sheets <- setdiff(sheets, summary_sheet)

  map_dfr(dept_sheets, function(sh) {
    read_excel(path, sheet = sh) |>
      rename(Ward = 1) |>                      # first column is "Ward" (or "Department" on summary)
      filter(!Ward %in% c("Total", NA)) |>      # drop the in-sheet Total row
      pivot_longer(
        cols = -Ward,
        names_to  = "Month",
        values_to = "Value"
      ) |>
      filter(!Month %in% c("Total")) |>         # drop the in-sheet Total column
      mutate(
        Department = sh,
        Indicator  = indicator_name,
        Value      = as.numeric(Value),
        Ward       = str_squish(str_to_upper(Ward)),   # normalise so lookups are reliable
        Month      = factor(Month, levels = c("Jan","Feb","Mar","Apr","May","Jun"))
      ) |>
      relocate(Department, Ward, Month, Indicator, Value)
  })
}

# ------------------------------------------------------------
# 2. QA CHECK
#    Confirms your long-format reconstruction matches the
#    workbook's own published Total row/column. Run this on
#    every new indicator file before trusting the numbers.
# ------------------------------------------------------------
qa_check_against_workbook <- function(path, long_df, summary_sheet = "TTH") {

  dept_totals_reported <- read_excel(path, sheet = summary_sheet) |>
    rename(Department = 1) |>
    filter(!Department %in% c("Total")) |>
    pivot_longer(-Department, names_to = "Month", values_to = "Reported") |>
    filter(Month != "Total") |>
    mutate(Month = factor(Month, levels = c("Jan","Feb","Mar","Apr","May","Jun")))

  dept_totals_computed <- long_df |>
    group_by(Department, Month) |>
    summarise(Computed = sum(Value, na.rm = TRUE), .groups = "drop")

  dept_totals_reported |>
    left_join(dept_totals_computed, by = c("Department", "Month")) |>
    mutate(Match = Reported == Computed)
}

# ------------------------------------------------------------
# 3. WARD -> SUBSPECIALTY LOOKUP (ALL DEPARTMENTS)
#    One shared table, tagged by Department, so the same
#    build_department_table() function below works for Surgery,
#    Internal Medicine, O&G, etc.
#
#    Departments where each ward already stands on its own (no
#    male/female/paed variants to merge) use an IDENTITY mapping -
#    Ward maps to itself, cleaned up for display. Only add real
#    groupings where wards genuinely need to be combined, the way
#    Surgery's male/female/paed variants do.
# ------------------------------------------------------------
ward_group_map <- bind_rows(

  # --- Surgery: wards merged into surgical subspecialties ---
  tribble(
    ~Department, ~Ward,                                    ~Subspecialty,
    "Surgery",   "GENERAL SURGERY WARD",                   "General Surgery",
    "Surgery",   "FEMALE GENERAL SURGICAL WARD",           "General Surgery",
    "Surgery",   "MALE GENERAL SURGICAL WARD",             "General Surgery",
    "Surgery",   "GENERAL SURGERY SIDE WARD",               "General Surgery",
    "Surgery",   "MALE PAED SURGERY",                      "Paediatric Surgery",
    "Surgery",   "FEMALE PAED SURGERY",                    "Paediatric Surgery",
    "Surgery",   "INTENSIVE CARE UNIT (ICU) WARD",         "ICU",
    "Surgery",   "ICU VIP",                                "ICU",
    "Surgery",   "PLASTIC SURGERY WARD",                   "Plastic / Thoracic",
    "Surgery",   "PLASTIC / THORACIC WARD",                "Plastic / Thoracic",
    "Surgery",   "THORACIC SURGERY WARD",                  "Plastic / Thoracic",
    "Surgery",   "MALE NEUROSURGERY WARD",                 "Neurosurgery",
    "Surgery",   "FEMALE NEUROSURGERY WARD",               "Neurosurgery",
    "Surgery",   "NEUROSURGERY WARD",                      "Neurosurgery",
    "Surgery",   "MALE UROLOGY WARD",                      "Urology",
    "Surgery",   "FEMALE UROLOGY WARD",                    "Urology",
    "Surgery",   "UROLOGY STAFF WARD",                     "Urology",
    "Surgery",   "PAEDITRICS UROLOGY WARD",                "Urology",
    "Surgery",   "UROLOGY WARD",                           "Urology"
  ),

  # --- Internal Medicine: each ward is already its own unit ---
  tribble(
    ~Department,          ~Ward,                        ~Subspecialty,
    "Internal Medicine",  "INFECTIOUS DISEASE CENTRE",  "Infectious Disease Centre",
    "Internal Medicine",  "MEDICAL 1 WARD",              "Medical 1",
    "Internal Medicine",  "MEDICAL 2 WARD",              "Medical 2",
    "Internal Medicine",  "MEDICAL 3 WARD",               "Medical 3"
  ),

  # --- Obstetrics & Gynaecology: identity mapping (edit if grouping needed) ---
  tribble(
    ~Department,                  ~Ward,                                    ~Subspecialty,
    "Obstetrics & Gynaecology",   "GYNAECOLOGY A WARD",                    "Gynaecology A",
    "Obstetrics & Gynaecology",   "GYNAECOLOGY B WARD",                    "Gynaecology B",
    "Obstetrics & Gynaecology",   "MATERNITY EAST WARD (PRENATAL)",        "Maternity East (Prenatal)",
    "Obstetrics & Gynaecology",   "MATERNITY WEST WARD (POSTNATAL)",       "Maternity West (Postnatal)",
    "Obstetrics & Gynaecology",   "MATERNITY INTENSIVE CARE UNIT (MICU)",  "MICU"
  ),

  # --- Paediatrics & Child Health: identity mapping ---
  tribble(
    ~Department,                     ~Ward,                             ~Subspecialty,
    "Paediatrics & Child Health",    "NICU",                            "NICU",
    "Paediatrics & Child Health",    "PAEDIATRIC BABIES UNIT",          "Babies Unit",
    "Paediatrics & Child Health",    "PAEDIATRIC CRITICAL CARE WARD",   "Critical Care",
    "Paediatrics & Child Health",    "PAEDIATRIC EAST WARD",            "East Ward",
    "Paediatrics & Child Health",    "PAEDIATRIC EMERGENCY WARD",       "Emergency Ward",
    "Paediatrics & Child Health",    "PAEDIATRIC MALNUTRITION WARD",    "Malnutrition Ward",
    "Paediatrics & Child Health",    "PAEDIATRIC ONCOLOGY UNIT",        "Oncology Unit",
    "Paediatrics & Child Health",    "PAEDIATRIC UNDER 6 WARD",         "Under 6 Ward",
    "Paediatrics & Child Health",    "PAEDIATRIC WEST WARD",            "West Ward"
  ),

  # --- Accident & Emergency: identity mapping ---
  tribble(
    ~Department,             ~Ward,                               ~Subspecialty,
    "Accident & Emergency",  "A&E CDU",                           "A&E CDU",
    "Accident & Emergency",  "A&E ORANGE",                        "A&E Orange",
    "Accident & Emergency",  "A&E RED",                           "A&E Red",
    "Accident & Emergency",  "A&E VIP",                           "A&E VIP",
    "Accident & Emergency",  "A&E YELLOW",                        "A&E Yellow",
    "Accident & Emergency",  "EMERGENCY TRIAGE DETENTION WARD",   "Emergency Triage Detention"
  ),

  # --- DEENT: single ward, identity mapping ---
  tribble(
    ~Department, ~Ward,          ~Subspecialty,
    "DEENT",     "DEENT WARD",   "DEENT"
  ),

  # --- Poly Clinic: identity mapping ---
  tribble(
    ~Department,    ~Ward,                          ~Subspecialty,
    "Poly Clinic",  "FAMILY MEDICINE FEMALE WARD",  "Family Medicine",
    "Poly Clinic",  "FAMILY MEDICINE MALE WARD",    "Family Medicine",
    "Poly Clinic",  "POLYCLINIC DETENTION WARD",    "Polyclinic Detention"
  ),

  # --- Trauma & Orthopaedics: identity mapping ---
  tribble(
    ~Department,               ~Ward,                            ~Subspecialty,
    "Trauma & Orthopaedics",  "PAEDIATRIC ORTHOPAEDICS WARD",     "Paediatric Orthopaedics",
    "Trauma & Orthopaedics",  "TRAUMA AND ORTHO",                 "Trauma and Ortho"
  )

  # Add further departments here the same way as bed complement /
  # patient days / discharges data becomes available for them.
)

# Kept as an alias so earlier code in this script referencing
# surgical_ward_map still works unchanged.
surgical_ward_map <- ward_group_map |>
  filter(Department == "Surgery") |>
  select(Ward, Subspecialty)

# ------------------------------------------------------------
# 4. GENERIC AGGREGATOR
#    Works for any single indicator's long table.
# ------------------------------------------------------------
summarise_by_subspecialty <- function(long_df, ward_map = surgical_ward_map) {

  unmatched <- long_df |>
    distinct(Ward) |>
    anti_join(ward_map, by = "Ward")

  if (nrow(unmatched) > 0) {
    warning(
      "Wards with no subspecialty mapping (excluded from summary): ",
      paste(unmatched$Ward, collapse = "; ")
    )
  }

  long_df |>
    inner_join(ward_map, by = "Ward") |>
    group_by(Indicator, Subspecialty, Month) |>
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = Month, values_from = Value) |>
    mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE))
}

# ------------------------------------------------------------
# 4b. BATCH READER
#     Recurses into subfolders (e.g. Raw_Submissions/ which may
#     contain top-level files AND a Bed_Indicators_datasets/
#     subfolder), one workbook per indicator.
#
#     Indicator name is derived from the filename itself:
#       "Available_Beds_V01.xlsx" -> "Available Beds"
#       "Patient_Days.xlsx"       -> "Patient Days"
#       "2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx" -> matched via TTH- pattern -> "Admissions"
#     Override any of these with name_map if the auto-guess is wrong.
# ------------------------------------------------------------
read_indicator_folder <- function(folder, summary_sheet = "TTH",
                                   name_map = NULL, recursive = TRUE) {

  files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE,
                       recursive = recursive)

  if (length(files) == 0) {
    stop("No .xlsx files found in: ", folder)
  }

  guess_indicator <- function(fname) {
    base <- tools::file_path_sans_ext(basename(fname))

    # Try the "...TTH-<NAME>_V0x" convention first.
    # [A-Za-z-]+ (not just [A-Za-z]+) so multi-word names like
    # "PATIENTS-DAYS" or "TRANS-INS" are captured in full, not
    # truncated at the first hyphen.
    tth_match <- str_extract(base, "(?<=TTH-)[A-Za-z-]+(?=_[Vv]\\d+$|$)")
    if (!is.na(tth_match)) {
      return(tth_match |> str_replace_all("-", " ") |> str_to_title())
    }

    # Otherwise: strip a trailing _V01 / _v2 style version tag,
    # then turn underscores/dashes into spaces and title-case it
    base |>
      str_remove("_[Vv]\\d+$") |>
      str_replace_all("[_\\-]+", " ") |>
      str_squish() |>
      str_to_title()
  }

  map_dfr(files, function(f) {
    indicator_name <- if (!is.null(name_map) && basename(f) %in% names(name_map)) {
      name_map[[basename(f)]]
    } else {
      guess_indicator(f)
    }
    read_indicator_workbook(f, indicator_name, summary_sheet)
  })
}

# Usage - reads everything at once, top-level files + Bed_Indicators_datasets/:
# all_indicators_long <- read_indicator_folder(
#   "/Users/asi/Library/CloudStorage/OneDrive-SmartWorkplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions"
# )
#
# Sanity check: confirm the guessed indicator names look right before
# doing any aggregation on top of them.
# all_indicators_long |> distinct(Indicator)
#
# If any filename's auto-guessed name is wrong, override just that one:
# all_indicators_long <- read_indicator_folder(
#   "path/to/Raw_Submissions",
#   name_map = list(
#     "Available_Beds_V01.xlsx" = "Available Beds",
#     "Patient_Days_V01.xlsx"   = "Patient Days"
#   )
# )

# # ------------------------------------------------------------
# # 5. WORKED EXAMPLE - Admissions
# # ------------------------------------------------------------
# admissions_long <- read_indicator_workbook(
#   path           = "2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx",
#   indicator_name = "Admissions"
# )

# qa_admissions <- qa_check_against_workbook(
#   "2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx",
#   admissions_long
# )
# print(qa_admissions)   # every row should show Match = TRUE

# surg_admissions_summary <- admissions_long |>
#   summarise_by_subspecialty()

# print(surg_admissions_summary)

# ------------------------------------------------------------
# 6. FULL PIPELINE - Bed Indicators + Bed Complement
#    Combines everything in Bed_Indicators_datasets/ (Admissions,
#    Discharges, Patients Days, Trans Ins, Trans Out, and — once
#    you add it — Deaths) with a separate Bed Complement workbook,
#    then derives the standard bed-utilization metrics per
#    surgical subspecialty per month.
#
#    name_map pins each filename to a canonical label so the
#    formulas below (`Patients Days`, `Discharges`, `Beds`) always
#    resolve correctly regardless of how a filename is styled.
# ------------------------------------------------------------

bed_indicators_folder <- "/Users/asi/Library/CloudStorage/OneDrive-SmartWorkplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets"

bed_indicators_long <- read_indicator_folder(
  bed_indicators_folder,
  name_map = list(
    "2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx"    = "Admissions",
    "2026-JAN-JUN_TTH-DISCHARGES_V01.xlsx"    = "Discharges",
    "2026-JAN-JUN_TTH-PATIENTS-DAYS_V01.xlsx" = "Patients Days",
    "2026-JAN-JUN_TTH-TRANS-INS_V01.xlsx"     = "Trans Ins",
    "2026-JAN-JUN_TTH-TRANS-OUT_V01.xlsx"     = "Trans Out"
    # once added:
    # "2026-JAN-JUN_TTH-DEATHS_V01.xlsx"      = "Deaths"
  )
)

# Bed complement lives in a separate workbook - same sheet structure,
# read the same way, just a different indicator label.
bed_complement_long <- read_indicator_workbook(
  file.path(bed_indicators_folder, "2026-JAN-JUN_TTH-BED-COMPLIMENT_V01.xlsx"),
  indicator_name = "Beds"
)

all_indicators_long <- bind_rows(bed_indicators_long, bed_complement_long)

days_in_month <- tibble(
  Month = factor(c("Jan","Feb","Mar","Apr","May","Jun"),
                  levels = c("Jan","Feb","Mar","Apr","May","Jun")),
  Days  = c(31, 28, 31, 30, 31, 30)   # set Feb = 29 in a leap year
)

# summarise_by_subspecialty() only handles ONE indicator at a time,
# so map it across each indicator present, then pivot wide.
#
# NOTE ON FORMULAS: verified against the hospital's own "Table 12
# Ward Indicators" report. Several standard bed-statistics formulas
# use (Discharges + Deaths) as the denominator, not Discharges alone
# - i.e. a death counts as a "discharge outcome" for ALOS/turnover
# purposes. This matched Table 12 exactly across all 7 subspecialty
# columns when checked by hand.
bed_metrics <- all_indicators_long |>
  inner_join(surgical_ward_map, by = "Ward") |>
  group_by(Indicator, Subspecialty, Month) |>
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Indicator, values_from = Value) |>
  left_join(days_in_month, by = "Month") |>
  { \(df) if (!"Deaths" %in% names(df)) mutate(df, Deaths = 0) else df }() |>
  mutate(
    Bed_Days_Available   = Beds * Days,
    Total_Discharged      = Discharges + Deaths,   # standard convention: deaths count as a discharge outcome
    Occupancy_Rate_Pct    = round(`Patients Days` / Bed_Days_Available * 100, 1),
    Avg_Daily_Occupancy   = round(`Patients Days` / Days, 1),
    ALOS_Days             = round(`Patients Days` / Total_Discharged, 1),
    Turnover_Per_Bed      = round(Total_Discharged / Beds, 1),
    Turnover_Interval     = round((Bed_Days_Available - `Patients Days`) / Total_Discharged, 1),
    Death_Rate_Pct        = round(Deaths / Total_Discharged * 100, 1),
    # Net bed-flow check: Admissions + Trans Ins should reconcile
    # against Discharges + Trans Out + Deaths. Useful as a
    # data-quality sanity check, not a reporting metric.
    Net_Flow_Check        = (Admissions + `Trans Ins`) - (Discharges + `Trans Out` + Deaths)
  )

print(bed_metrics)

# ------------------------------------------------------------
# 7. "TABLE 12" STYLE OUTPUT
#    Reshapes bed_metrics for one month into the same layout as
#    the hospital's existing monthly report: indicators as rows,
#    subspecialty as columns, plus a weighted Surgery-SubBMC total.
#
#    IMPORTANT: the SubBMC total column is NOT an average of the
#    subspecialty percentages/rates - each rate is recomputed from
#    the SUMMED raw counts, exactly like Table 12 does (e.g. its
#    Surgery-SubBMC % Bed Occupancy of 75.3 = 2959/3930*100, not
#    the average of the 7 individual occupancy rates).
# ------------------------------------------------------------

compute_indicators <- function(Beds, Admissions, Discharges, Deaths,
                                Trans_In, Trans_Out, Patients_Days, Days) {
  Bed_Days_Available <- Beds * Days
  Total_Discharged    <- Discharges + Deaths
  tibble(
    `Bed Compliment`              = Beds,
    `Admissions`                  = Admissions,
    `Discharges`                  = Discharges,
    `Deaths`                      = Deaths,
    `Transfer In`                 = Trans_In,
    `Transfer Out`                = Trans_Out,
    `Patient Days`                = Patients_Days,
    `% Death Rate`                = round(Deaths / Total_Discharged * 100, 1),
    `Average Daily Bed Occupancy` = round(Patients_Days / Days, 1),
    `% Bed Occupancy`             = round(Patients_Days / Bed_Days_Available * 100, 1),
    `Average Length of Stay`      = round(Patients_Days / Total_Discharged, 1),
    `Turn over per bed`           = round(Total_Discharged / Beds, 1),
    `Turn over per interval`      = round((Bed_Days_Available - Patients_Days) / Total_Discharged, 1),
    `Available Bed Days`          = Bed_Days_Available
  )
}

build_department_table <- function(all_indicators_long, department, target_month,
                                    ward_map = ward_group_map, total_label = NULL) {

  dept_ward_map <- ward_map |>
    filter(Department == department) |>
    select(Ward, Subspecialty)

  if (nrow(dept_ward_map) == 0) {
    stop("No ward grouping found for department '", department,
         "' - add it to ward_group_map first.")
  }

  if (is.null(total_label)) total_label <- paste0(department, "-SubBMC")

  monthly <- all_indicators_long |>
    inner_join(dept_ward_map, by = "Ward") |>
    group_by(Indicator, Subspecialty, Month) |>
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = Indicator, values_from = Value) |>
    left_join(days_in_month, by = "Month") |>
    { \(df) if (!"Deaths" %in% names(df)) mutate(df, Deaths = 0) else df }() |>
    filter(Month == target_month)

  if (nrow(monthly) == 0) return(tibble())

  by_subspecialty <- monthly |>
    rowwise() |>
    mutate(
      row = list(compute_indicators(Beds, Admissions, Discharges, Deaths,
                                     `Trans Ins`, `Trans Out`, `Patients Days`, Days))
    ) |>
    ungroup() |>
    select(Subspecialty, row) |>
    unnest(row) |>
    pivot_longer(-Subspecialty, names_to = "Indicator", values_to = "Value") |>
    pivot_wider(names_from = Subspecialty, values_from = Value)

  totals_row_inputs <- monthly |>
    summarise(
      Beds = sum(Beds), Admissions = sum(Admissions), Discharges = sum(Discharges),
      Deaths = sum(Deaths), `Trans Ins` = sum(`Trans Ins`), `Trans Out` = sum(`Trans Out`),
      `Patients Days` = sum(`Patients Days`), Days = first(Days)
    )

  dept_total <- with(totals_row_inputs,
    compute_indicators(Beds, Admissions, Discharges, Deaths,
                        `Trans Ins`, `Trans Out`, `Patients Days`, Days)
  ) |>
    pivot_longer(everything(), names_to = "Indicator", values_to = total_label)

  by_subspecialty |> left_join(dept_total, by = "Indicator")
}

# Thin wrapper - keeps existing calls like build_table12(bed_metrics, "Jan") working.
# NOTE: bed_metrics is already Surgery-only (built via inner_join(surgical_ward_map)
# in Section 6), so this wrapper works directly on it as before.
build_table12 <- function(bed_metrics_df, target_month) {
  monthly <- bed_metrics_df |> filter(Month == target_month)
  if (nrow(monthly) == 0) return(tibble())

  by_subspecialty <- monthly |>
    rowwise() |>
    mutate(
      row = list(compute_indicators(Beds, Admissions, Discharges, Deaths,
                                     `Trans Ins`, `Trans Out`, `Patients Days`, Days))
    ) |>
    ungroup() |>
    select(Subspecialty, row) |>
    unnest(row) |>
    pivot_longer(-Subspecialty, names_to = "Indicator", values_to = "Value") |>
    pivot_wider(names_from = Subspecialty, values_from = Value)

  totals_row_inputs <- monthly |>
    summarise(
      Beds = sum(Beds), Admissions = sum(Admissions), Discharges = sum(Discharges),
      Deaths = sum(Deaths), `Trans Ins` = sum(`Trans Ins`), `Trans Out` = sum(`Trans Out`),
      `Patients Days` = sum(`Patients Days`), Days = first(Days)
    )

  subbmc_total <- with(totals_row_inputs,
    compute_indicators(Beds, Admissions, Discharges, Deaths,
                        `Trans Ins`, `Trans Out`, `Patients Days`, Days)
  ) |>
    pivot_longer(everything(), names_to = "Indicator", values_to = "Surgery-SubBMC")

  by_subspecialty |> left_join(subbmc_total, by = "Indicator")
}

# Usage across departments (once each department has its own
# Admissions/Discharges/Patients Days/Beds data available):
# table12_surgery_jan <- build_department_table(all_indicators_long, "Surgery", "Jan")
# table12_medicine_jan <- build_department_table(all_indicators_long, "Internal Medicine", "Jan")
# print(table12_medicine_jan)

# ------------------------------------------------------------
# 9. MANAGEMENT-STYLE FORMATTED TABLE (green-blue theme)
#    Takes the output of build_department_table() / build_table12()
#    and styles it for a management audience: dark teal header,
#    light green/blue banded rows, and a green-blue gradient
#    highlighting the rate-based rows (% Bed Occupancy, % Death
#    Rate, etc.) so high/low values are visually obvious at a
#    glance. Exports to Word (.docx), HTML, and PDF from one object.
# ------------------------------------------------------------
library(flextable)
library(officer)

style_management_table <- function(tbl, title = NULL, subtitle = NULL,
                                    rate_rows = c("% Bed Occupancy", "% Death Rate",
                                                  "Average Daily Bed Occupancy")) {

  n_cols <- ncol(tbl)
  total_col_name <- names(tbl)[n_cols]   # last column is always the department/subBMC total

  ft <- flextable(tbl) |>
    set_table_properties(layout = "autofit") |>
    font(fontname = "Arial", part = "all") |>
    fontsize(size = 10, part = "all") |>
    fontsize(size = 12, part = "header") |>

    # Header: dark teal, white bold text
    bg(part = "header", bg = "#0E5A5A") |>
    color(part = "header", color = "white") |>
    bold(part = "header") |>
    align(align = "center", part = "header") |>

    # Body base: alternating light green / light blue banding
    bg(i = seq(1, nrow(tbl), by = 2), bg = "#E8F6F3", part = "body") |>
    bg(i = seq(2, nrow(tbl), by = 2), bg = "#E8F1F8", part = "body") |>

    # First column (Indicator names) bold, left-aligned
    bold(j = 1, part = "body") |>
    align(j = 1, align = "left", part = "body") |>
    align(j = 2:n_cols, align = "center", part = "body") |>

    # Total column: distinct darker shade, bold, to draw the eye
    bg(j = total_col_name, bg = "#CFE8E3", part = "body") |>
    bold(j = total_col_name, part = "body") |>

    border_outer(border = fp_border(color = "#0E5A5A", width = 1.5)) |>
    border_inner_h(border = fp_border(color = "#CBD5D8", width = 0.5)) |>
    border_inner_v(border = fp_border(color = "#CBD5D8", width = 0.5))

  # Green-blue gradient on rate-based rows: darker teal = higher value
  gradient_pal <- colorRampPalette(c("#EAF7F5", "#0E5A5A"))(101)

  for (row_label in rate_rows) {
    row_idx <- which(tbl$Indicator == row_label)
    if (length(row_idx) == 0) next

    for (col in 2:n_cols) {
      val <- suppressWarnings(as.numeric(tbl[[col]][row_idx]))
      if (is.na(val)) next
      val_clamped <- max(0, min(100, val))          # gradient scaled 0-100
      cell_color <- gradient_pal[round(val_clamped) + 1]
      text_color <- if (val_clamped > 55) "white" else "black"
      ft <- ft |>
        bg(i = row_idx, j = col, bg = cell_color, part = "body") |>
        color(i = row_idx, j = col, color = text_color, part = "body")
    }
  }

  if (!is.null(title)) {
    ft <- ft |> set_caption(caption = as_paragraph(as_chunk(title, props = fp_text(bold = TRUE, font.size = 14))))
  }

  ft
}

# ------------------------------------------------------------
# Export helpers - Word, HTML, PDF from the same styled table
# ------------------------------------------------------------
export_management_table <- function(ft, base_path) {
  save_as_docx(ft, path = paste0(base_path, ".docx"))
  save_as_html(ft, path = paste0(base_path, ".html"))
  # PDF export requires Chrome/Chromote installed - comment out if unavailable
  tryCatch(
    save_as_image(ft, path = paste0(base_path, ".png")),   # PDF-quality image fallback
    error = function(e) message("PNG/PDF export skipped (Chrome/Chromote not available): ", e$message)
  )
  message("Saved: ", base_path, ".docx / .html")
}

# Usage:
# table12_jan <- build_table12(bed_metrics, "Jan")
# ft_jan <- style_management_table(
#   table12_jan,
#   title = "TTH Surgery Ward Indicators - January 2026"
# )
# ft_jan   # print in RStudio Viewer to preview
# export_management_table(ft_jan, "TTH_Surgery_Jan_2026")

# ------------------------------------------------------------
# 8. EXCEL EXPORT - one sheet per month, styled like Table 12
# ------------------------------------------------------------
library(openxlsx)

export_table12_workbook <- function(all_indicators_long, department, out_path,
                                     months = c("Jan","Feb","Mar","Apr","May","Jun"),
                                     ward_map = ward_group_map) {

  wb <- createWorkbook()

  header_style <- createStyle(
    fontName = "Arial", textDecoration = "bold",
    fgFill = "#D9D9D9", border = "TopBottomLeftRight", halign = "center"
  )
  label_style <- createStyle(
    fontName = "Arial", textDecoration = "bold",
    border = "TopBottomLeftRight"
  )
  value_style <- createStyle(
    fontName = "Arial", border = "TopBottomLeftRight", halign = "center"
  )
  total_col_style <- createStyle(
    fontName = "Arial", textDecoration = "bold",
    fgFill = "#F2F2F2", border = "TopBottomLeftRight", halign = "center"
  )

  for (m in months) {

    tbl <- build_department_table(all_indicators_long, department, m, ward_map = ward_map)
    if (nrow(tbl) == 0) next   # skip months with no data

    sheet_name <- m
    addWorksheet(wb, sheet_name)

    # Title row
    writeData(wb, sheet_name,
              x = paste0("Table 12 Ward Indicators - ", department, ", ", m),
              startCol = 1, startRow = 1)
    mergeCells(wb, sheet_name, cols = 1:ncol(tbl), rows = 1)
    addStyle(wb, sheet_name, createStyle(fontName = "Arial", textDecoration = "bold",
                                          fontSize = 13), rows = 1, cols = 1)

    # Header + data, starting row 3
    writeData(wb, sheet_name, tbl, startRow = 3, colNames = TRUE)

    n_rows <- nrow(tbl)
    n_cols <- ncol(tbl)

    addStyle(wb, sheet_name, header_style, rows = 3, cols = 1:n_cols, gridExpand = TRUE)
    addStyle(wb, sheet_name, label_style, rows = 4:(3 + n_rows), cols = 1, gridExpand = TRUE)
    addStyle(wb, sheet_name, value_style, rows = 4:(3 + n_rows), cols = 2:(n_cols - 1), gridExpand = TRUE)
    addStyle(wb, sheet_name, total_col_style, rows = 4:(3 + n_rows), cols = n_cols, gridExpand = TRUE)

    setColWidths(wb, sheet_name, cols = 1, widths = 28)
    setColWidths(wb, sheet_name, cols = 2:n_cols, widths = 16)
    freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
  }

  saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Saved: ", out_path)
}

# Usage - one workbook per department:
export_table12_workbook(all_indicators_long, "Surgery", "/Users/asi/Library/CloudStorage/OneDrive-SmartWorkplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Cleaned_Data/TTH_Surgery_Ward_Indicators_Jan-Jun_2026.xlsx")
# export_table12_workbook(all_indicators_long, "Internal Medicine", "TTH_Medicine_Ward_Indicators_Jan-Jun_2026.xlsx")
