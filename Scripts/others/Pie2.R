 ------------------------------------------------
# 3. WARD -> SURGICAL SUB-SPECIALTY LOOKUP
#    One table to maintain, instead of hard-coded formulas.
#    Add rows here as new ward-naming variants show up in other
#    indicator files (Patient Days, Discharges, etc.).
#    Ward names are upper-cased to match the normalisation above.
# ------------------------------------------------------------
surgical_ward_map <- tribble(
  ~Ward,                                    ~Subspecialty,
  "GENERAL SURGERY WARD",                   "General Surgery",
  "FEMALE GENERAL SURGICAL WARD",           "General Surgery",
  "MALE GENERAL SURGICAL WARD",             "General Surgery",
  "GENERAL SURGERY SIDE WARD",              "General Surgery",
  "MALE PAED SURGERY",                      "Paediatric Surgery",
  "FEMALE PAED SURGERY",                    "Paediatric Surgery",
  "INTENSIVE CARE UNIT (ICU) WARD",         "ICU",
  "ICU VIP",                                "ICU",
  "PLASTIC SURGERY WARD",                   "Plastic Surgery",
  "PLASTIC / THORACIC WARD",                "Plastic/Thoracic Surgery",
  "THORACIC SURGERY WARD",                  "Thoracic Surgery",
  "MALE NEUROSURGERY WARD",                 "Neurosurgery",
  "FEMALE NEUROSURGERY WARD",               "Neurosurgery",
  "NEUROSURGERY WARD",                      "Neurosurgery",
  "MALE UROLOGY WARD",                      "Urology",
  "FEMALE UROLOGY WARD",                    "Urology",
  "UROLOGY STAFF WARD",                     "Urology",
  "PAEDITRICS UROLOGY WARD",                "Urology",
  "UROLOGY WARD",                           "Urology"
)

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

# ------------------------------------------------------------
# 5. WORKED EXAMPLE - Admissions
# ------------------------------------------------------------
admissions_long <- read_indicator_workbook(
  path           = "2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx",
  indicator_name = "Admissions"
)

qa_admissions <- qa_check_against_workbook(
  "2026-JAN-JUN_TTH-ADMISSIONS_V01.xlsx",
  admissions_long
)
print(qa_admissions)   # every row should show Match = TRUE

surg_admissions_summary <- admissions_long |>
  summarise_by_subspecialty()

print(surg_admissions_summary)

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
bed_metrics <- all_indicators_long |>
  inner_join(surgical_ward_map, by = "Ward") |>
  group_by(Indicator, Subspecialty, Month) |>
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Indicator, values_from = Value) |>
  left_join(days_in_month, by = "Month") |>
  mutate(
    Bed_Days_Available = Beds * Days,
    Occupancy_Rate_Pct  = round(`Patients Days` / Bed_Days_Available * 100, 1),
    ALOS_Days           = round(`Patients Days` / Discharges, 1),
    Turnover_Interval   = round((Bed_Days_Available - `Patients Days`) / Discharges, 1),
    # Net bed-flow check: Admissions + Trans Ins should reconcile
    # against Discharges + Trans Out (+ Deaths once added). Useful
    # as a data-quality sanity check, not a "metric" per se.
    Net_Flow_Check      = (Admissions + `Trans Ins`) - (Discharges + `Trans Out`)
  )

print(bed_metrics)
