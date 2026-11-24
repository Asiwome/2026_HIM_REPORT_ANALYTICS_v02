# ============================================================
# Admission/Discharge ward-movement -> visit-level LOS analysis
# ============================================================
# Assumes each row = one ward "segment" within a visit_no.
# A visit_no with multiple rows = patient moved wards during
# that stay; only the LAST row's outcome is the real discharge
# outcome (earlier rows show "Trans-Out / Bed Changed").

library(tidyverse)
library(lubridate)

# ---- 1. Load & parse -----------------------------------------------------
df <- read_csv("your_data.csv") %>%
  mutate(
    # adjust the parser to match your actual date format if this fails
    date_and_time_of_admission = ymd_hms(date_and_time_of_admission),
    date_and_time_of_discharge = ymd_hms(date_and_time_of_discharge)
  )

# ---- 2. Data quality check: gaps/overlaps between consecutive segments ---
# In a clean record, one segment's discharge time should equal the next
# segment's admission time (continuous stay). Anything else is worth flagging.
segment_gaps <- df %>%
  arrange(visit_no, date_and_time_of_admission) %>%
  group_by(visit_no) %>%
  mutate(
    prev_discharge = lag(date_and_time_of_discharge),
    gap_hours = as.numeric(difftime(date_and_time_of_admission, prev_discharge, units = "hours"))
  ) %>%
  ungroup() %>%
  filter(!is.na(gap_hours) & gap_hours != 0)

# View this before trusting totals downstream:
# View(segment_gaps)

# ---- 3. Collapse to one row per visit: path, first/last ward, outcome, LOS
visit_summary <- df %>%
  arrange(visit_no, date_and_time_of_admission) %>%
  group_by(visit_no) %>%
  summarise(
    first_ward       = first(ward),
    last_ward        = last(ward),
    ward_path        = paste(ward, collapse = " -> "),
    n_wards          = n(),
    n_transfers      = n() - 1,
    final_outcome    = last(outcome_of_admission_discharge), # rename to your real column
    admission_time   = first(date_and_time_of_admission),
    discharge_time   = last(date_and_time_of_discharge),
    total_los_hours  = as.numeric(difftime(discharge_time, admission_time, units = "hours")),
    total_los_days   = total_los_hours / 24,
    directorate      = first(directorate_sub_bmc),
    .groups = "drop"
  )

# ---- 4. LOS by final outcome ---------------------------------------------
los_by_outcome <- visit_summary %>%
  group_by(final_outcome) %>%
  summarise(
    n_visits        = n(),
    mean_los_days   = mean(total_los_days, na.rm = TRUE),
    median_los_days = median(total_los_days, na.rm = TRUE),
    min_los_days    = min(total_los_days, na.rm = TRUE),
    max_los_days    = max(total_los_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_visits))

# ---- 5. LOS by ward (time actually spent in each ward, segment-level) ----
los_by_ward <- df %>%
  mutate(segment_los_hours = as.numeric(difftime(
    date_and_time_of_discharge, date_and_time_of_admission, units = "hours"
  ))) %>%
  group_by(ward) %>%
  summarise(
    n_segments   = n(),
    mean_hours   = mean(segment_los_hours, na.rm = TRUE),
    median_hours = median(segment_los_hours, na.rm = TRUE),
    total_hours  = sum(segment_los_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_hours))

# ---- 6. Quick look --------------------------------------------------------
print(head(visit_summary))
print(los_by_outcome)
print(los_by_ward)

# Optional: write out
# write_csv(visit_summary, "visit_summary.csv")
# write_csv(los_by_outcome, "los_by_outcome.csv")
# write_csv(los_by_ward, "los_by_ward.csv")

# ============================================================
# 7. Convert to an event/activity log for process mining
# ============================================================
# install.packages("bupaR")
library(bupaR)

# Attach the visit-level final outcome onto every segment row as a
# case attribute, so you can slice process maps by outcome later.
df_pm <- df %>%
  left_join(
    visit_summary %>% select(visit_no, final_outcome),
    by = "visit_no"
  )

# bupaR's activitylog() is built for exactly this shape: one row per
# activity instance with its own start and complete timestamp.
admissions_log <- activitylog(
  df_pm,
  case_id       = "visit_no",
  activity_id   = "ward",              # each ward stay = one activity
  resource_id   = "directorate_sub_bmc",
  timestamps    = c("date_and_time_of_admission", "date_and_time_of_discharge")
)

# Quick checks
summary(admissions_log)

# Process map (needs DiagrammeR + processmapR, installed with bupaR)
admissions_log %>% process_map()

# Filter to a subset of outcomes before mapping, e.g. only visits
# that ended in death, to see if their paths look different:
# admissions_log %>%
#   filter(final_outcome == "Died") %>%
#   process_map()

# Trace variants: which ward sequences are most common
admissions_log %>% trace_explorer(coverage = 0.8)

# Dotted chart: visualise timing of every activity instance across cases
admissions_log %>% dotted_chart(x = "absolute")

# If you need a strict event log (start + complete as separate rows,
# the classical XES-style format used by tools like ProM/PM4Py):
events_log <- admissions_log %>% to_eventlog()
# write_xes(events_log, "admissions_eventlog.xes")  # for ProM / PM4Py

# ---- 7b. Directorate-level log (coarser view, department to department) --
# Consecutive segments can share the same directorate (e.g. two theatre
# bounces both tagged TRAUMA & ORTHO) — collapse those into one activity
# instance first, otherwise the process map just shows repeated self-loops
# instead of a genuine directorate-to-directorate transition.
df_directorate <- df_pm %>%
  arrange(visit_no, date_and_time_of_admission) %>%
  group_by(visit_no) %>%
  mutate(
    change_block = cumsum(directorate_sub_bmc != lag(directorate_sub_bmc, default = first(directorate_sub_bmc)))
  ) %>%
  group_by(visit_no, change_block) %>%
  summarise(
    directorate_sub_bmc = first(directorate_sub_bmc),
    seg_admission = first(date_and_time_of_admission),
    seg_discharge = last(date_and_time_of_discharge),
    final_outcome = first(final_outcome),
    .groups = "drop"
  ) %>%
  arrange(visit_no, seg_admission)

directorate_log <- activitylog(
  df_directorate,
  case_id     = "visit_no",
  activity_id = "directorate_sub_bmc",
  timestamps  = c("seg_admission", "seg_discharge")
)

summary(directorate_log)
directorate_log %>% process_map()
directorate_log %>% trace_explorer(coverage = 0.8)
directorate_log %>% dotted_chart(x = "absolute")

# Compare: ward_log gives the fine-grained bed-move picture (noisy,
# useful for bottleneck/theatre-bounce detection); directorate_log
# gives the clinical-pathway picture (cleaner, useful for outcome
# comparison and high-level flow). Keep both around and use whichever
# fits the question you're asking.
ward_log <- admissions_log  # renamed for clarity alongside directorate_log












# ============================================================
# 8. Curated tables for other officers to query directly
# ============================================================
# Design: two tables, different grain.
#  - visit_master: one row per unique, complete visit -> for LOS/outcome
#    questions (most officers' use case).
#  - ward_segments_clean: one row per ward stay, flagged -> for drill-down
#    on where time was spent.

REAL_OUTCOMES <- c("Discharged [Normal]", "Died", "DAMA", "Absconded")  # extend to your actual outcome labels
REUSE_GAP_HOURS <- 72  # threshold above which a "gap" suggests a reused visit_no, not a data error

ward_segments_clean <- df %>%
  arrange(visit_no, date_and_time_of_admission) %>%
  group_by(visit_no) %>%
  mutate(
    seq_no             = row_number(),
    is_first_segment   = seq_no == 1,
    is_last_segment    = seq_no == n(),
    prev_discharge     = lag(date_and_time_of_discharge),
    gap_hours          = as.numeric(difftime(date_and_time_of_admission, prev_discharge, units = "hours")),
    gap_flag           = !is.na(gap_hours) & gap_hours != 0,
    suspected_id_reuse = !is.na(gap_hours) & gap_hours > REUSE_GAP_HOURS,
    segment_los_hours  = as.numeric(difftime(date_and_time_of_discharge, date_and_time_of_admission, units = "hours"))
  ) %>%
  ungroup() %>%
  mutate(visit_key = paste(visit_no, format(if_else(is_first_segment, date_and_time_of_admission, as.POSIXct(NA)), "%Y%m%d"))) %>%
  group_by(visit_no) %>%
  fill(visit_key, .direction = "downup") %>%
  ungroup() %>%
  select(visit_key, visit_no, seq_no, ward, directorate_sub_bmc,
         date_and_time_of_admission, date_and_time_of_discharge,
         outcome_of_admission_discharge, segment_los_hours,
         is_first_segment, is_last_segment, gap_flag, suspected_id_reuse)

visit_master <- ward_segments_clean %>%
  group_by(visit_key, visit_no) %>%
  summarise(
    n_segments        = n(),
    first_ward        = first(ward),
    last_ward         = last(ward),
    first_directorate  = first(directorate_sub_bmc),
    last_directorate   = last(directorate_sub_bmc),
    ward_path         = paste(ward, collapse = " -> "),
    directorate_path  = paste(rle(directorate_sub_bmc)$values, collapse = " -> "),  # collapsed, no repeats
    final_outcome     = last(outcome_of_admission_discharge),
    admission_time    = first(date_and_time_of_admission),
    discharge_time    = last(date_and_time_of_discharge),
    total_los_hours   = as.numeric(difftime(discharge_time, admission_time, units = "hours")),
    total_los_days    = total_los_hours / 24,
    n_transfers       = n_segments - 1,
    any_gap_flag      = any(gap_flag),
    any_suspected_reuse = any(suspected_id_reuse),
    is_complete       = final_outcome %in% REAL_OUTCOMES,
    .groups = "drop"
  )

# Officers filtering by ward/directorate for LOS/outcome analysis should
# use visit_master and typically exclude incomplete visits:
# visit_master %>% filter(is_complete, !any_suspected_reuse)

# Officers drilling into ward-level dwell time use ward_segments_clean,
# typically after excluding suspected ID-reuse visits:
# ward_segments_clean %>% filter(!visit_no %in% (visit_master %>% filter(any_suspected_reuse) %>% pull(visit_no)))

write_csv(visit_master, "visit_master.csv")
write_csv(ward_segments_clean, "ward_segments_clean.csv")

# Data dictionary so other officers know what each field means without
# asking you directly -- keep this alongside the two CSVs.
data_dictionary <- tibble::tribble(
  ~table, ~field, ~description,
  "visit_master", "visit_key", "Unique key: visit_no + first admission date; guards against reused visit_no values",
  "visit_master", "is_complete", "TRUE if the visit's last segment has a genuine discharge/death/DAMA outcome, not a transfer",
  "visit_master", "any_suspected_reuse", "TRUE if a >72hr unexplained gap was found within this visit_no -- verify before trusting LOS",
  "visit_master", "directorate_path", "Sequence of directorates visited, consecutive repeats collapsed into one",
  "ward_segments_clean", "gap_flag", "TRUE if this segment's admission time didn't match the prior segment's discharge time",
  "ward_segments_clean", "segment_los_hours", "Hours spent in this specific ward stay only, not the whole visit"
)
write_csv(data_dictionary, "data_dictionary.csv")
