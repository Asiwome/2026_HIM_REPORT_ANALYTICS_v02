
#-------------Cleaning and Joining data IPD Visits --------------------#
# Author: Asiwome
# Date : 2026/04/11
# ---------------------------------------------------------#
library(dplyr)
library(haven)
library(tidyr)
library(tibble)
library(purrr)
library(janitor)
library(readr)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

#-------------- Import  data
# All visits
ipd_visit <- readxl::read_excel("Database_Registers/IPD_register/InpatientMorbidityRegister_01Jan202630Jun2026.xlsx") |> 
  clean_names()

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
ipd_visit0 <- ipd_visit |> 
  mutate(
    # adjust the parser to match your actual date format if this fails
    date_time_of_admission = as_datetime(date_and_time_of_admission),
    date_time_of_discharge = as_datetime(date_and_time_of_discharge)
  )

# ---- 2. Data quality check: gaps/overlaps between consecutive segments ---
# In a clean record, one segment's discharge time should equal the next
# segment's admission time (continuous stay). Anything else is worth flagging.
segment_gaps <- ipd_visit0 |> 
  arrange(visit_no, date_time_of_admission) |> 
  group_by(visit_no) %>%
  mutate(
    prev_discharge = lag(date_time_of_admission),
    gap_hours = as.numeric(difftime(date_time_of_admission, prev_discharge, units = "hours"))
  ) %>%
  ungroup() %>%
  filter(!is.na(gap_hours) & gap_hours != 0)

segment_gaps %>%
  select(visit_no, ward, date_and_time_of_admission, prev_discharge, gap_hours) %>%
  arrange(desc(abs(gap_hours))) %>%
  print(n = 20)
# View this before trusting totals downstream:
# View(segment_gaps)

# ---- 3. Collapse to one row per visit: path, first/last ward, outcome, LOS
visit_summary <- ipd_visit0 %>%
  arrange(visit_no, date_time_of_admission) %>%
  group_by(visit_no) %>%
  summarise(
    first_ward       = first(ward),
    last_ward        = last(ward),
    ward_path        = paste(ward, collapse = " -> "),
    n_wards          = n(),
    n_transfers      = n() - 1,
    final_outcome    = last(outcome_of_admission_discharged_died_abscond_dama_transout), # rename to your real column
    admission_time   = first(date_time_of_admission),
    discharge_time   = last(date_time_of_admission),
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
los_by_ward <- ipd_visit0 %>%
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
library(DiagrammeR)
library(processmapR)

# Attach the visit-level final outcome onto every segment row as a
# case attribute, so you can slice process maps by outcome later.
df_pm <- ipd_visit0 %>%
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
  timestamps    = c("date_time_of_admission", "date_time_of_discharge")
)

admissions_log <- df_pm %>%
  dplyr::rename(start = date_time_of_admission,
                complete = date_time_of_discharge) %>%
  activitylog(
    case_id     = "visit_no",
    activity_id = "ward",
    resource_id = "directorate_sub_bmc",
    timestamps  = c("start", "complete")
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

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

map <- process_map(admissions_log, render = FALSE)  # returns a grViz/dgr_graph object

map %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png("process_map.png", width = 3000, height = 3000)
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


































#-------Wrangling

ipf_visit_df <- opd_visit %>%
  mutate(
    visitdate = as_date(schedule_date),
    patient_id = str_trim(toupper(patient_id)),
    pid = paste(patient_id,visitdate,clinic,sep = "_")
  ) %>% rename(visit_date = visitdate) |> 
  # Insurance
  mutate(
    payer = case_when(
      type_of_insurance == "NATIONAL HEALTH INSURANCE SCHEME" ~ "NHIS",
      type_of_insurance %in% c("SELF PAY / UNINSURED - GH", "FOREIGNER [PAYING]",
                               "CORPORATE CLINIC - NON GHANAIAN", "CORPORATE CLINIC - GHANAIAN") ~ "Self-pay/Cash",
      TRUE ~ "Private Insurance"  # <-- Removed the extra leading ~ here
    )
  ) |> 
  group_by(pid) |> 
  slice_min(order_by = visit_date, n = 1, with_ties = FALSE) |> 
  ungroup()

# Create the lookup tibble clinics
lookup_tbl <- tribble(
  ~old, ~New,
  "ADMISSION VISIT", "Admission",
  "ANTENATAL CONSULT", "Antenatal Clinic",
  "AUDIOLOGY CONSULT", "Audiology Clinic",
  "ADULT SICKLE CELL" ,"Adult Sickle Cell",
  "Abdominal Parasyntesis Service","Abdominal Parasyntesis Service",
  "BREAST CONSULT", "Breast Clinic",
  "BREAST SCREENING", "Breast Clinic",
  "Brought in Dead (BID)", "Brought in Dead (BID)",
  "CARDIOLOGY CONSULT", "Cardiology Clinic",
  "ECG" , "Cardiology Clinic",
  "CLINICAL PSYCHOLOGICAL CONSULT", "Clinical Psychology Clinic",
  "CLUB FOOT", "Club Foot",
  "COLORECTAL CONSULT", "Colorectal Clinic",
  "Antenatal Consultation (CC)",  "Co-operate Clinic",
  "Audiology Consultation (CC)","Co-operate Clinic",
  "Aneasthesia Consultation (CC)", "Co-operate Clinic",
  "Dental Consultation (CC)", "Co-operate Clinic",
  "Diet Consultation (CC)", "Co-operate Clinic",
  "ENT Consultation (CC)", "Co-operate Clinic",
  "Eye Clinic Consultation (CC)", "Co-operate Clinic",
  "General Consultation (CC)", "Co-operate Clinic",
  "General Surgery Consultation (CC)", "Co-operate Clinic",
  "General Surgery Consultation  (CC)", "Co-operate Clinic",
  "Gyneacology Consultation (CC)", "Co-operate Clinic",
  "Neurology Clinic Consult (CC)", "Co-operate Clinic",
  "Orthopaedics Consultation (CC)", "Co-operate Clinic",
  "Paediatric OPD Consultation (CC)", "Co-operate Clinic",
  "Paediatric Surgery Consultation (CC)","Co-operate Clinic",
  "Physician Medical Consultation (CC)", "Co-operate Clinic",
  "Physiotherapy Consultation (CC)", "Co-operate Clinic",
  "Plastic Surgery Consultation (CC)", "Co-operate Clinic",
  "Neurosurgery Consultation (CC)","Co-operate Clinic",
  "Urology Consultation (CC)","Co-operate Clinic",
  "Thoracic Specialist Consultation (CC)", "Co-operate Clinic",
  "Speech Therapy Clinic (CC)","Co-operate Clinic",
  "Renal Consultation (CC)","Co-operate Clinic",
  "Psychiatric Consultation (CC)","Co-operate Clinic",
  "Colorectal Consultation (CC)","Co-operate Clinic",
  "DAY PROCEDURE", "Day Procedures",
  "DENTAL CONSULTATION", "Dental Clinic",
  "DIABETES CLINIC", "Diabetes Clinic",
  "DIALYSIS (CASH)", "Dialysis Clinic",
  "DIRECT ADMISSION", "Admission",
  "ENDOCRINE" , "Endocrine",
  "Diet Therapy Clinic", "Diet Therapy Clinic",
  "NUTRITION & DIET THERAPY", "Diet Therapy Clinic",
  "EMERGENCY", "Emergency",
  "ENT CONSULTATION", "ENT Clinic",
  "EXERCISE THERAPY", "Exercise therapy",
  "EXERCISE THERAPY (MAINTENANCE PHASE)","Exercise therapy",
  "FAMILY MEDICINE", "Family Medicine",
  "GENERAL PHYSIO CONSULTATION", "General Physiotherapy Clinic",
  "PAEDIATRIC PHYSIO","General Physiotherapy Clinic",
  "GENERAL SURGERY", "General Surgery Clinic",
  "GYNAE CONSULT", "Gynaecological Clinic",
  "INFECTIOUS DISEASE", "Infectious Disease",
  "Gastroenterology", "Gastroenterology Clinic",
  "General Poly Consultation", "General Poly  Clinic",
  "Haemodialysis 1st Visit", "Dialysis Clinic",
  "Haemodialysis 2nd Visit", "Dialysis Clinic",
  "Haemodialysis 3rd Visit", "Dialysis Clinic",
  "Haemodialysis 4th Visit", "Dialysis Clinic",
  "Haemodialysis 5th Visit", "Dialysis Clinic",
  "Haemodialysis 6th Visit", "Dialysis Clinic",
  "Haemodialysis 7th Visit", "Dialysis Clinic",
  "Haemodialysis 8th Visit", "Dialysis Clinic",
  "Heart Failure Consult", "Heart Failure Consult",
  "NEURO CONSULT", "Neurology Clinic",
  "NEURO SURGERY", "Neurosurgery Clinic",
  "NICU FOLLOW UP CLINIC", "NICU Clinic",
  "NICU OPD CONSULT", "NICU Clinic",
  "NICU OPDONSULT", "NICU OPD Clinic",
  "O&G Anaesthesisa Clinic", "O&G Anaesthesia Clinic",
  "OPTHALMOLOGY", "Eye Clinic",
  "OPTOMETRIST [SPECIALIST CONSULTATION]", "Eye Clinic",
  "PAED ONCOLOGY CONSULT", "Paediatric Oncology Clinic",
  "PAED RENAL", "Paediatric Renal Clinic",
  "PAED SICKLE CELL", "Paediatric Sickle Cell Clinic",
  "PAEDIATRIC CONSULTATION" , "Paediatric Consulting Clinic",
  "PAEDIATRIC EMERGENCY", "Paediatric Emergency Clinic",
  "PAEDIATRIC SPECIALIST CONSULT", "Paediatric Specialist Clinic",
  "PAEDIATRIC SURGERY CONSULT", "Paediatric Surgery Clinic",
  "PAIN THERAPY" , "Pain Therapy",
  "PALLATIVE CARE", "Pallative Clinic",
  "PHYSICIAN GENERAL" , "General Physician Clinic",
  "PHYSIO SESSION [NHIS]", "Physiotherapy Clinic",
  "PLASTIC SURGER CONSULT", "Plastic Surgery Clinic",
  "POSTNATAL CONSULT", "Postnatal Clinic",
  "PSYCHIATRY CONSULT", "Psychiatry Clinic",
  "RENAL CLINIC", "Renal Clinic",
  "SPEECH THERAPY", "Speech Therapy Clinic",
  "ONCOLOGY CONSULT", "Oncology Clinic",
  "Chemotherapy","Oncology Clinic",
  "OBSETRICS EMERGENCY CONSULT", "Obstestircs Emergency",
  "OPTHALMOLOGY", "Eye Clinic",
  "ORTHOPAEDICS CONSULT", "Orthopaedics Clinic",
  "OPTOMETRIST [SPECIALIST CONSULTATION]", "Eye Clinic",
  "SURGEREY ANAESTESIA CONSULT", "Surgery Anaesthesia Clinic",
  "SURGERY ANAESTESIA CONSULT", "Surgery Anaesthesia Clinic",
  "Specialized - Diet Therapy Clinic", "Diet Therapy Clinic",
  "THORACIC CONSULTATION", "Thoracic Surgery Clinic",
  "TUBERCULOSIS CONSULT", "Tuberculosis Clinic",
  "TUBERCULOSISONSULT", "Tuberculosis Clinic",
  "UROLOGY CONSULT", "Urology Clinic",
  "UROLOGY DAY SERVICE", "Urology Clinic",
  "UROLOGY DAYSERVICE", "Urology Clinic",
  "Wound Management" , "Wound Management Clinic",
  "Direct Lab Request" ,"Laboratory investigation",
  "Direct Radiology Request", "Radiology/Imaging investigation",
  "HR Attachment Forms", "HR Activity",
  "HR Employment forms", "HR Activity",
  "Medical Forms", "HR Activity",
  "Individual International Research Program Fee", "Research Activity",
  "Local PHD Research Program Fee", "Research Activity",
  "Local Postgraduate Research Program Fee", "Research Activity",
  "Local Undergraduate Research Program Fee",  "Research Activity",
  "Transport - Ambulance External","Transport activity",
  "Upper GI Endoscopy", "Radiology/Imaging investigation"
)


# Create the lookup tibble departments/SUBBMC
lookup_dept <- tribble(
  ~old, ~New,
  "Abdominal Parasyntesis Service", "Internal Medicine",
  "Admission", "IPD" ,
  "Adult Sickle Cell",  "Internal Medicine",
  "Antenatal Clinic", "Obstetrics & Gynaecology",
  "Audiology Clinic", "DEENT",
  "Breast Clinic", "Surgery",
  "Brought in Dead (BID)", "Accident & Emergency",
  "Cardiology Clinic", "Internal Medicine",
  "Clinical Psychology Clinic" , "Internal Medicine",
  "Club Foot","Trauma & Orthopaedics",
  "Co-operate Clinic", "Co-operate Clinic",
  "Colorectal Clinic", "Surgery",
  "Day Procedures", "DEENT",
  "Dental Clinic",  "DEENT",
  "Diabetes Clinic", "Internal Medicine",
  "Dialysis Clinic", "Internal Medicine",
  "Diet Therapy Clinic", "Internal Medicine",
  "ENT Clinic", "DEENT",
  "Emergency","Surgery",
  "Endocrine", "Internal Medicine",
  "Exercise therapy","Poly Clinic",
  "Eye Clinic", "DEENT",
  "Family Medicine", "Internal Medicine",
  "Gastroenterology Clinic", "Internal Medicine",
  "General Physician Clinic", "Internal Medicine",
  "General Physiotherapy Clinic", "Poly Clinic",
  "General Poly  Clinic", "Poly Clinic",
  "General Surgery Clinic", "Surgery",
  "Gynaecological Clinic","Obstetrics & Gynaecology",
  "Heart Failure Consult", "Internal Medicine",
  "Infectious Disease", "Internal Medicine",
  "MALE CONSULT","Obstetrics & Gynaecology",
  "NICU Clinic","Paediatrics & Child Health",
  "Neurology Clinic", "Internal Medicine",
  "Neurosurgery Clinic",  "Surgery",
  "O&G Anaesthesia Clinic","Obstetrics & Gynaecology",
  "O&G Wound Clinic","Obstetrics & Gynaecology",
  "OPD REVIEW CONSULT","Obstetrics & Gynaecology",
  "Obstestircs Emergency","Obstetrics & Gynaecology",
  "Oncology Clinic" , "Surgery",
  "Orthopaedics Clinic","Trauma & Orthopaedics",
  "Paediatric Consulting Clinic","Paediatrics & Child Health",
  "Paediatric Emergency Clinic","Paediatrics & Child Health",
  "Paediatric Oncology Clinic","Paediatrics & Child Health",
  "Paediatric Renal Clinic", "Internal Medicine",
  "Paediatric Sickle Cell Clinic","Paediatrics & Child Health",
  "Paediatric Specialist Clinic","Paediatrics & Child Health",
  "Paediatric Surgery Clinic", "Surgery",
  "Pain Therapy","Poly Clinic",
  "Pallative Clinic","Obstetrics & Gynaecology",
  "Physiotherapy Clinic","Poly Clinic",
  "Plastic Surgery Clinic",  "Surgery",
  "Postnatal Clinic","Obstetrics & Gynaecology",
  "Psychiatry Clinic","Poly Clinic",
  "Renal Clinic" ,"Internal Medicine",
  "Speech Therapy Clinic", "DEENT",
  "Surgery Anaesthesia Clinic", "Surgery",
  "Thoracic Surgery Clinic",  "Surgery",
  "Tuberculosis Clinic", "Internal Medicine",
  "Urology Clinic",  "Surgery",
  "Wound Management Clinic", "Poly Clinic",
  "Laboratory investigation", "Direct investigations",
  "Radiology/Imaging investigation","Direct investigations",
  "HR Activity","HR/ADMIN/Research Activity",
  "Research Activity","HR/ADMIN/Research Activity",
  "Transport activity","HR/ADMIN/Research Activity"
)


opd_visit_df <- opd_visit_df %>%
  mutate(
    visiting_clinic = recode(clinic, !!!deframe(lookup_tbl)),
    visiting_dept = recode(visiting_clinic, !!!deframe(lookup_dept))
  )



lookup_visit <- opd_visit_df %>%
  distinct(patient_id) %>%
  mutate(
    pseudo_id = paste0(
      "TTH",
      str_pad(row_number(), 8, pad = "0")
    )
  )


opd_visit_pseudo <- opd_visit_df %>%
  left_join(lookup_visit, by = "patient_id") %>%
  select(-patient_id) |>
  select(ID=pseudo_id , age, gender, visit_date, 
         payer,visiting_clinic, visiting_dept) |> 
  filter(!visiting_dept %in% c("Accident & Emergency", "Direct investigations",
                               "HR/ADMIN/Research Activity","IPD" ))

write_csv(lookup_visit, "visit_reidentification_key.csv")

write_csv(opd_visit_pseudo,"Database_Registers/Data_Warehouse/OPD_Visits/2026-JAN-JUN_OPD-VISITS-TTH.csv")









