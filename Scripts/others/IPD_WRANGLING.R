## TTH Surgery SubBMC Monthly Reports ----
#  Data Wrangling ---- IPD 
#  Data Cleaning to ensure appropriate variables are created for analysis

#  Author ----
#  Asiwome

library(readxl)
library(tidyverse)
library(janitor)
library(stringr)
#library(icd)
library(lubridate)
library(haven)
library(here)
library(stringr)
library(openxlsx)
library(tidyr)


## Importing IPD data sets , JAN-JUN
ipd_jan_jun <- readxl::read_xlsx("DataFiles/Database_Registers/IPD_register/InpatientMorbidityRegisterJANJUN.xlsx") |> clean_names()
ipd_jul_sep <- read_csv("DataFiles/Database_Registers/IPD_register/InpatientMorbidityRegister_01 Jul 2026 00_00_00_to_30 Sep 2026 23_59_59 (1).csv") |> clean_names()
ipd0_jul_sep <- ipd_jul_sep |> 
  mutate(date_and_time_of_admission = parse_date_time(
    date_and_time_of_admission, 
    orders = c("dmy HM", "dmy HMS", "dmy", "mdy HM", "mdy HMS", "mdy", "ymd"), 
    tz = "UTC"
  ),
  date_and_time_of_discharge = parse_date_time(
    date_and_time_of_discharge, 
    orders = c("dmy HM", "dmy HMS", "dmy", "mdy HM", "mdy HMS", "mdy", "ymd"), 
    tz = "UTC"
  )
  )

ipd_df0_jan_sep_ <- bind_rows(ipd_jan_jun, ipd0_jul_sep)
icd109   <- readxl::read_xlsx("../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/2019-icd10-cm.xlsx") |> clean_names() |> 
  mutate(icd10_code= code)

ipd_df1 <- ipd_df0_jan_sep_ |> mutate(
  admission_date = as_date(date_and_time_of_admission),
  admission_date_adj = pmax(admission_date, as.Date("2026-01-01")),
  discharge_date = as_date(date_and_time_of_discharge),
  ward = stringr::str_squish(str_to_upper(ward))
)


# Create the lookup tibble departments/SUBBMC
lookup_dept_ward <- tribble(
  ~old, ~New,
  
  # Accident & Emergency
  "A&E CDU", "Accident & Emergency",
  "A&E ORANGE", "Accident & Emergency",
  "A&E RED", "Accident & Emergency",
  "A&E VIP", "Accident & Emergency",
  "A&E YELLOW", "Accident & Emergency",
  "EMERGENCY TRIAGE DETENTION WARD", "Accident & Emergency",
  
  #Corporate Clinic
  "CORPORATE CLINIC DETENTION", "Corporate Clinic" ,
  
  # DEENT
  "DEENT WARD","DEENT",
  
  # Poly Clinc
  "FAMILY MEDICINE FEMALE WARD" , "Poly Clinic",
  "FAMILY MEDICINE MALE WARD" , "Poly Clinic",
  "POLYCLINIC DETENTION WARD" , "Poly Clinic",
  
  # Surgery
  "GENERAL SURGERY WARD", "Surgery",
  "MAIN THEATRE", "Surgery",
  "MAIN THEATRE RECOVERY", "Surgery",
  "NEUROSURGERY WARD", "Surgery",
  "PLASTIC / THORACIC WARD", "Surgery",
  "UROLOGY THEATRE RECOVERY", "Surgery",
  "UROLOGY WARD", "Surgery",
  "INTENSIVE CARE UNIT (ICU) WARD","Surgery",
  "GENERAL SURGERY WARD","Surgery",
 
   # Obstetrics & Gynaecology
  "GYNAE THEATRE", "Obstetrics & Gynaecology",
  "GYNAECOLOGY A WARD", "Obstetrics & Gynaecology",
  "GYNAECOLOGY B WARD", "Obstetrics & Gynaecology",
  "GYNAECOLOGY THEATRE RECOVERY", "Obstetrics & Gynaecology",
  "LABOUR WARD",  "Obstetrics & Gynaecology",
  "MATERNITY EAST WARD (PRENATAL)", "Obstetrics & Gynaecology",
  "MATERNITY INTENSIVE CARE UNIT (MICU)", "Obstetrics & Gynaecology",
  "MATERNITY WEST WARD (POSTNATAL)",  "Obstetrics & Gynaecology",
  "O&G EMERGENCY","Obstetrics & Gynaecology",
  "OBSTETRICS THEATRE","Obstetrics & Gynaecology",
  "OBSTETRICS THEATRE RECOVERY","Obstetrics & Gynaecology",
 
   # Internal Medicine
  "MEDICAL 1 WARD", "Internal Medicine",
  "MEDICAL 2 WARD", "Internal Medicine",
  "MEDICAL 3 WARD", "Internal Medicine",
  "STROKE WARD MALE", "Internal Medicine",
  "INFECTIOUS DISEASE CENTRE", "Internal Medicine",
  
  # Paediatrics & Child Health
  "PAEDIATRIC WEST WARD","Paediatrics & Child Health",
  "PAEDIATRIC UNDER 6 WARD","Paediatrics & Child Health",
  "PAEDIATRIC ONCOLOGY UNIT","Paediatrics & Child Health",
  "PAEDIATRIC MALNUTRITION WARD","Paediatrics & Child Health",
  "PAEDIATRIC EMERGENCY WARD","Paediatrics & Child Health",
  "PAEDIATRIC EAST WARD","Paediatrics & Child Health",
  "PAEDIATRIC CRITICAL CARE WARD","Paediatrics & Child Health",
  "PAEDIATRIC BABIES UNIT","Paediatrics & Child Health",
  "NICU","Paediatrics & Child Health",
  "PAEDIATRIC CRITICAL CARE WARD","Paediatrics & Child Health",
  
  # Trauma & Orthopaedics
  "TRAUMA AND ORTHO","Trauma & Orthopaedics",
  "PAEDIATRIC ORTHOPAEDICS WARD", "Trauma & Orthopaedics",
  "ORTHOPAEDICS THEATRE", "Trauma & Orthopaedics"
)



# patient_days <- ipd1 |> 
#   mutate(
#     ward_dept = recode(ward, !!!deframe(lookup_dept_ward))
#   ) |> 
#   filter(!ward %in% c(
#     "EMERGENCY TRIAGE DETENTION WARD","A&E VIP", "PAEDIATRIC BABIES UNIT",
#     "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
#     "INFECTIOUS DISEASE CENTRE",
#                       "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
#                       "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE", 
#                       "O&G EMERGENCY","LABOUR WARD", "A&E CDU", "CORPORATE CLINIC DETENTION")) |> 
#   rowwise() |> 
#   mutate(date = list(seq(admission_date_adj,discharge_date, by="day"))) |> 
#   unnest(date) |> 
#   ungroup() |> 
#   mutate(month = month(date,label = T))

# day60 <- ipd1 |>
#   mutate(los = as.numeric(discharge_date - admission_date_adj) + 1) |> 
#   filter(los >60)
# 
# wardsum <- day90 |> group_by(ward) |> 
#   summarise(
#     mean_los = mean(los),
#     median_los = median(los),
#     max_los = max(los),
#     min_los = min(los)
#   )

# patient_days2 <- ipd_df1 |>
#   mutate(
#     ward_dept = recode(ward, !!!deframe(lookup_dept_ward)),
#     discharge_date_cap = pmin(
#       discharge_date,
#       admission_date_adj + 60
#     )
#   ) |>
#   filter(!ward %in% c(
#     "EMERGENCY TRIAGE DETENTION WARD","A&E VIP", "PAEDIATRIC BABIES UNIT",
#     "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
#     "INFECTIOUS DISEASE CENTRE",
#     "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
#     "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE",
#     "O&G EMERGENCY","LABOUR WARD", "A&E CDU", "CORPORATE CLINIC DETENTION"
#   )) |>
#   rowwise() |>
#   mutate(
#     date = list(seq(admission_date_adj, discharge_date_cap, by = "day"))
#   ) |>
#   unnest(date) |>
#   ungroup() |>
#   mutate(month = month(date, label = TRUE))


patient_days2 <- ipd_df1 |>
  mutate(
    # 🔍 Ensure proper Date types first to prevent pmin() from turning values into NA
    admission_date_adj = as.Date(admission_date_adj),
    discharge_date = as.Date(discharge_date),
    
    ward_dept = recode(ward, !!!deframe(lookup_dept_ward)),
    discharge_date_cap = pmin(
      discharge_date,
      admission_date_adj + 60
    )
  ) |>
  filter(!ward %in% c(
    "EMERGENCY TRIAGE DETENTION WARD","A&E VIP", "PAEDIATRIC BABIES UNIT",
    "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
    "INFECTIOUS DISEASE CENTRE",
    "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
    "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE",
    "O&G EMERGENCY","LABOUR WARD", "A&E CDU", "CORPORATE CLINIC DETENTION"
  )) |>
  filter(!is.na(admission_date_adj) & !is.na(discharge_date_cap)) |>
  rowwise() |>
  mutate(
    # seq() works perfectly even when admission and discharge are the same day (returns 1 date)
    date = list(seq(admission_date_adj, discharge_date_cap, by = "day"))
  ) |>
  unnest(date) |>
  ungroup() |>
  mutate(month = month(date, label = TRUE))










# TTH SUB

tth_patientday_q1 <- patient_days2 |> filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_patientday_q2 <- patient_days2 |> filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Apr,   May ,  Jun, Q2=Total)

tth_patientday_h1 <- patient_days2 |> filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  H1 =Total)

tth_patientday_q3 <- patient_days2 |> filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Jul,   Aug ,  Sep, Q3=Total)


tth_patient_days <- inner_join(tth_patientday_q1, tth_patientday_q2, by="Department") |>
  inner_join(tth_patientday_h1, by="Department") |> 
  inner_join(tth_patientday_q3, by="Department")

# SURGERY SUB
surgery_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

surgery_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Surgery")  |>
  filter(month %in% c("Apr","May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)

surgery_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Surgery")  |>
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)

surgery_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Surgery")  |>
  filter(month %in% c("Jul","Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)

surgery_patientdays <- inner_join(surgery_patientday_q1, surgery_patientday_q2, by="Ward") |> 
  inner_join(surgery_patientday_h1, by="Ward") |> 
  inner_join(surgery_patientday_q3, by="Ward")

# O&G SUB
OG_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

OG_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)

OG_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Obstetrics & Gynaecology")  |>
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)


OG_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Obstetrics & Gynaecology")  |>
  filter(month %in% c("Jul","Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)


OG_patientdays <- inner_join(OG_patientday_q1, OG_patientday_q2, by="Ward") |> 
  inner_join(OG_patientday_h1, by="Ward") |> 
  inner_join(OG_patientday_q3, by="Ward")

# DEENT SUB
DEENT_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

DEENT_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)

DEENT_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)

DEENT_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)

DEENT_patientdays <- inner_join(DEENT_patientday_q1, DEENT_patientday_q2, by="Ward") |> 
  inner_join(DEENT_patientday_h1, by="Ward") |> 
  inner_join(DEENT_patientday_q3, by="Ward")  


# Internal Medicine SUB
IM_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

IM_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)


IM_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)


IM_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)


IM_patientdays <- inner_join(IM_patientday_q1, IM_patientday_q2, by="Ward") |>
  inner_join(IM_patientday_h1, by="Ward") |> 
  inner_join(IM_patientday_q3, by="Ward")



# Poly Clinic SUB
Poly_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

Poly_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)


Poly_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)

Poly_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)


Poly_patientdays <- inner_join(Poly_patientday_q1, Poly_patientday_q2, by="Ward") |> 
  inner_join(Poly_patientday_h1, by="Ward") |> 
  inner_join(Poly_patientday_q3, by="Ward")

# Trauma & Orthopaedics SUB

ORTH_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

ORTH_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)


ORTH_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)


ORTH_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)


ORTH_patientdays <- inner_join(ORTH_patientday_q1, ORTH_patientday_q2, by="Ward") |> 
  inner_join(ORTH_patientday_h1, by="Ward") |> 
  inner_join(ORTH_patientday_q3, by="Ward")


# # Corporate Clinic SUB
# COP_patientday <- patient_days |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
#   select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)



# A&E SUB

AE_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Accident & Emergency") |>
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

AE_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Accident & Emergency") |>
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)

AE_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Accident & Emergency") |>
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)

AE_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Accident & Emergency") |>
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)

AE_patientdays <- inner_join(AE_patientday_q1, AE_patientday_q2, by="Ward") |>
  inner_join(AE_patientday_h1, by="Ward") |> 
  inner_join(AE_patientday_q3, by="Ward")



# Paediatrics & Child Health Sub

PAED_patientday_q1 <- patient_days2 |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

PAED_patientday_q2 <- patient_days2 |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Apr,   May ,  Jun, Q2=Total)


PAED_patientday_h1 <- patient_days2 |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward,  H1=Total)

PAED_patientday_q3 <- patient_days2 |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jul,   Aug ,  Sep, Q3=Total)

PAED_patientdays <- inner_join(PAED_patientday_q1, PAED_patientday_q2, by="Ward") |> 
  inner_join(PAED_patientday_h1, by="Ward") |> 
  inner_join(PAED_patientday_q3, by="Ward") 

####### Writing to excel sheets (Patient Days WorkBook)
patdays <- createWorkbook()
addWorksheet(patdays, "TTH")
addWorksheet(patdays, "Accident & Emergency")
addWorksheet(patdays, "DEENT")
addWorksheet(patdays, "Internal Medicine")
addWorksheet(patdays, "Obstetrics & Gynaecology")
addWorksheet(patdays, "Paediatrics & Child Health")
addWorksheet(patdays, "Poly Clinic")
addWorksheet(patdays, "Surgery")
addWorksheet(patdays, "Trauma & Orthopaedics")

writeData(patdays, "TTH", tth_patient_days)
writeData(patdays, "Accident & Emergency", AE_patientdays)
writeData(patdays, "DEENT", DEENT_patientdays)
writeData(patdays, "Internal Medicine", IM_patientdays)
writeData(patdays, "Obstetrics & Gynaecology", OG_patientdays)
writeData(patdays, "Paediatrics & Child Health", PAED_patientdays)
writeData(patdays, "Poly Clinic", Poly_patientdays)
writeData(patdays, "Surgery", surgery_patientdays)
writeData(patdays, "Trauma & Orthopaedics", ORTH_patientdays)

saveWorkbook(patdays, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-SEP_TTH-PATIENTS-DAYS_V03.xlsx",overwrite = TRUE)
#saveWorkbook(patdays, "DataFiles/Database_Registers/Data_Warehouse//2026-JAN-SEP_TTH-PATIENTS-DAYS_V03.xlsx",overwrite = TRUE)
 
#path = "Database_Registers/Admission_Resgister/",

###############
# ADMISSIONS
################

# Get the file paths
admission_file_paths <- list.files(
  path = "DataFiles/Database_Registers/Admission_Resgister/",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

admission_visits_jan_jun <- admission_file_paths |>
  map_dfr(function(file_path) {
    # Read just the "ALL" sheet from the current file
    read_xlsx(path = file_path) |>
      clean_names() |>
      # Keeps track of which file the data came from
      mutate(source_file = basename(file_path))
  }) |> select(-source_file, -no)
writexl::write_xlsx(admission_visits_jan_jun, "DataFiles/Database_Registers/Admission_Resgister/2026-JAN-JUN_ADMISSION-RESGISTER..xlsx")

admission_visits_jul_sep <- read_csv("DataFiles/Database_Registers/Admission_Resgister/AdmissionRegister_01 Jul 2026 00_00_00_to_30 Sep 2026 23_59_59.csv") |>clean_names() 

admission_visits_jan_sep <- bind_rows(admission_visits_jan_jun,admission_visits_jul_sep)

admission_visits_df <-  admission_visits_jan_sep |>
  mutate(
    admission_date_clean = admission_date |>
      str_remove_all('"') |>
      str_squish() |>                     # collapses/strips tabs, extra spaces, etc.
      parse_date_time(
        orders = c("dmy HM", "dmy HMS", "dmy", "mdy HM", "mdy HMS", "mdy", "ymd"), 
    tz = "UTC"
      ),
    month=month(admission_date_clean, label = T),
    ward = stringr::str_squish(str_to_upper(ward_room)),
    ward_dept = recode(ward, !!!deframe(lookup_dept_ward))
  ) |> 
  filter(!ward %in% c(
                      "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
                      "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
                      "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE", 
                      "O&G EMERGENCY","LABOUR WARD", "CORPORATE CLINIC DETENTION","OBS AND GYNE WARD")) |> 
  group_by(visit_no, ward_dept) |> 
  slice_min(order_by = admission_date_clean, n = 1, with_ties = FALSE) |> 
  ungroup()


# TTH SUB

tth_admission_q1 <- admission_visits_df |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_admission_q2 <- admission_visits_df  |> filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Apr,   May ,  Jun, Q2=Total)

tth_admission_h1 <- admission_visits_df |> filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  H1 =Total)

tth_admission_q3 <- admission_visits_df  |> filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Jul,   Aug ,  Sep, Q3=Total)

tth_admission <- inner_join(tth_admission_q1, tth_admission_q2, by="Department") |> 
  inner_join(tth_admission_h1, by="Department") |> 
  inner_join(tth_admission_q3, by="Department")


# SURGERY SUB
surgery_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

surgery_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

surgery_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)


surgery_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

surgery_admission <- inner_join(surgery_admission_q1, surgery_admission_q2, by="Ward") |> 
  inner_join(surgery_admission_h1, by="Ward") |> 
  inner_join(surgery_admission_q3, by="Ward")


# O&G SUB

OG_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

OG_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

OG_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

OG_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)



OG_admission <- inner_join(OG_admission_q1, OG_admission_q2, by="Ward") |> 
  inner_join(OG_admission_h1, by="Ward") |> 
  inner_join(OG_admission_q3, by="Ward")
  


# DEENT SUB

DEENT_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

DEENT_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

DEENT_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

DEENT_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)



DEENT_admission <- inner_join(DEENT_admission_q1, DEENT_admission_q2, by="Ward") |> 
  inner_join(DEENT_admission_h1, by="Ward") |> 
  inner_join(DEENT_admission_q3, by="Ward")



# Internal Medicine SUB

IM_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

IM_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

IM_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

IM_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)



IM_admission <- inner_join(IM_admission_q1, IM_admission_q2, by="Ward") |>
  inner_join(IM_admission_h1, by="Ward") |> 
  inner_join(IM_admission_q3, by="Ward")


# Poly Clinic SUB

Poly_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

Poly_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

Poly_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

Poly_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)



Poly_admission <- inner_join(Poly_admission_q1, Poly_admission_q2, by="Ward") |> 
  inner_join(Poly_admission_h1, by="Ward") |> 
  inner_join(Poly_admission_q3, by="Ward") 


# Trauma & Orthopaedics SUB

ORTH_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

ORTH_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

ORTH_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

ORTH_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)


ORTH_admission <- inner_join(ORTH_admission_q1, ORTH_admission_q2, by="Ward") |> 
  inner_join(ORTH_admission_h1, by="Ward") |> 
  inner_join(ORTH_admission_q3, by="Ward")
  




# Corporate Clinic SUB
# tth_COP_ad <- admission_visits_df |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col"))

# A&E SUB

AE_admission_q1 <- admission_visits_df |>  
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

AE_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

AE_admission_q3 <- admission_visits_df |>  
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jul ,  Aug ,  Sep,Q3=Total)

AE_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

AE_admission <- inner_join(AE_admission_q1, AE_admission_q2, by="Ward") |> 
  inner_join(AE_admission_h1, by="Ward") |> 
  inner_join(AE_admission_q3, by="Ward")


# Paediatrics & Child Health Sub

PAED_admission_q1 <- admission_visits_df |>  
 filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

PAED_admission_q2 <- admission_visits_df  |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

PAED_admission_q3 <- admission_visits_df  |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

PAED_admission_h1 <- admission_visits_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

PAED_admission <- inner_join(PAED_admission_q1, PAED_admission_q2, by="Ward") |>
  inner_join(PAED_admission_h1, by="Ward") |> 
  inner_join(PAED_admission_q3, by="Ward")


##################
## TRANS-IN
##################

trans_in_jan_jun <- readxl::read_xlsx("DataFiles/Database_Registers/TransIN register/TransInRegister_01Jan2026_to_30Jun2026.xlsx") |> clean_names()
trans_in_jul_sep <- read_csv("DataFiles/Database_Registers/TransIN register/TransInRegister_01 Jul 2026 00_00_00_to_30 Sep 2026 23_59_59.csv") |> clean_names()
trans0_in_jul_sep <- trans_in_jul_sep |> 
  mutate(admission_date = parse_date_time(
    admission_date, 
    orders = c("dmy HM", "dmy HMS", "dmy", "mdy HM", "mdy HMS", "mdy", "ymd"), 
    tz = "UTC"
  ))

trans_df0_jan_sep <- bind_rows(trans_in_jan_jun,trans0_in_jul_sep)

transin_df <- trans_df0_jan_sep |> mutate(
  admis_date = as_date(admission_date),
  month=month(admis_date, label = T),
  ward = stringr::str_squish(str_to_upper(to_ward)),
  ward_dept = recode(ward, !!!deframe(lookup_dept_ward))
) |> 
  filter(!ward %in% c(
    "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
    "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
    "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE",
    "O&G EMERGENCY","LABOUR WARD", 
    "CORPORATE CLINIC DETENTION","OBS AND GYNE WARD", "EMERGENCY TRIAGE DETENTION WARD",
    "A&E CDU","A&E ORANGE","A&E RED", "A&E VIP", "A&E YELLOW")) |> 
  group_by(visit_no, ward_dept) |> 
  slice_max(order_by = admis_date, n = 1, with_ties = FALSE) |> 
  ungroup()

# 
# 
# TTH SUB

tth_transin_q1 <- transin_df |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_transin_q2 <- transin_df  |> filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Apr,   May ,  Jun, Q2=Total)

tth_transin_h1 <- transin_df |> filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  H1 =Total)


tth_transin_q3 <- transin_df  |> filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Jul,   Aug ,  Sep, Q3=Total)

tth_transin <- inner_join(tth_transin_q1, tth_transin_q2, by="Department") |> 
  inner_join(tth_transin_h1, by="Department") |> 
  inner_join(tth_transin_q3, by="Department") 


# SURGERY SUB
surgery_transin_q1 <- transin_df |> 
   filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

surgery_transin_q2 <- transin_df  |> 
   filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

surgery_transin_q3 <- transin_df  |> 
   filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

surgery_transin_h1 <- transin_df |> 
   filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

surgery_transin <- inner_join(surgery_transin_q1, surgery_transin_q2, by="Ward") |> 
  inner_join(surgery_transin_h1, by="Ward") |> 
  inner_join(surgery_transin_q3, by="Ward")


# O&G SUB
OG_transin_q1 <- transin_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

OG_transin_q2 <- transin_df  |> 
   filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)


OG_transin_q3 <- transin_df  |> 
   filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

OG_transin_h1 <- transin_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

OG_transin <- inner_join(OG_transin_q1, OG_transin_q2, by="Ward") |> 
  inner_join(OG_transin_h1, by="Ward") |> 
  inner_join(OG_transin_q3, by="Ward")


# DEENT SUB
DEENT_transin_q1 <- transin_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

DEENT_transin_q2 <- transin_df  |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

DEENT_transin_q3 <- transin_df  |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)




DEENT_transin_h1 <- transin_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

DEENT_transin <- inner_join(DEENT_transin_q1, DEENT_transin_q2, by="Ward") |> 
  inner_join(DEENT_transin_h1, by="Ward") |> 
  inner_join(DEENT_transin_q3, by="Ward") 
  

# Internal Medicine SUB
IM_transin_q1 <- transin_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

IM_transin_q2 <- transin_df  |> 
 filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

IM_transin_q3 <- transin_df  |> 
 filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)


IM_transin_h1 <- transin_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

IM_transin <- inner_join(IM_transin_q1, IM_transin_q2, by="Ward") |> 
  inner_join(IM_transin_h1, by="Ward") |> 
  inner_join(IM_transin_q3, by="Ward") 


# Poly Clinic SUB

Poly_transin_q1 <- transin_df |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

Poly_transin_q2 <- transin_df  |> 
 filter(ward_dept=="Poly Clinic") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

Poly_transin_q3 <- transin_df  |> 
 filter(ward_dept=="Poly Clinic") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

Poly_transin_h1 <- transin_df |> 
  filter(ward_dept=="Poly Clinic") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)


poly_h1 <- Poly_transin_q1 %>%
  full_join(Poly_transin_q2, by = "Ward") %>%
  full_join(Poly_transin_h1, by = "Ward") %>%
  full_join(Poly_transin_q3, by = "Ward") %>%
  mutate(across(-Ward, ~replace_na(.x, 0)))

# Move Total row to bottom
poly_h1 <- poly_h1 %>%
  arrange(Ward == "Total")


# Trauma & Orthopaedics SUB

ORTH_transin_q1 <- transin_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |>
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

ORTH_transin_q2 <- transin_df  |> 
 filter(ward_dept=="Trauma & Orthopaedics") |>
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

ORTH_transin_q3 <- transin_df  |> 
 filter(ward_dept=="Trauma & Orthopaedics") |>
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

ORTH_transin_h1 <- transin_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |>
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

ORTH_transin <- inner_join(ORTH_transin_q1, ORTH_transin_q2, by="Ward") |>
  inner_join(ORTH_transin_h1, by="Ward") |> 
  inner_join(ORTH_transin_q3, by="Ward") 


# # Corporate Clinic SUB
# tth_COP_tranin <- discharges_df |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col"))

# # A&E SUB
# tth_AE_tranin <- transin_df |> 
#   filter(ward_dept=="Accident & Emergency") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col"))


# Paediatrics & Child Health Sub
PAED_transin_q1 <- transin_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

PAED_transin_q2 <- transin_df  |> 
 filter(ward_dept=="Paediatrics & Child Health") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

PAED_transin_q3 <- transin_df  |> 
 filter(ward_dept=="Paediatrics & Child Health") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)




PAED_transin_h1 <- transin_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

PAED_transin <- inner_join(PAED_transin_q1, PAED_transin_q2, by="Ward") |> 
  inner_join(PAED_transin_h1, by="Ward") |> 
  inner_join(PAED_transin_q3, by="Ward")


####### Writing to excel sheets (Patient Days WorkBook)
transIN <- createWorkbook()
addWorksheet(transIN, "TTH")
#addWorksheet(transIN, "Accident & Emergency")
addWorksheet(transIN, "DEENT")
addWorksheet(transIN, "Internal Medicine")
addWorksheet(transIN, "Obstetrics & Gynaecology")
addWorksheet(transIN, "Paediatrics & Child Health")
addWorksheet(transIN, "Poly Clinic")
addWorksheet(transIN, "Surgery")
addWorksheet(transIN, "Trauma & Orthopaedics")

writeData(transIN, "TTH", tth_transin)
#writeData(transIN, "Accident & Emergency", AE_patientday)
writeData(transIN, "DEENT", DEENT_transin)
writeData(transIN, "Internal Medicine", IM_transin)
writeData(transIN, "Obstetrics & Gynaecology", OG_transin)
writeData(transIN, "Paediatrics & Child Health", PAED_transin)
writeData(transIN, "Poly Clinic", poly_h1)
writeData(transIN, "Surgery", surgery_transin)
writeData(transIN, "Trauma & Orthopaedics", ORTH_transin)

saveWorkbook(transIN, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-SEP_TTH-TRANS-INS_V03.xlsx", overwrite = TRUE)
  


##################################################
############### ADMISSION + TRANS-IN
##################################################
admission_visits_df2 <-  admission_visits_df  |> select(ward, month, ward_dept)
transin_df2 <-  transin_df  |> select(ward, month, ward_dept)

admission_tranin_df <- bind_rows(
  transin_df2 ,admission_visits_df2
)
 
# TTH SUB
tth_adm_in_q1 <- admission_tranin_df |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_adm_in_q2 <- admission_tranin_df  |> filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Apr,   May ,  Jun, Q2=Total)

tth_adm_in_h1 <- admission_tranin_df |> filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  H1 =Total)

tth_adm_in_q3 <- admission_tranin_df  |> filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Jul,   Aug ,  Sep, Q3=Total)


tth_adm_in <- inner_join(tth_adm_in_q1, tth_adm_in_q2, by="Department") |> 
  inner_join(tth_adm_in_h1, by="Department") |> 
  inner_join(tth_adm_in_q3, by="Department")


# SURGERY SUB
surgery_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

surgery_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Surgery") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

surgery_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Surgery") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

surgery_adm_in_h1 <- admission_tranin_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

surgery_adm_in <- inner_join(surgery_adm_in_q1, surgery_adm_in_q2, by="Ward") |>
  inner_join(surgery_adm_in_h1, by="Ward") |> 
  inner_join(surgery_adm_in_q3, by="Ward")


# O&G SUB
OG_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

OG_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Obstetrics & Gynaecology") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

OG_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Obstetrics & Gynaecology") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

OG_adm_in_h1 <- admission_tranin_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

OG_adm_in <- inner_join(OG_adm_in_q1, OG_adm_in_q2, by="Ward") |> 
  inner_join(OG_adm_in_h1, by="Ward") |> 
  inner_join(OG_adm_in_q3, by="Ward") 


# DEENT SUB
tth_DEENT_adm_in <- admission_tranin_df |> 
  filter(ward_dept=="DEENT") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

DEENT_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

DEENT_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="DEENT") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

DEENT_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="DEENT") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

DEENT_adm_in_h1 <- admission_tranin_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

DEENT_adm_in <- inner_join(DEENT_adm_in_q1, DEENT_adm_in_q2, by="Ward") |> 
  inner_join(DEENT_adm_in_h1, by="Ward") |> 
  inner_join(DEENT_adm_in_q3, by="Ward")

# Internal Medicine SUB
IM_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

IM_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Internal Medicine") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

IM_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Internal Medicine") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)


IM_adm_in_h1 <- admission_tranin_df |> 
 filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

IM_adm_in <- inner_join(IM_adm_in_q1, IM_adm_in_q2, by="Ward") |> 
  inner_join(IM_adm_in_h1, by="Ward") |> 
  inner_join(IM_adm_in_q3, by="Ward")


# Poly Clinic SUB

Poly_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Poly Clinic") |>
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

Poly_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

Poly_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

Poly_adm_in_h1 <- admission_tranin_df |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

Poly_adm_in <- inner_join(Poly_adm_in_q1, Poly_adm_in_q2, by="Ward") |>
  inner_join(Poly_adm_in_h1, by="Ward") |> 
  inner_join(Poly_adm_in_q3, by="Ward")


# Trauma & Orthopaedics SUB


ORTH_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

ORTH_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

ORTH_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

ORTH_adm_in_h1 <- admission_tranin_df |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

ORTH_adm_in <- inner_join(ORTH_adm_in_q1, ORTH_adm_in_q2, by="Ward") |> 
  inner_join(ORTH_adm_in_h1, by="Ward") |> 
  inner_join(ORTH_adm_in_q3, by="Ward")

# # Corporate Clinic SUB
# tth_COP_tranin <- discharges_df |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col"))

# A&E SUB
AE_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

AE_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Accident & Emergency") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)


AE_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Accident & Emergency") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)

AE_adm_in_h1 <- admission_tranin_df |> 
filter(ward_dept=="Accident & Emergency") |> 
filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

AE_adm_in <- inner_join(AE_adm_in_q1, AE_adm_in_q2, by="Ward") |> 
  inner_join(AE_adm_in_h1, by="Ward") |> 
  inner_join(AE_adm_in_q3, by="Ward")


# Paediatrics & Child Health Sub

PAED_adm_in_q1 <- admission_tranin_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

PAED_adm_in_q2 <- admission_tranin_df  |> 
 filter(ward_dept=="Paediatrics & Child Health") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)


PAED_adm_in_q3 <- admission_tranin_df  |> 
 filter(ward_dept=="Paediatrics & Child Health") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)


PAED_adm_in_h1 <- admission_tranin_df |> 
filter(ward_dept=="Paediatrics & Child Health") |> 
filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

PAED_adm_in <- inner_join(PAED_adm_in_q1, PAED_adm_in_q2, by="Ward") |>
  inner_join(PAED_adm_in_h1, by="Ward") |> 
  inner_join(PAED_adm_in_q3, by="Ward")

####### Writing to excel sheets (Admissions WorkBook)
admissions <- createWorkbook()
addWorksheet(admissions, "TTH")
addWorksheet(admissions, "Accident & Emergency")
addWorksheet(admissions, "DEENT")
addWorksheet(admissions, "Internal Medicine")
addWorksheet(admissions, "Obstetrics & Gynaecology")
addWorksheet(admissions, "Paediatrics & Child Health")
addWorksheet(admissions, "Poly Clinic")
addWorksheet(admissions, "Surgery")
addWorksheet(admissions, "Trauma & Orthopaedics")

writeData(admissions, "TTH", tth_adm_in)
writeData(admissions, "Accident & Emergency", AE_adm_in)
writeData(admissions, "DEENT", DEENT_adm_in)
writeData(admissions, "Internal Medicine", IM_adm_in)
writeData(admissions, "Obstetrics & Gynaecology", OG_adm_in)
writeData(admissions, "Paediatrics & Child Health", PAED_adm_in)
writeData(admissions, "Poly Clinic", Poly_adm_in)
writeData(admissions, "Surgery", surgery_adm_in)
writeData(admissions, "Trauma & Orthopaedics", ORTH_adm_in)

saveWorkbook(admissions, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-SEP_TTH-ADMISSIONS_V03.xlsx", overwrite = TRUE) 



#########################
## DISCHARGES
#########################
dis_jan_jun <- read_csv("DataFiles/Database_Registers/Discharge Register/DischargeRegister_01Jan 202630Jun 2026.csv") |> 
  clean_names()
dis_jul_sep <- read_csv("DataFiles/Database_Registers/Discharge Register/DischargeRegister_01 Jul 2026 00_00_00_to_30 Sep 2026 23_59_59 (1).csv") |> 
  clean_names()

dis0_jan_sep <- bind_rows(dis_jan_jun, dis_jul_sep)


discharges_df <- dis0_jan_sep |> 
  mutate(discharge_date_date = parse_date_time(
    discharge_date_date, 
    orders = c("dmy HM", "dmy HMS", "dmy", "mdy HM", "mdy HMS", "mdy", "ymd"), 
    tz = "UTC"
  ),
  month=month(discharge_date_date, label = T),
  ward = stringr::str_squish(str_to_upper(ward_room)),
  ward_dept = recode(ward, !!!deframe(lookup_dept_ward)))  |> 
  filter(!ward %in% c("EMERGENCY TRIAGE DETENTION WARD","Corporate Clinic",
                      "CORPORATE CLINIC DETENTION",
                      "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
                      "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
                      "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE"))

# TTH SUB
tth_disc_q1 <- discharges_df |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_disc_q2 <- discharges_df  |> filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Apr,   May ,  Jun, Q2=Total)

tth_disc_h1 <- discharges_df |> filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  H1 =Total)

tth_disc_q3 <- discharges_df  |> filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Jul,   Aug ,  Sep, Q3=Total)

tth_disc <- inner_join(tth_disc_q1, tth_disc_q2, by="Department") |> 
  inner_join(tth_disc_h1, by="Department") |> 
  inner_join(tth_disc_q3, by="Department")


# SURGERY SUB
surgery_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

surgery_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Surgery") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

surgery_disc_q3 <- discharges_df  |> 
 filter(ward_dept=="Surgery") |> 
 filter(month %in% c("Jul", "Aug", "Sep")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Jul,   Aug ,  Sep, Q3=Total)



surgery_disc_h1 <- discharges_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

surgery_disc <- inner_join(surgery_disc_q1, surgery_disc_q2, by="Ward") |> 
  inner_join(surgery_disc_h1, by="Ward") |> 
  inner_join(surgery_disc_q3, by="Ward")


# O&G SUB
OG_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

OG_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Obstetrics & Gynaecology") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

OG_disc_h1 <- discharges_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

OG_disc <- inner_join(OG_disc_q1, OG_disc_q2, by="Ward") |> inner_join(OG_disc_h1, by="Ward")


# DEENT SUB
tth_DEENT_disc <- discharges_df |> 
  filter(ward_dept=="DEENT") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

DEENT_disc_q1 <- discharges_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

DEENT_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="DEENT") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

DEENT_disc_h1 <- discharges_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

DEENT_disc <- inner_join(DEENT_disc_q1, DEENT_disc_q2, by="Ward") |> inner_join(DEENT_disc_h1, by="Ward")

# Internal Medicine SUB
IM_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

IM_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Internal Medicine") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

IM_disc_h1 <- discharges_df |> 
 filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

IM_disc <- inner_join(IM_disc_q1, IM_disc_q2, by="Ward") |> inner_join(IM_disc_h1, by="Ward")


# Poly Clinic SUB

Poly_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Poly Clinic") |>
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

Poly_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

Poly_disc_h1 <- discharges_df |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

Poly_disc <- inner_join(Poly_disc_q1, Poly_disc_q2, by="Ward") |> inner_join(Poly_disc_h1, by="Ward")


# Trauma & Orthopaedics SUB


ORTH_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

ORTH_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

ORTH_disc_h1 <- discharges_df |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

ORTH_disc <- inner_join(ORTH_disc_q1, ORTH_disc_q2, by="Ward") |> inner_join(ORTH_disc_h1, by="Ward")

# # Corporate Clinic SUB
# tth_COP_tranin <- discharges_df |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col"))

# A&E SUB
AE_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

AE_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Accident & Emergency") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

AE_disc_h1 <- discharges_df |> 
filter(ward_dept=="Accident & Emergency") |> 
filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

AE_disc <- inner_join(AE_disc_q1, AE_disc_q2, by="Ward") |> inner_join(AE_disc_h1, by="Ward")


# Paediatrics & Child Health Sub

PAED_disc_q1 <- discharges_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

PAED_disc_q2 <- discharges_df  |> 
 filter(ward_dept=="Paediatrics & Child Health") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

PAED_disc_h1 <- discharges_df |> 
filter(ward_dept=="Paediatrics & Child Health") |> 
filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

PAED_disc <- inner_join(PAED_disc_q1, PAED_disc_q2, by="Ward") |> inner_join(PAED_disc_h1, by="Ward")

####### Writing to excel sheets (Admissions WorkBook)
discharges <- createWorkbook()
addWorksheet(discharges, "TTH")
addWorksheet(discharges, "Accident & Emergency")
addWorksheet(discharges, "DEENT")
addWorksheet(discharges, "Internal Medicine")
addWorksheet(discharges, "Obstetrics & Gynaecology")
addWorksheet(discharges, "Paediatrics & Child Health")
addWorksheet(discharges, "Poly Clinic")
addWorksheet(discharges, "Surgery")
addWorksheet(discharges, "Trauma & Orthopaedics")

writeData(discharges, "TTH", tth_disc)
writeData(discharges, "Accident & Emergency", AE_disc)
writeData(discharges, "DEENT", DEENT_disc)
writeData(discharges, "Internal Medicine", IM_disc)
writeData(discharges, "Obstetrics & Gynaecology", OG_disc)
writeData(discharges, "Paediatrics & Child Health", PAED_disc)
writeData(discharges, "Poly Clinic", Poly_disc)
writeData(discharges, "Surgery", surgery_disc)
writeData(discharges, "Trauma & Orthopaedics", ORTH_disc)

saveWorkbook(discharges, "../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-JUN_TTH-DISCHARGES_V02.xlsx", overwrite = TRUE) 





# TRANS-OUT Register ####

tran_out_jan_jun <- readxl::read_xlsx("DataFiles/Database_Registers/TransOUT resgister/TransOutRegister_01Jan2026to_30 Jun 2026.xlsx") |> 
  clean_names()

tran_out_jul_sep <- readxl::read_xlsx("DataFiles/Database_Registers/TransOUT resgister/TransOutRegister_01Jan2026to_30 Jun 2026.xlsx") |> 
  clean_names()

tran_out_df_jan_sep <- bind_rows(tran_out_jan_jun, tran_out_jul_sep)


tout_df <- tran_out_df_jul_sep |> 
  mutate(
  month=month(admission_date, label = T),
  ward = stringr::str_squish(str_to_upper(from_ward)),
  ward_dept = recode(ward, !!!deframe(lookup_dept_ward))
) |> 
  filter(!ward %in% c(
    "MAIN THEATRE", "MAIN THEATRE RECOVERY", "UROLOGY THEATRE RECOVERY",
    "GYNAE THEATRE", "GYNAECOLOGY THEATRE RECOVERY","OBSTETRICS THEATRE",
    "OBSTETRICS THEATRE RECOVERY", "ORTHOPAEDICS THEATRE",
    "O&G EMERGENCY","LABOUR WARD", 
    "CORPORATE CLINIC DETENTION","OBS AND GYNE WARD"))|> 
  group_by(visit_no, ward_dept) |> 
  slice_min(order_by = admission_date, n = 1, with_ties = FALSE) |> 
  ungroup()


# TTH SUB
tth_transout_q1 <- tout_df |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_transout_q2 <- tout_df  |> filter(month %in% c("Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Apr,   May ,  Jun, Q2=Total)

tth_transout_h1 <- tout_df |> filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  H1 =Total)

tth_transout_q3 <- tout_df  |> filter(month %in% c("Jul", "Aug", "Sep")) |>
  tabyl(ward_dept, month) |> adorn_totals(where = c("row", "col")) |>
  select(Department=ward_dept,  Jul,   Aug ,  Sep, Q3=Total)


tth_transout <- inner_join(tth_transout_q1, tth_transout_q2, by="Department") |> inner_join(tth_transout_h1, by="Department")


# SURGERY SUB
surgery_transout_q1 <- tout_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

surgery_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Surgery") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

surgery_transout_h1 <- tout_df |> 
  filter(ward_dept=="Surgery") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

surgery_transout <- inner_join(surgery_transout_q1, surgery_transout_q2, by="Ward") |> inner_join(surgery_transout_h1, by="Ward")


# O&G SUB
OG_transout_q1 <- tout_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

OG_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Obstetrics & Gynaecology") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

OG_transout_h1 <- tout_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

OG_transout <- inner_join(OG_transout_q1, OG_transout_q2, by="Ward") |> inner_join(OG_transout_h1, by="Ward")


# DEENT SUB
tth_DEENT_transout <- tout_df |> 
  filter(ward_dept=="DEENT") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

DEENT_transout_q1 <- tout_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

DEENT_transout_q2 <- tout_df  |> 
 filter(ward_dept=="DEENT") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

DEENT_transout_h1 <- tout_df |> 
  filter(ward_dept=="DEENT") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

DEENT_transout <- inner_join(DEENT_transout_q1, DEENT_transout_q2, by="Ward") |> inner_join(DEENT_transout_h1, by="Ward")

# Internal Medicine SUB
IM_transout_q1 <- tout_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

IM_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Internal Medicine") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

IM_transout_h1 <- tout_df |> 
 filter(ward_dept=="Internal Medicine") |> 
  filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward,  H1 =Total)

IM_transout <- inner_join(IM_transout_q1, IM_transout_q2, by="Ward") |> inner_join(IM_transout_h1, by="Ward")


# Poly Clinic SUB

Poly_transout_q1 <- tout_df |> 
  filter(ward_dept=="Poly Clinic") |>
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

Poly_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

Poly_transout_h1 <- tout_df |> 
 filter(ward_dept=="Poly Clinic") |>
 filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

Poly_transout <- inner_join(Poly_transout_q1, Poly_transout_q2, by="Ward") |> inner_join(Poly_transout_h1, by="Ward")


# Trauma & Orthopaedics SUB


ORTH_transout_q1 <- tout_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

ORTH_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

ORTH_transout_h1 <- tout_df |> 
 filter(ward_dept=="Trauma & Orthopaedics") |> 
 filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

ORTH_transout <- inner_join(ORTH_transout_q1, ORTH_transout_q2, by="Ward") |> inner_join(ORTH_transout_h1, by="Ward")

# # Corporate Clinic SUB
# tth_COP_tranin <- tout_df |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col"))

# A&E SUB
AE_transout_q1 <- tout_df |> 
  filter(ward_dept=="Accident & Emergency") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

AE_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Accident & Emergency") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

AE_transout_h1 <- tout_df |> 
filter(ward_dept=="Accident & Emergency") |> 
filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

AE_transout <- inner_join(AE_transout_q1, AE_transout_q2, by="Ward") |> inner_join(AE_transout_h1, by="Ward")


# Paediatrics & Child Health Sub

PAED_transout_q1 <- tout_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  filter(month %in% c("Jan", "Feb", "Mar")) |>
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
  select(Ward=ward, Jan ,  Feb ,  Mar,Q1=Total)

PAED_transout_q2 <- tout_df  |> 
 filter(ward_dept=="Paediatrics & Child Health") |> 
 filter(month %in% c("Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  Apr,   May ,  Jun, Q2=Total)

PAED_transout_h1 <- tout_df |> 
filter(ward_dept=="Paediatrics & Child Health") |> 
filter(month %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
 tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |>
 select(Ward=ward,  H1 =Total)

PAED_transout <- inner_join(PAED_transout_q1, PAED_transout_q2, by="Ward") |> inner_join(PAED_transout_h1, by="Ward")

####### Writing to excel sheets (Admissions WorkBook)
transouts <- createWorkbook()
addWorksheet(transouts, "TTH")
addWorksheet(transouts, "Accident & Emergency")
addWorksheet(transouts, "DEENT")
addWorksheet(transouts, "Internal Medicine")
addWorksheet(transouts, "Obstetrics & Gynaecology")
addWorksheet(transouts, "Paediatrics & Child Health")
addWorksheet(transouts, "Poly Clinic")
addWorksheet(transouts, "Surgery")
addWorksheet(transouts, "Trauma & Orthopaedics")

writeData(transouts, "TTH", tth_transout)
writeData(transouts, "Accident & Emergency", AE_transout)
writeData(transouts, "DEENT", DEENT_transout)
writeData(transouts, "Internal Medicine", IM_transout)
writeData(transouts, "Obstetrics & Gynaecology", OG_transout)
writeData(transouts, "Paediatrics & Child Health", PAED_transout)
writeData(transouts, "Poly Clinic", Poly_transout)
writeData(transouts, "Surgery", surgery_transout)
writeData(transouts, "Trauma & Orthopaedics", ORTH_transout)

saveWorkbook(transouts, "../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-JUN_TTH-TRANS-OUT_V02.xlsx", overwrite = TRUE) 



################################
# IPD CONDITIONS
################################
ipddx <-  ipd_df0_jan_sep_ 
ipddx0 <- ipddx |> 
  pivot_longer(
    cols = contains("diagnosis"), # all diagnoses
    names_to = "diagnosis_type", 
    values_to = "diagnosis_val"
  )  |>                                   # Split the pipe-separated items dynamically into rows
  separate_longer_delim(
    cols = diagnosis_val, 
    delim = " | "
  ) |> 
  mutate(diagnosis_val = trimws(diagnosis_val)) |> 
  separate_wider_delim(
    cols = diagnosis_val,
    delim = " - ",
    names = c("dx_code", "dx_description"),
    too_few = "align_start",  # Handles codes without descriptions safely (assigns NA)
    too_many = "merge"        # Merges any extra hyphens into the description column
  ) |>  
  mutate(dx_code = trimws(dx_code)) |> 
  filter(dx_code != "N/A") |>   # Removes the text "N/A"
  drop_na(dx_code)

ipd_dx1 <- ipddx0 %>%
  mutate(
    code=str_sub(str_remove_all(dx_code, fixed(".")), 1, 7),
    icd10_code= str_sub(str_remove_all(dx_code, fixed(".")), 1, 4),
    ward_dept = recode(ward, !!!deframe(lookup_dept_ward))
  ) |> 
  mutate(
    diagnosis_type2 = case_when(
      diagnosis_type %in% c("principal_diagnosis_old_case", "principal_diagnosis_new_case")~"Principal",
      diagnosis_type %in% c("additional_diagnosis_old_case","additional_diagnosis_new_case")~"Additional",
      diagnosis_type %in% c("provisional_diagnosis")~"Provisional",
      diagnosis_type %in% c("referral_diagnosis")~"Referral"
    ),
    case_type = case_when(
      diagnosis_type %in% c("principal_diagnosis_new_case","additional_diagnosis_new_case")~"New",
      diagnosis_type %in% c("principal_diagnosis_old_case", "additional_diagnosis_old_case")~"Old"
    ),
    # Ids to detect duplicated same day dx code
    pid_dxcode = paste(patient_no,date_and_time_of_discharge,dx_code, sep = "_"),
  ) |> extract_gdrg_parts(dx_code) |> 
  filter(!ward %in% c("Day Procedures", "GYNAE THEATRE", "Gynaecology Theatre Recovery",
                            "Labour Ward","MAIN THEATRE","Main Theatre Recovery",
                           "OBSTETRICS THEATRE","ORTHOPAEDICS THEATRE","Obstetrics Theatre Recovery",
                           "Urology Theatre Recovery"
                           )) |> 
  distinct(pid_dxcode, .keep_all = T) |>  #duplicated same day dx code
  filter(has_gdrg=="FALSE")


# Pulling codes with GDR 

#opd_dx_gdrs <- filter(opd_dx1, has_gdrg=="TRUE")

# 
#################################################################################
ipd_condition_df <- ipd_dx1 |> 
  select(
    patient_no, age = age,dx_date=date_and_time_of_discharge, gender=sex, icd10_code=code, dx_description, 
    ward, department = ward_dept,
    old_new_case = case_type,diagnosis_type=diagnosis_type2
  ) |> 
  mutate(
icd10_code = trimws(icd10_code, which = "both"),
    icd10_code2 = recode(
      icd10_code,
      "I64"=	"I639", "I500" = "I509", "V5849" =	"Z4889", "A099" =	"A09", "Z2251"	= "Z228",
      "Z2252" =	"Z228", "B004.A0" =	"A010", "NI89" =	"N189", "O809" =	"O80", "R104"	= "R109",
      "K749"	= "K7460", "O06"	= "O03", "I845" =	"K649", "K056" =	"k053", "I986"	= "S026",
      "I849"	 = "I849", "T814XXA" =	"T8140", "B004-A1" =	"A178", "I859" =	"I85","B004-A0" =	"A178",
      "E141" =	"E131","N3590"	= "N359","X791"	= "X79","A090"	= "A09","H4011X3"	= "H4010X3",
      "O822"	= "Z9071", "NI82" =	"N182", "M841"	= "M8430XP", "O756" =	"O75", "KO55"	= "K055",
      "I849" = "K649","k053"="K053", "S027" = "S026", "C536" = "N879", "H4011X1"="H401191",
      "Z225" = "Z228","k055" = "K0530","G419" = "G459",  "H4011X2"="H401191",
      "K103"="M273", "K350" = "K3532", "N459" = "N453", "S327"="S3281","T793" = "L89",
      "Z2250"="Z228","D895" = "S02401A", "H131" = "H109",  "HI51" = "H1510","K351" = "K358",
      "K37" = "K377", "R568"= "R569", "T144" = "T149", "T814XXS" = "T814XXS", "G41"="G459","K009"= "K009",
      "K914" = "K9410", "M840" = "M8430XP","S0210XD" = "S0210" ,"S0267XA"= "S0267"  ,
      "S028XXD" = "S028", "S028XXS" = "S028","T1490" = "T1490","T814XXD" = "T814", "Z22" = "Z2259",
      "I849" = "K649"
    )
  )

#
ipdlookup <- ipd_condition_df %>%
  distinct(patient_no) %>%
  mutate(
    pseudo_id = paste0(
      "TTH",
      str_pad(row_number(), 8, pad = "0")
    )
  )

ipd_df_pseudo <- ipd_condition_df %>%
  left_join(ipdlookup, by = "patient_no") %>%
  select(-patient_no) |>
  mutate(dx_month=month(dx_date, label = T)) |> 
  select(ID=pseudo_id, age, gender,dx_month, dx_date,icd10_code=icd10_code2, 
         dx_description,old_new_case,diagnosis_type,ward,department) 

ipd_df_pseudo_nA <- filter(ipd_df_pseudo, is.na(major))


write_csv(ipdlookup, "DataFiles/Metadata Repo/ipd_reidentification_key.csv")

write_csv(ipd_df_pseudo,"DataFiles/Database_Registers/Data_Warehouse/IPD_AD_Diagnoses/2026-JAN-SEP_TTH-IPD-CONDITIONS-V03.csv")

writexl::write_xlsx(ipd_df_pseudo, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/2026-JAN-SEP_TTH-IPD-CONDITIONS-V03.xlsx")




#################
# MORTALITIES
#################

Mort0 <- readxl::read_xlsx("../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Database_Registers/Mortality Register/MortalityRegister_01Jan2026_30Jun2026.xlsx") |> 
  clean_names()

Mort_df <- Mort0 |> mutate(
  discharge_date = as_date(discharge_date_date),
  month=month(discharge_date, label = T),
  ward = stringr::str_squish(str_to_upper(ward_room)),
  ward_dept = recode(ward, !!!deframe(lookup_dept_ward))
)


# TTH SUB
tth_sub_mort <- tabyl(Mort_df,ward_dept, month) |> 
  adorn_totals(where = c("row", "col")) |> 
  select(Deparment=ward_dept, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# SURGERY SUB
tth_surgery_mort <- Mort_df |> 
  filter(ward_dept=="Surgery") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# O&G SUB
tth_OG_mort <- Mort_df |> 
  filter(ward_dept=="Obstetrics & Gynaecology") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# DEENT SUB
tth_DEENT_mort <- Mort_df |> 
  filter(ward_dept=="DEENT") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# Internal Medicine SUB
tth_IM_mort <- Mort_df |> 
  filter(ward_dept=="Internal Medicine") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# Poly Clinic SUB
tth_Poly_mort <- Mort_df |> 
  filter(ward_dept=="Poly Clinic") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# Trauma & Orthopaedics SUB
tth_ORTH_mort <- Mort_df |> 
  filter(ward_dept=="Trauma & Orthopaedics") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# # Corporate Clinic SUB
# tth_COP_transout <- tout_df |> 
#   filter(ward_dept=="Corporate Clinic") |> 
#   tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
#   select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# A&E SUB
tth_AE_mort <- Mort_df |> 
  filter(ward_dept=="Accident & Emergency") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)

# Paediatrics & Child Health Sub

tth_PAED_mort <- Mort_df |> 
  filter(ward_dept=="Paediatrics & Child Health") |> 
  tabyl(ward, month) |> adorn_totals(where = c("row", "col")) |> 
  select(Ward=ward, Jan ,  Feb ,  Mar,   Apr,   May ,  Jun, Total)


####### Writing to excel sheets (Mortality WorkBook)
mortality <- createWorkbook()
addWorksheet(mortality, "TTH")
addWorksheet(mortality, "Accident & Emergency")
addWorksheet(mortality, "DEENT")
addWorksheet(mortality, "Internal Medicine")
addWorksheet(mortality, "Obstetrics & Gynaecology")
addWorksheet(mortality, "Paediatrics & Child Health")
addWorksheet(mortality, "Poly Clinic")
addWorksheet(mortality, "Surgery")
addWorksheet(mortality, "Trauma & Orthopaedics")

writeData(mortality, "TTH", tth_sub_mort)
writeData(mortality, "Accident & Emergency", tth_AE_mort)
writeData(mortality, "DEENT", tth_DEENT_mort)
writeData(mortality, "Internal Medicine", tth_IM_mort)
writeData(mortality, "Obstetrics & Gynaecology", tth_OG_mort)
writeData(mortality, "Paediatrics & Child Health", tth_PAED_mort)
writeData(mortality, "Poly Clinic", tth_Poly_mort)
writeData(mortality, "Surgery", tth_surgery_mort)
writeData(mortality, "Trauma & Orthopaedics", tth_ORTH_mort)

saveWorkbook(mortality, "../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-JUN_TTH-MORTALITIES_V01.xlsx", overwrite = TRUE) 





















#tth_patient_days,tth_adm_in, tth_transin,tth_disc,


####### Writing to excel sheets (Admissions WorkBook)
Indicators <- createWorkbook()
addWorksheet(Indicators, "Consulting Room Visits")
addWorksheet(Indicators, "Patients Days")
addWorksheet(Indicators, "Admissions")
addWorksheet(Indicators, "Discharges")
addWorksheet(Indicators, "Trans INS")


writeData(Indicators, "Consulting Room Visits", tth_consulting_visits)
writeData(Indicators, "Patients Days", tth_patient_days)
writeData(Indicators, "Admissions", tth_adm_in)
writeData(Indicators, "Trans INS", tth_transin)
writeData(Indicators, "Discharges", tth_disc)


saveWorkbook(Indicators, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-SEP_TTH-INDICATORS.xlsx", overwrite = TRUE) 
