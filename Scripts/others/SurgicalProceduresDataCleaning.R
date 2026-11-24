
#-------------Surgical Procedures Data Cleaning --------------------#
# Author: Asiwome
# Date : 2026/06/18
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



#-------------- Import  data Consulting Room Data 

surpath <- "Database_Registers/2026_MAIN_THEATRE_SURGERY_REGISTER.xlsx"
sheet_names <- excel_sheets(surpath)


all_sheets <- sheet_names %>% 
  set_names() %>% 
  map(~ read_excel(path = surpath, sheet = .x, skip = 1))

combined_df <- sheet_names %>% 
  set_names() %>% 
  map_df(~ read_excel(path = surpath, sheet = .x, skip = 1), .id = "source_sheet")

surgical_proc_df <- combined_df |> clean_names() |> 
  mutate(
    patient_id = folder_number,
    date1 = janitor::excel_numeric_to_date(as.numeric(date)),
    date2 = dmy(date),
    surgery_date = date1,
    surgery_date = if_else(is.na(surgery_date), date2, date1),
    sex =case_when(sex %in% c("F", "f") ~ "Female", sex %in% c("M", "m") ~ "Male"),
    
  )  |> 
  bind_cols(parse_ages(surgical_proc_df$age))







# Create the lookup tibble
lookup_tbl <- tribble(
  ~old, ~New,
  "A&E RED", "Admission",
  "A&E YELLOW", "Antenatal Clinic",
  "A/E", "Audiology Clinic",
  "A/E Orange", "Audiology Clinic",
  "A/E RED", "Breast Clinic",
  "A/E Red", "Breast Clinic",
  "Brought in Dead (BID)", "Brought in Dead (BID)",
  "CARDIOLOGY CONSULT", "Cardiology Clinic",
  "CLINICAL PSYCHOLOGICAL CONSULT", "Clinical Psychology Clinic",
  "CLUB FOOT", "Club Foot",
  "COLORECTAL CONSULT", "Colorectal Clinic",
  "Antenatal Consultation (CC)",  "Co-operate Clinic",
  "Aneasthesia Consultation (CC)", "Co-operate Clinic",
  "Dental Consultation (CC)", "Co-operate Clinic",
  "Diet Consultation (CC)", "Co-operate Clinic",
  "ENT Consultation (CC)", "Co-operate Clinic",
  "General Consultation (CC)", "Co-operate Clinic",
  "General Surgery Consultation (CC)", "Co-operate Clinic",
  "General Surgery Consultation  (CC)", "Co-operate Clinic",
  "Gyneacology Consultation (CC)", "Co-operate Clinic",
  "Neurology Clinic Consult (CC)", "Co-operate Clinic",
  "Orthopaedics Consultation (CC)", "Co-operate Clinic",
  "Paediatric OPD Consultation (CC)", "Co-operate Clinic",
  "Physician Medical Consultation (CC)", "Co-operate Clinic",
  "Physiotherapy Consultation (CC)", "Co-operate Clinic",
  "Plastic Surgery Consultation (CC)", "Co-operate Clinic",
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
  "FAMILY MEDICINE", "Family Medicine",
  "GENERAL PHYSIO CONSULTATION", "General Physiotherapy Clinic",
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
  "Wound Management" , "Wound Management Clinic"
)

# Create the lookup tibble departments
lookup_dept <- tribble(
  ~old, ~New,
  "ADMISSION VISIT", "IPD",
  "ANTENATAL CONSULT", "Obstetrics & Gynaecology",
  "AUDIOLOGYCONSULT", "DEENT",
  "AUDIOLOGYONSULT", "DEENT",
  "BREAST CONSULT", "Surgery",
  "BREAST SCREENING", "Surgery",
  "Brought in Dead (BID)", "BID",
  "CARDIOLOGY CONSULT", "Internal Medicine",
  "CLINICAL PSYCHOLOGICAL CONSULT", "Internal Medicine",
  "CLUB FOOT", "Trauma & Orthopaedics",
  "ORTHOPAEDICS CONSULT", "Trauma & Orthopaedics",
  "COLORECTAL CONSULT", "Surgery",
  "Antenatal Consultation (CC)",  "Co-operate Clinic",
  "Aneasthesia Consultation (CC)", "Co-operate Clinic",
  "Dental Consultation (CC)", "Co-operate Clinic",
  "Diet Consultation (CC)", "Co-operate Clinic",
  "ENT Consultation (CC)", "Co-operate Clinic",
  "General Consultation (CC)", "Co-operate Clinic",
  "General Surgery Consultation  (CC)", "Co-operate Clinic",
  "Gyneacology Consultation (CC)", "Co-operate Clinic",
  "Neurology Clinic Consult (CC)", "Co-operate Clinic",
  "Orthopaedics Consultation (CC)", "Co-operate Clinic",
  "Paediatric OPD Consultation (CC)", "Co-operate Clinic",
  "Physician Medical Consultation (CC)", "Co-operate Clinic",
  "Physiotherapy Consultation (CC)", "Co-operate Clinic",
  "Plastic Surgery Consultation (CC)", "Co-operate Clinic",
  "DAY PROCEDURE", "Day Procedures",
  "DENTAL CONSULTATION", "DEENT",
  "DIABETES CLINIC", "Internal Medicine",
  "DIALYSIS (CASH)", "Internal Medicine",
  "DIRECT ADMISSION", "Admission",
  "ENDOCRINE" , "Internal Medicine",
  "Diet Therapy Clinic", "Internal Medicine",
  "NUTRITION & DIET THERAPY", "Internal Medicine",
  "EMERGENCY", "Accident & Emergency",
  "ENT CONSULTATION", "DEENT",
  "EXERCISE THERAPY", "Poly Clinic",
  "FAMILY MEDICINE", "Poly Clinic",
  "GENERAL PHYSIO CONSULTATION", "Poly Clinic",
  "GENERAL SURGERY", "Surgery",
  "GYNAE CONSULT", "Obstetrics & Gynaecology",
  "INFECTIOUS DISEASE", "Internal Medicine",
  "Gastroenterology", "Internal Medicine",
  "General Poly Consultation", "Poly Clinic",
  "Haemodialysis 1st Visit", "Internal Medicine",
  "Haemodialysis 2nd Visit", "Internal Medicine",
  "Haemodialysis 3rd Visit", "Internal Medicine",
  "Haemodialysis 4th Visit", "Internal Medicine",
  "Haemodialysis 5th Visit", "Internal Medicine",
  "Haemodialysis 6th Visit", "Internal Medicine",
  "Haemodialysis 7th Visit", "Internal Medicine",
  "Haemodialysis 8th Visit", "Internal Medicine",
  "Heart Failure Consult", "Internal Medicine",
  "NEURO CONSULT", "Internal Medicine",
  "NEURO SURGERY", "Surgery",
  "NICU FOLLOW UP CLINIC", "Paediatrics & Child Health",
  "NICU OPD CONSULT", "Paediatrics & Child Health",
  "NICU OPDONSULT", "Paediatrics & Child Health",
  "O&G Anaesthesisa Clinic", "Obstetrics & Gynaecology",
  "OPTHALMOLOGY", "DEENT",
  "OPTOMETRIST [SPECIALIST CONSULTATION]", "DEENT",
  "PAED ONCOLOGY CONSULT", "Paediatrics & Child Health",
  "PAED RENAL", "Paediatrics & Child Health",
  "PAED SICKLE CELL", "Paediatrics & Child Health",
  "PAEDIATRIC CONSULTATION" , "Paediatrics & Child Health",
  "PAEDIATRIC EMERGENCY", "Paediatrics & Child Health",
  "PAEDIATRIC SPECIALIST CONSULT", "Paediatrics & Child Health",
  "PAEDIATRIC SURGERY CONSULT", "Surgery",
  "PAIN THERAPY" , "Poly Clinic",
  "PALLATIVE CARE", "Internal Medicine",
  "PHYSICIAN GENERAL" , "Poly Clinic",
  "PHYSIO SESSION [NHIS]", "Poly Clinic",
  "PLASTIC SURGER CONSULT", "Surgery",
  "POSTNATAL CONSULT", "Obstetrics & Gynaecology",
  "PSYCHIATRY CONSULT", "Internal Medicine",
  "RENAL CLINIC", "Internal Medicine",
  "SPEECH THERAPY", "DEENT",
  "ONCOLOGY CONSULT", "Surgery",
  "OBSETRICS EMERGENCY CONSULT", "Obstetrics & Gynaecology",
  "OPTHALMOLOGY", "DEENT",
  "OPTOMETRIST [SPECIALIST CONSULTATION]", "DEENT",
  "SURGEREY ANAESTESIA CONSULT", "Surgery",
  "SURGERY ANAESTESIA CONSULT", "Surgery",
  "Specialized - Diet Therapy Clinic", "Internal Medicine",
  "THORACIC CONSULTATION", "Surgery",
  "TUBERCULOSIS CONSULT", "Internal Medicine",
  "UROLOGY CONSULT", "Surgery",
  "UROLOGY DAY SERVICE", "Surgery",
  "UROLOGY DAYSERVICE", "Surgery",
  "Wound Management" , "Poly Clinic"
)

opd_df3_df <- opd_df3 %>%
  mutate(
    icd10_code= str_sub(str_remove_all(diagnosis, fixed(".")), 1, 4),
    consult_clinic = recode(clinic, !!!deframe(lookup_tbl)),
    consult_dept = recode(clinic, !!!deframe(lookup_dept))
  ) |> 
  mutate(
    diagnosis_type2 = case_when(
      diagnosis_type %in% c("dx_pn1", "dx_pn2")~"Principal",
      diagnosis_type %in% c("dx_an1","dx_an2", "dx_an3", "dx_an4", "dx_an5",
                            "dx_ao1", "dx_ao2", "dx_ao3", "dx_ao4", "dx_ao5")~"Additional"
    ),
    case_type = case_when(
      diagnosis_type %in% c("dx_pn1","dx_pn2", "dx_pn1", "dx_an1","dx_an2", "dx_an3", "dx_an4","dx_an5")~"New",
      diagnosis_type %in% c("dx_ao1", "dx_ao2","dx_ao3", "dx_ao4","dx_ao5")~"Old"
    )
  ) |> 
  filter(!consult_dept %in% c("Admission", "BID", "Day Procedures", "IPD"))



# ---------- Merging with ICD

opd_df3_icd_df2 <- left_join(opd_df3_df, icd10, by= "icd10_code") |> distinct(pid, .keep_all = T)


#################################################################################
opd_condition_df <- opd_df3_icd_df2 |> 
  select(
    patient_id, icd10_age = age,icd10_date=sch_date, icd10_code, clinic = consult_clinic, department = consult_dept,
    case = case_type,major_desc = major, short_desc, chapter
  )
write_csv(opd_condition_df,"Database_Registers/Data_Warehouse/OPD_Diagnoses/2026-JAN-APR_OPD-CONDITIONS-TTH.csv")










