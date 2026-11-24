
#-------------Cleaning and Joining data --------------------#
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
library(googledrive)
library(here)

#-------------- Import  data Consulting Room Data 
opd_con   <- read_csv("Database_Registers/OPD_consulting_register/ConsultationRoomRegister_01 Jan 2026 00_00_00_to_30 Jun 2026 23_59_59.csv") |> clean_names()
opd_con1   <- read_csv("Database_Registers/ConsultingRoomShedu_register/ConsultationRoomRegister_01 Jan 2026 00_00_00_to_30 Jun 2026 23_59_59.csv") |> clean_names()
opd_con <- read_csv("C:/Users/DELL/OneDrive - Smart Workplace/TTH Analytics and Reports/2026-TTH-REPORT-ANALYTICS/2026_TTH_SURGERY_REPORTING_ANALYTICS/Database_Registers/OPD_consulting_register/ConsultationRoomRegister_01 Jan 2026 00_00_00_to_30 Jun 2026 23_59_59.csv") |> 
  clean_names()

opd_con <- read_csv("../Database_Registers/OPD_consulting_register/ConsultationRoomRegister_01 Jan 2026 00_00_00_to_30 Jun 2026 23_59_59.csv") |> 
  clean_names()

list.files()
# Or to see if Database_Registers is a level up:
list.files("../")

# --- icd10 CM SETS

icd10   <- readxl::read_xlsx("2019-2021-icd10-cm.xlsx") |> clean_names() |> 
  mutate(icd10_code= str_sub(str_remove_all(code, fixed(".")), 1, 4))

#-------------ICD10 Extraction from OPD data


opd_df2 <- opd_con|> 
  mutate( ## Extracting ICD10-codes
    # principal_diagnosis_new_case
    dx_pn1 = str_extract_all(principal_diagnosis,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)) |> view()


opd_df2 <- opd_con|> 
  mutate( ## Extracting ICD10-codes
    # new_case
    dx_pn1 = str_extract_all(principal_diagnosis,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,1],
    dx_pn2 = str_extract_all(principal_diagnosis,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,2],
    dx_an1 = str_extract_all(additional_diagnosis_1_new_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,1],
    dx_an2 = str_extract_all(additional_diagnosis_1_new_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,2],
    dx_an3 = str_extract_all(additional_diagnosis_1_new_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,3],
    dx_an4 = str_extract_all(additional_diagnosis_1_new_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,4],
    dx_an5 = str_extract_all(additional_diagnosis_1_new_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,5],
    dx_an6 = str_extract_all(additional_diagnosis_1_new_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,6],
    # Old case
    dx_ao1 = str_extract_all(additional_diagnosis_1_old_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,1],
    dx_ao2 = str_extract_all(additional_diagnosis_1_old_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,2],
    dx_ao3 = str_extract_all(additional_diagnosis_1_old_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,3],
    dx_ao4 = str_extract_all(additional_diagnosis_1_old_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,4],
    dx_ao5 = str_extract_all(additional_diagnosis_1_old_case,"\\b[A-TV-Z][0-9]{2}(\\.[A-Z0-9]{1,4})?\\b", simplify = TRUE)[,5]
  ) |> mutate(
    across(starts_with("dx"), ~na_if(., ""))
  )


opd_df3 <- opd_df2 |> 
  pivot_longer(
    cols = starts_with("dx"),
    names_to = "diagnosis_type",
    values_to = "diagnosis"
  )   |>  drop_na(diagnosis) |> 
  mutate(
    sch_date = as_date(mdy_hms(schedule_date)),
    pid = paste(patient_id,sch_date,diagnosis, sep="_"),
    diagnosis = str_remove_all(diagnosis, fixed("."))
  ) |> distinct(pid, .keep_all = TRUE)



# Create the lookup tibble
lookup_tbl <- tribble(
  ~old, ~New,
  "ADMISSION VISIT", "Admission",
  "ANTENATAL CONSULT", "Antenatal Clinic",
  "AUDIOLOGYCONSULT", "Audiology Clinic",
  "AUDIOLOGYONSULT", "Audiology Clinic",
  "BREAST CONSULT", "Breast Clinic",
  "BREAST SCREENING", "Breast Clinic",
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










