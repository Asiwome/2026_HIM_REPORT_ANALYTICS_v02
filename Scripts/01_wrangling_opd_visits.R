
#-------------Cleaning and Joining data OPD Visits --------------------#
# Author: Asiwome
# Date : 2026/04/11
#. Upated with 3rd Quater only for Surgery Directorate , Polyclinic and DEENT

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
library(here)

#-------------- Import  data
# All visits MS excel
opd_visit <- list.files(path = "DataFiles/Database_Registers/OPD_Register/visits/",    
                        pattern = "*.xlsx",
                        full.names = TRUE)  |>  
  lapply(read_excel) |> 
  bind_rows() |> clean_names()

# All visits CSV
opd_visit2 <- list.files(path = "DataFiles/Database_Registers/OPD_Register/visits/",    
                        pattern = "*.csv",
                        full.names = TRUE)  |>  
  lapply(read_csv) |> 
  bind_rows() |> clean_names() |> 
  mutate(schedule_date = as.POSIXct(
    schedule_date, 
    format = "%m/%d/%Y %I:%M:%S %p", 
    tz = "UTC"
  ))

opd_visit_df0 <- bind_rows(opd_visit, opd_visit2)

#-------Wrangling

opd_visit_df1 <- opd_visit_df0 %>%
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
  "Family Medicine", "Poly Clinic",
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
  "Psychiatry Clinic","Internal Medicine",
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


opd_visit_df2 <- opd_visit_df1 %>%
  mutate(
    visiting_clinic = recode(clinic, !!!deframe(lookup_tbl)),
    visiting_dept = recode(visiting_clinic, !!!deframe(lookup_dept))
  )

lookup_visit <- opd_visit_df2 %>%
  distinct(patient_id) %>%
  mutate(
    pseudo_id = paste0(
      "TTH",
      str_pad(row_number(), 8, pad = "0")
    )
  )

opd_visit_pseudo <- opd_visit_df2 %>%
  left_join(lookup_visit, by = "patient_id") %>%
  mutate(monthlab= month(visit_date, label = T)) |> 
  select(-patient_id) |>
  select(ID=pseudo_id , age, gender,monthlab,visit_date, 
         payer,visiting_clinic, visiting_dept) |> 
  filter(!visiting_dept %in% c("Accident & Emergency", "Direct investigations",
                               "HR/ADMIN/Research Activity","IPD" ))

write_csv(lookup_visit, "visit_reidentification_key.csv")
write_csv(opd_visit_pseudo,"DataFiles/Database_Registers/Data_Warehouse/OPD_Visits/2026-JAN-SEPT_OPD-VISITS-TTH_V04.csv")
writexl::write_xlsx(opd_visit_pseudo, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/2026-JAN-SEPT_OPD-VISITS-TTH_V04.xlsx")



#########################################################################################################
## USING CONSULTING ROOM REGISTER
#########################################################################################################
#---- Import data Consulting Room Data 
opd_con_visit0 <- readxl::read_xlsx("DataFiles/Database_Registers/OPD_consulting_register/ConsultationRoomRegister_jan_june26.xlsx") |> clean_names()
opd_con_visit1 <- read_csv("DataFiles/Database_Registers/OPD_consulting_register/ConsultationRoomRegister_01 Jul 2026 00_00_00_to_30 Sep 2026 23_59_59 (1).csv") |> 
  clean_names()  |> 
  mutate(schedule_date = parse_date_time(
    schedule_date, 
    orders = c("mdy HM", "mdy HMS", "mdy", "ymd"), 
    tz = "UTC"
  ))

opd_con_df0 <- bind_rows(opd_con_visit0, opd_con_visit1)

opd_con_df <- opd_con_df0 %>%
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


opd_con_df2 <- opd_con_df %>%
  mutate(
    visiting_clinic = recode(clinic, !!!deframe(lookup_tbl)),
    visiting_dept = recode(visiting_clinic, !!!deframe(lookup_dept))
  )



opd_con_lookup_visit <- opd_con_df2 %>%
  distinct(patient_id) %>%
  mutate(
    pseudo_id = paste0(
      "TTH",
      str_pad(row_number(), 8, pad = "0")
    )
  )


opd_con_visit_pseudo <- opd_con_df2 %>%
  left_join(opd_con_lookup_visit, by = "patient_id") %>%
  mutate(monthlab= month(visit_date, label = T)) |> 
  select(-patient_id) |>
  select(ID=pseudo_id , age, gender, visit_date,monthlab, 
         payer,visiting_clinic, visiting_dept) |> 
  filter(!visiting_dept %in% c("Accident & Emergency", "Direct investigations",
                               "HR/ADMIN/Research Activity","IPD" ))

write_csv(lookup_visit, "visit_reidentification_key.csv")

write_csv(opd_con_visit_pseudo,"DataFiles/Database_Registers/Data_Warehouse/OPD_Visits/2026-JAN-SEP_OPD-CONSULTING-ROOM-VISITS-TTH_V03.csv")
writexl::write_xlsx(opd_con_visit_pseudo, "DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/2026-JAN-SEP_OPD-CONSULTING-ROOM-VISITS-TTH_V03.xlsx")

# TTH CONSULTING ROOM ATTENDANCE

tth_consulting_visits_q1 <- opd_con_visit_pseudo |> filter(monthlab %in% c("Jan", "Feb", "Mar")) |>
  tabyl(visiting_dept, monthlab) |> adorn_totals(where = c("row", "col")) |>
  select(Department=visiting_dept, Jan ,  Feb ,  Mar,Q1=Total)

tth_consulting_visits_q2 <- opd_con_visit_pseudo |> filter(monthlab %in% c("Apr", "May", "Jun")) |>
  tabyl(visiting_dept, monthlab) |> adorn_totals(where = c("row", "col")) |>
  select(Department=visiting_dept,  Apr,   May ,  Jun, Q2=Total)

tth_consulting_visits_h1 <- opd_con_visit_pseudo |> filter(monthlab %in% c("Jan", "Feb", "Mar","Apr", "May", "Jun")) |>
  tabyl(visiting_dept, monthlab) |> adorn_totals(where = c("row", "col")) |>
  select(Department=visiting_dept,  H1 =Total)

tth_consulting_visits_q3 <- opd_con_visit_pseudo |> filter(monthlab %in% c("Jul", "Aug", "Sep")) |>
  tabyl(visiting_dept, monthlab) |> adorn_totals(where = c("row", "col")) |>
  select(Department=visiting_dept,Jul,Aug, Sep, Q3 =Total)


tth_consulting_visits <- inner_join(tth_consulting_visits_q1, tth_consulting_visits_q2, by="Department") |> 
  inner_join(tth_consulting_visits_h1, by="Department") |> 
  inner_join(tth_consulting_visits_q3, by="Department")


writexl::write_xlsx(tth_consulting_visits, "DataFiles/Share2HI_repo/03_Datasets/Cleaned_Data/2026-JAN-SEP_CONSULTING-ROOM-VISITS-TTH.xlsx")


# SURGERY SUB
surgery_consulting_visits_q1 <- patient_days2 |> 
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
surgery_patientdays <- inner_join(surgery_patientday_q1, surgery_patientday_q2, by="Ward") |> inner_join(surgery_patientday_h1, by="Ward")

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

OG_patientdays <- inner_join(OG_patientday_q1, OG_patientday_q2, by="Ward") |> inner_join(OG_patientday_h1, by="Ward")



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

DEENT_patientdays <- inner_join(DEENT_patientday_q1, DEENT_patientday_q2, by="Ward") |> inner_join(DEENT_patientday_h1, by="Ward")


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


IM_patientdays <- inner_join(IM_patientday_q1, IM_patientday_q2, by="Ward") |> inner_join(IM_patientday_h1, by="Ward")



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


Poly_patientdays <- inner_join(Poly_patientday_q1, Poly_patientday_q2, by="Ward") |> inner_join(Poly_patientday_h1, by="Ward")


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


ORTH_patientdays <- inner_join(ORTH_patientday_q1, ORTH_patientday_q2, by="Ward") |> inner_join(ORTH_patientday_h1, by="Ward")


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


AE_patientdays <- inner_join(AE_patientday_q1, AE_patientday_q2, by="Ward") |> inner_join(AE_patientday_h1, by="Ward")



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


PAED_patientdays <- inner_join(PAED_patientday_q1, PAED_patientday_q2, by="Ward") |> inner_join(PAED_patientday_h1, by="Ward")

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

saveWorkbook(patdays, "../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Bed_Indicators_datasets/2026-JAN-JUN_TTH-PATIENTS-DAYS_V02.xlsx",overwrite = TRUE)
  




