
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
library(digest)


#-------------- Import data Consulting Room Data 
opd_con   <- readxl::read_xlsx("../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Database_Registers/OPD_consulting_register/ConsultationRoomRegister_jan_june26.xlsx") |> clean_names()



# --- icd10 CM SETS

icd109   <- readxl::read_xlsx("../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/2019-icd10-cm.xlsx") |> clean_names() |> 
  mutate(icd10_code= code)





#-------------ICD10 Extraction from OPD data

# Pivot the multiple columns into a single column

opd_dx0 <- opd_con |> 
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
  "Physician Medical Consultation (CC)", "Co-operate Clinic",
  "Physiotherapy Consultation (CC)", "Co-operate Clinic",
  "Plastic Surgery Consultation (CC)", "Co-operate Clinic",
  "Neurosurgery Consultation (CC)","Co-operate Clinic",
  "Urology Consultation (CC)","Co-operate Clinic",
  "Thoracic Specialist Consultation (CC)", "Co-operate Clinic",
  "Speech Therapy Clinic (CC)","Co-operate Clinic",
  "Renal Consultation (CC)","Co-operate Clinic",
  "Psychiatric Consultation (CC)","Co-operate Clinic",
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
  "Wound Management" , "Wound Management Clinic"
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
  "Psychiatry Clinic","Poly Clinic",
  "Renal Clinic" ,"Internal Medicine",
  "Speech Therapy Clinic", "DEENT",
  "Surgery Anaesthesia Clinic", "Surgery",
  "Thoracic Surgery Clinic",  "Surgery",
  "Tuberculosis Clinic", "Internal Medicine",
  "Urology Clinic",  "Surgery",
  "Wound Management Clinic", "Poly Clinic",
)


opd_dx1 <- opd_dx0 %>%
  mutate(
    code=str_sub(str_remove_all(dx_code, fixed(".")), 1, 7),
    icd10_code= str_sub(str_remove_all(dx_code, fixed(".")), 1, 4),
    consulting_clinic = recode(clinic, !!!deframe(lookup_tbl)),
    consulting_dept = recode(consulting_clinic, !!!deframe(lookup_dept))
  ) |> 
  mutate(
    diagnosis_type2 = case_when(
      diagnosis_type %in% c("principal_diagnosis_old", "principal_diagnosis_new")~"Principal",
      diagnosis_type %in% c("additional_diagnosis_1_old_case","additional_diagnosis_1_new_case")~"Additional",
      diagnosis_type %in% c("provisional_diagnosis")~"Provisional",
      diagnosis_type %in% c("referral_diagnosis")~"Referral"
    ),
    case_type = case_when(
      diagnosis_type %in% c("principal_diagnosis_new","additional_diagnosis_1_new_case")~"New",
      diagnosis_type %in% c("principal_diagnosis_old", "additional_diagnosis_1_old_case")~"Old"
    ),
    # Ids to detect duplicated same day dx code
    pid_dxcode = paste(patient_id,schedule_date,dx_code, sep = "_"),
    ) |> extract_gdrg_parts(dx_code) |> 
  filter(!consulting_dept %in% c("Accident & Emergency","Day Procedures", "IPD")) |> 
  filter(!diagnosis_type2 %in% c("Referral","Provisional")) |> 
  distinct(pid_dxcode, .keep_all = T) |>  #duplicated same day dx code
  filter(has_gdrg=="FALSE")


# Pulling codes with GDR 

opd_dx_gdrs <- filter(opd_dx1, has_gdrg=="TRUE")

# 

# # ---------- Merging with ICD
# 
# opd_df3_icd_df2 <- left_join(opd_dx1, icd10, by= "code") |> 
#   distinct(pid_dxcode, .keep_all = T) |> 
#   filter(has_gdrg=="FALSE") # droping GDRS codes
  

#################################################################################
opd_condition_df <- opd_dx1 |> 
  select(
    patient_id, age = age,dx_date=schedule_date, gender, icd10_code=code, dx_description, 
    clinic = consulting_clinic, department = consulting_dept,
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
lookup <- opd_condition_df %>%
  distinct(patient_id) %>%
  mutate(
    pseudo_id = paste0(
      "TTH",
      str_pad(row_number(), 8, pad = "0")
    )
  )

opd_df_pseudo <- opd_condition_df %>%
  left_join(lookup, by = "patient_id") %>%
  select(-patient_id) |>
  select(ID=pseudo_id, , age, gender, dx_date,icd10_code=icd10_code2, 
         dx_description,old_new_case,diagnosis_type,clinic,department
        ) |> 
  left_join(icd109, by="icd10_code")
  
opd_df_pseudo_nA <- filter(opd_df_pseudo, is.na(major))

write_csv(lookup, "../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Metadata Repo/reidentification_key.csv")

write_csv(opd_df_pseudo,"../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Database_Registers/Data_Warehouse/OPD_Diagnoses/2026-JAN-JUN_TTH-OPD-CONDITIONS-V03.csv")

writexl::write_xlsx(opd_df_pseudo, "../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/OPD_Diagnoses_Datasets/2026-JAN-JUN_TTH-OPD-CONDITIONS-V03.xlsx")
