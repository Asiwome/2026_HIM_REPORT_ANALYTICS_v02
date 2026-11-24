
#-------------Cleaning and Joining data --------------------#
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
library(googledrive)


#-------------- Import  data

opd_df1   <- read_csv("Database_Registers/OPD_consulting_register/2026-JAN-MAR_CONSULT_REGISTER.csv")
visit_df1 <- read_csv("Database_Registers/visit_register_merged/2026-Jan-Mar_Allvisit_v01.csv")


#-------------ICD10 Extraction from OPD data

opd_df2 <- opd_df1 %>% 
mutate( ## Extracting ICD10-codes
  # principal_diagnosis_new_case
  dx_pn1 = str_extract_all(principal_diagnosis_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
  dx_pn2 = str_extract_all(principal_diagnosis_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
  dx_pn3 = str_extract_all(principal_diagnosis_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
  dx_pn4 = str_extract_all(principal_diagnosis_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
  # principal_diagnosis_old_case
  dx_po1 = str_extract_all(principal_diagnosis_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
  dx_po2 = str_extract_all(principal_diagnosis_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
  dx_po3 = str_extract_all(principal_diagnosis_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
  dx_po4 = str_extract_all(principal_diagnosis_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
  
  # additional_diagnosis_1_new_case
  dx_an1 = str_extract_all(additional_diagnosis_1_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
  dx_an2 = str_extract_all(additional_diagnosis_1_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
  dx_an3 = str_extract_all(additional_diagnosis_1_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
  dx_an4 = str_extract_all(additional_diagnosis_1_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
  dx_an5 = str_extract_all(additional_diagnosis_1_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,5],
  dx_an6 = str_extract_all(additional_diagnosis_1_new_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,6],
  
  # additional_diagnosis_1_old_case
  dx_ao1 = str_extract_all(additional_diagnosis_1_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
  dx_ao2 = str_extract_all(additional_diagnosis_1_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
  dx_ao3 = str_extract_all(additional_diagnosis_1_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
  dx_ao4 = str_extract_all(additional_diagnosis_1_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
  dx_ao5 = str_extract_all(additional_diagnosis_1_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,5],
  dx_ao6 = str_extract_all(additional_diagnosis_1_old_case,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,6],
  
  # provisional_diagnosis
  dx_pro1 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
  dx_pro2 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
  dx_pro3 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
  dx_pro4 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
  dx_pro5 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,5],
  dx_pro6 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,6],
  dx_pro7 = str_extract_all(provisional_diagnosis,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,7]
) |> mutate(
  across(starts_with("dx"), ~na_if(., ""))
)
opd_df3 <- opd_df2 %>% 
  pivot_longer(
    cols = starts_with("dx"),
    names_to = "diagnosis_type",
    values_to = "diagnosis"
  )  %>% drop_na(diagnosis) %>% 
  mutate(
    sch_date = as_date(mdy_hm(schedule_date)),
    pid = paste(patient_id,sch_date, sep="_")
  ) %>% 
  distinct(pid, .keep_all = TRUE) 


visit <- visit_df1 %>%
  mutate(
    visitdate = as_date(visitdate),
    patient_id = str_trim(toupper(patient_id))
  ) %>% rename(visit_date = visitdate) %>% 
  select(visit_no, patient_id, visit_date, consult_type, sub_bmc_directorate,branch_level)


# Create the lookup tibble
lookup_tbl <- tribble(
  ~old, ~New,
  "ADMISSIONVISIT", "Admission",
  "ADMISSIONISIT", "Admission",
  "ADULT SICKLEELL","Adult Sickle Cell Clinic",
  "ADULT SICKLECELL", "Adult Sickle Cell Clinic",
  "ANTENATAL CONSULT", "Antenatal Clinic",
  "ANTENATALCONSULT", "Antenatal Clinic",
  "ANTENATALONSULT", "Antenatal Clinic",
  "AUDIOLOGYCONSULT", "Audiology Clinic",
  "AUDIOLOGYONSULT", "Audiology Clinic",
  "BREAST CONSULT", "Breast Clinic",
  "BREASTCONSULT", "Breast Clinic",
  "BREASTCREENING", "Breast Clinic",
  "BREASTSCREENING", "Breast Clinic",
  "BREASTONSULT", "Breast Clinic",
  "CARDIOLOGYCONSULT", "Cardiology Clinic",
  "CARDIOLOGYONSULT", "Cardiology Clinic",
  "CLINICALPSYCHOLOGICALCONSULT", "Clinical Psychology Clinic",
  "CLINICALSYCHOLOGICALONSULT", "Clinical Psychology Clinic",
  "CLINICAL PSYCHOLOGICALCONSULT", "Clinical Psychology Clinic",
  "COLORECTALCONSULT", "Colorectal Clinic",
  "COLORECTALONSULT", "Colorectal Clinic",
  "DAY PROCEDURE", "Day Procedures",
  "DAYROCEDURE","Day Procedures",
  "DAYPROCEDURE", "Day Procedures",
  "DENTALCONSULTATION", "Dental Clinic",
  "DENTALONSULTATION", "Dental Clinic",
  "DIABETES CLINIC", "Diabetes Clinic",
  "DIABETESCLINIC", "Diabetes Clinic",
  "DIABETESLINIC", "Diabetes Clinic",
  "DIALYSIS (CASH)", "Dialysis Clinic",
  "DIALYSISCASH)", "Dialysis Clinic",
  "DIALYSIS(CASH)", "Dialysis Clinic",
  "DIRECT ADMISSION", "Admission",
  "DIRECTADMISSION", "Admission",
  "DIRECTDMISSION", "Admission",
  "Direct Labequest" , "Direct Lab Request",
  "ENDOCRINE" , "Endocrine",
  "Diet Therapy Clinic", "Diet Therapy Clinic",
  "Diet TherapyClinic", "Diet Therapy Clinic",
  "Diet Therapylinic", "Diet Therapy Clinic",
  "NUTRITION &DIET THERAPY", "Diet Therapy Clinic",
  "EMERGENCY", "Emergency",
  "ENTCONSULTATION", "ENT Clinic",
  "ENTONSULTATION", "ENT Clinic",
  "EXERCISE THERAPY", "Exercise therapy",
  "EXERCISEHERAPY", "Exercise therapy",
  "EXERCISETHERAPY", "Exercise therapy",
  "FAMILYMEDICINE", "Family Medicine",
  "FAMILYEDICINE", "Family Medicine",
  "GENERAL PHYSIOCONSULTATION", "General Physiotherapy Clinic",
  "GENERAL SURGERY", "General Surgery Clinic",
  "GENERALHYSIOONSULTATION", "General Physiotherapy Clinic",
  "GENERALPHYSIOCONSULTATION", "General Physiotherapy Clinic",
  "GENERALSURGERY", "General Surgery Clinic",
  "GENERALURGERY", "General Surgery Clinic",
  "GYNAE CONSULT", "Gynaecological Clinic",
  "GYNAECONSULT", "Gynaecological Clinic",
  "GYNAEONSULT", "Gynaecological Clinic",
  "INFECTIOUSISEASE", "Infectious Disease",
  "Gastroenterology", "Gastroenterology Clinic",
  "General PolyConsultation", "General Poly  Clinic",
  "General Polyonsultation", "General Poly  Clinic",
  "Generalolyonsultation", "General Poly  Clinic",
  "Haemodialysis 1stVisit", "Dialysis Clinic",
  "Haemodialysis 1stisit", "Dialysis Clinic",
  "Haemodialysis 2ndVisit", "Dialysis Clinic",
  "Haemodialysis 2ndisit", "Dialysis Clinic",
  "Haemodialysis 3rdVisit", "Dialysis Clinic",
  "Haemodialysis 3rdisit", "Dialysis Clinic",
  "Haemodialysis 4thVisit", "Dialysis Clinic",
  "Haemodialysis 4thisit", "Dialysis Clinic",
  "Haemodialysis 5thVisit", "Dialysis Clinic",
  "Haemodialysis 5thisit", "Dialysis Clinic",
  "Haemodialysis 6thVisit", "Dialysis Clinic",
  "Haemodialysis 6thisit", "Dialysis Clinic",
  "Haemodialysis 7thVisit", "Dialysis Clinic",
  "Haemodialysis 7thisit", "Dialysis Clinic",
  "Haemodialysis 8thVisit", "Dialysis Clinic",
  "Haemodialysis 8thisit", "Dialysis Clinic",
  "Haemodialysis1st Visit", "Dialysis Clinic",
  "Haemodialysis2nd Visit", "Dialysis Clinic",
  "Haemodialysis3rd Visit", "Dialysis Clinic",
  "Haemodialysis4th Visit", "Dialysis Clinic",
  "Haemodialysis5th Visit", "Dialysis Clinic",
  "Haemodialysis6th Visit", "Dialysis Clinic",
  "Haemodialysis7th Visit", "Dialysis Clinic",
  "Haemodialysis8th Visit", "Dialysis Clinic",
  "Haemodialysisnd Visit", "Dialysis Clinic",
  "Haemodialysisrd Visit", "Dialysis Clinic",
  "Haemodialysisst Visit", "Dialysis Clinic",
  "Haemodialysisth Visit", "Dialysis Clinic",
  "MALE CONSULT", "Gynaecological Clinic",
  "NEURO CONSULT", "Neurosurgery Clinic",
  "NEURO SURGERY", "Neurosurgery Clinic",
  "NEUROCONSULT", "Neurosurgery Clinic",
  "NEUROONSULT", "Neurosurgery Clinic",
  "NEUROSURGERY", "Neurosurgery Clinic",
  "NEUROURGERY", "Neurosurgery Clinic",
  "NICU FOLLOW UP CLINIC", "NICU Follow-UP  Clinic",
  "NICU FOLLOW UPCLINIC", "NICU Follow-UP  Clinic",
  "NICU FOLLOWP CLINIC", "NICU Follow-UP  Clinic",
  "NICU FOLLOWUP CLINIC", "NICU Follow-UP  Clinic",
  "NICU OPD CONSULT", "NICU OPD Clinic",
  "NICU OPDCONSULT", "NICU OPD Clinic",
  "NICU OPDONSULT", "NICU OPD Clinic",
  "O&G Anaesthesisa Clinic", "O&G Anaesthesia Clinic",
  "O&GAnaesthesisaClinic" , "O&G Anaesthesia Clinic",
  "O&G Wound Clinic", "O&G Wound Clinic",
  "O&G WoundClinic", "O&G Wound Clinic",
  "ONCOLOGYCONSULT", "Oncology Clinic",
  "ONCOLOGYONSULT", "Oncology Clinic",
  "OBSETRICS EMERGENCYCONSULT", "Obstestircs Emergency",
  "OBSETRICS EMERGENCYONSULT", "Obstestircs Emergency",
  "OBSETRICSEMERGENCY CONSULT", "Obstestircs Emergency",
  "OBSETRICSEMERGENCYCONSULT", "Obstestircs Emergency",
  "OPD REVIEWCONSULT", "Gynaecological Clinic",
  "OPD REVIEW CONSULT" , "Gynaecological Clinic",
  "OPTHALMOLOGY", "Eye Clinic",
  "OPTOMETRISTSPECIALISTONSULTATION]", "Eye Clinic",
  "OPTOMETRIST[SPECIALISTCONSULTATION]", "Eye Clinic",
  "ORTHOPAEDICS CONSULT", "Orthopaedics Clinic",
  "ORTHOPAEDICSCONSULT", "Orthopaedics Clinic",
  "ORTHOPAEDICSONSULT", "Orthopaedics Clinic",
  "PAED RENAL", "Renal Clinic",
  "PAED SICKLECELL", "Paediatric Sickle Cell Clinic",
  "PAED SICKLEELL", "Paediatric Sickle Cell Clinic",
  "PAEDIATRICCONSULTATION", "Paediatric Specialist Clinic",
  "PAEDIATRICEMERGENCY", "Paediatric Emergency",
  "PAEDIATRICMERGENCY", "Paediatric Emergency",
  "PAEDIATRICONSULTATION", "Paediatric Specialist Clinic",
  "PAEDIATRICPECIALISTONSULT", "Paediatric Specialist Clinic",
  "PAEDIATRICSPECIALISTCONSULT", "Paediatric Specialist Clinic",
  "PAEDIATRICSURGERYCONSULT", "Paediatric Surgery Clinic",
  "PAEDIATRICURGERYONSULT", "Paediatric Surgery Clinic",
  "PAEDIATRIC SURGERYCONSULT","Paediatric Surgery Clinic",
  "PAEDIATRICHYSI", "Paediatric Physiotherapy Clinic",
  "PAEDIATRICHYSIO", "Paediatric Physiotherapy Clinic",
  "PAEDONCOLOGYCONSULT", "Paediatric Oncology  Clinic",
  "PAEDNCOLOGYONSULT", "Paediatric Oncology  Clinic",
  "PALLATIVE CARE" , "Pallative Clinic",
  "PAIN THERAPY", "Pain Therapy  Clinic",
  "PAINHERAPY", "Pain Therapy  Clinic",
  "PAINTHERAPY", "Pain Therapy  Clinic",
  "PHYSICIANENERAL", "General Physician Clinic/Consult",
  "PHYSICIANGENERAL", "General Physician Clinic/Consult",
  "PHYSICIAN GENERAL", "General Physician Clinic/Consult",
  "PHYSIO SESSION [NHIS]", "Physiotherapy Clinic",
  "PHYSIOESSION [NHIS]", "Physiotherapy Clinic",
  "PHYSIOESSIONNHIS]", "Physiotherapy Clinic",
  "PHYSIOSESSION [NHIS]", "Physiotherapy Clinic",
  "PHYSIOSESSION[NHIS]", "Physiotherapy Clinic",
  "PLASTIC SURGERCONSULT", "Plastic Surgery Clinic",
  "PLASTIC SURGERONSULT", "Plastic Surgery Clinic",
  "PLASTICSURGERCONSULT", "Plastic Surgery Clinic",
  "PLASTICURGERONSULT", "Plastic Surgery Clinic",
  "POSTNATAL CONSULT", "Postnatal Clinic",
  "POSTNATALCONSULT", "Postnatal Clinic",
  "PSYCHIATRY CONSULT", "Psychiatry Clinic",
  "PSYCHIATRYCONSULT", "Psychiatry Clinic",
  "PSYCHIATRYONSULT", "Psychiatry Clinic",
  "RENAL CLINIC", "Renal Clinic",
  "RENALCLINIC", "Renal Clinic",
  "SPEECHHERAPY", "Speech Therapy Clinic",
  "SPEECHTHERAPY", "Speech Therapy Clinic",
  "SURGEREYANAESTESIACONSULT", "Surgery Anaesthesia Clinic",
  "SURGEREYNAESTESIAONSULT", "Surgery Anaesthesia Clinic",
  "SURGERYANAESTESIACONSULT","Surgery Anaesthesia Clinic",
  "THORACICCONSULTATION", "Thoracic Surgery Clinic",
  "THORACICONSULTATION", "Thoracic Surgery Clinic",
  "TUBERCULOSISCONSULT", "Tuberculosis Clinic",
  "TUBERCULOSISONSULT", "Tuberculosis Clinic",
  "UROLOGYCONSULT", "Urology Clinic",
  "UROLOGYONSULT", "Urology Clinic",
  "UROLOGY DAYSERVICE", "Urology Clinic",
  "Upper GIEndoscopy", "Upper GI/Endoscopy",
  "WoundManagement", "Wound Management Clinic",
  "Woundanagement", "Wound Management Clinic"
)

unique_visits_df <- visit %>%
  mutate(
    consult_type_new = recode(consult_type, !!!deframe(lookup_tbl)),
    
    ) %>% distinct(visit_no,.keep_all = T) %>% select(
      visit_no, patient_id,visit_date,consult_type_new, sub_bmc_directorate, 
    )
  






opd <- opd_df3 %>%
  mutate(
    schedule_date = as_date(sch_date),
    patient_id = str_trim(toupper(patient_id))
  ) %>% rename(visit_date = schedule_date,
               name = patient_name)
  

visit_opd <- unique_visits_df %>%
  left_join(
    opd,
    by = c("patient_id", "visit_date")
  ) %>% drop_na(pid) %>% distinct(pid, .keep_all = T)


#################################################################################
opd_visits <- visit_opd %>% 
  mutate(
    diagnosis_type2 = case_when(
      diagnosis_type %in% c("dx_pn1", "dx_po1")~"Principal",
      diagnosis_type %in% c("dx_an1", "dx_ao1")~"Additional",
      diagnosis_type %in% c("dx_pro1")~"Provisional",
    ),
    case_type = case_when(
      diagnosis_type %in% c("dx_pn1", "dx_an1")~"New",
      diagnosis_type %in% c("dx_po1", "dx_ao1")~"Old",
      diagnosis_type %in% c("dx_pro1")~"Unknown",
    )
    ) %>% 
  select(
    visit_no,
    patient_id,
    name,
    visit_date,
    age,
    gender,
    sub_bmc_directorate,
    type_of_insurance,
    patient_clinic_status,
    diagnosis,
    case_type,
    diagnosis_type=diagnosis_type2,
    consult_type=consult_type_new
  )
write_csv(opd_visits,"Database_Registers/IPD_register/2026-JAN-MAR_VISIT-CONSULTING-LINKED_REGISTER.csv")

