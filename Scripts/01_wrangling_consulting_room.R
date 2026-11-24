#---------Cleaning and Joining data Consulting Room Register --------------------#
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

opd_vist_df <- read_csv("Database_Registers/OPD_Register/2026-JAN_MAR_OPD-TTH-VISITS_v01.csv") %>% 
  mutate(pid = paste(patient_id,visit_date,type_of_insurance, sep="_")) %>% distinct(pid, .keep_all = T)

opd_vist_df2 <- read_csv("Database_Registers/OPD_Register/2026-JAN_MAR_OPD-TTH-VISITS_v01.csv") %>% 
  mutate(pid = paste(patient_id,visit_date, sep="_")) %>% distinct(pid, .keep_all = T)


#-------------ICD10 Extraction from OPD data

opd_df2 <- opd_df1 %>% 
  mutate( 
    ## Extracting ICD10-codes
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
    across(starts_with("dx"), ~na_if(., "")),
    sch_date = as_date(mdy_hm(schedule_date)),
    pid = paste(patient_id,sch_date,type_of_insurance, sep="_")
  ) %>% distinct(pid, .keep_all = T) 



  visit_diag_df <- right_join(opd_df2, opd_vist_df, by="pid") %>% 
    select(
      schedule_date, visit_no,patient_id=patient_id.y, patient_name, type_of_insurance=type_of_insurance.x,
      provisional_diagnosis,principal_diagnosis_new_case,principal_diagnosis_old_case,
      additional_diagnosis_1_new_case, additional_diagnosis_1_old_case, patient_clinic_status,consult_type_new
    )
# Share to HI repo
write_csv(visit_diag_df, "Share2HI_repo/03_Datasets/Raw_Submissions/2026-JAN_MAR_CONSULTING-ROOM-REGISTER_v01.csv")
  

opd_df3 <- opd_df2 %>% 
  pivot_longer(
    cols = starts_with("dx"),
    names_to = "diagnosis_type",
    values_to = "diagnosis"
  )  %>% drop_na(diagnosis) %>% 
  mutate(
    sch_date = as_date(mdy_hm(schedule_date)),
    pid = paste(patient_id,sch_date, sep="_"),
    pid2 = paste(patient_id,sch_date, diagnosis, sep="_"),
    dig_type = case_when(
      diagnosis_type %in% c("dx_an1","dx_an2","dx_an3","dx_an4", "dx_an5" ,"dx_an6",
                            "dx_pn1", "dx_pn2", "dx_pn3", "dx_pn4") ~"New",
      diagnosis_type %in% c("dx_ao1","dx_ao2","dx_ao2","dx_ao3", "dx_ao4" ,"dx_ao5",
                            "dx_po1", "dx_po2", "dx_po3", "dx_po4", "dx_po5") ~"Old",
      diagnosis_type %in% c("dx_pro1","dx_pro2","dx_pro3","dx_pro4", "dx_pro5") ~"Prov",
      ),
    dig_class = case_when(
      diagnosis_type %in% c(
        "dx_pn1", "dx_pn2", "dx_pn3", "dx_pn4","dx_po1", "dx_po2", "dx_po3", "dx_po4", "dx_po5"
        ) ~"Principal",
      diagnosis_type %in% c(
        "dx_ao1","dx_ao2","dx_ao2","dx_ao3", "dx_ao4" ,"dx_ao5","dx_an1","dx_an2","dx_an3","dx_an4", "dx_an5" ,"dx_an6",
        "dx_po1", "dx_po2", "dx_po3", "dx_po4", "dx_po5") ~"Additional",
      diagnosis_type %in% c("dx_pro1","dx_pro2","dx_pro3","dx_pro4", "dx_pro5") ~"Prov",
    )
  ) |> distinct(pid2, .keep_all = T)
    
    
opd_vist_diag <- inner_join(opd_df3, opd_vist_df2, , by='pid') |> drop_na(diagnosis) |> 
  select(visit_date,visit_no, patient_id = patient_id.x, consult_type_new,diagnosis,dig_type,dig_class)

writexl::write_xlsx(opd_vist_diag, "Share2HI_repo/03_Datasets/Raw_Submissions/2026-JAN-MAR_OPD-DIAGNOSES_TTH_v01.xlsx")


opd_df_wide <- opd_df3 %>%
  group_by(pid) %>%
  mutate(dx_num = row_number()) %>%
  pivot_wider(
    names_from = dx_num,
    values_from = diagnosis,
    names_prefix = "dx"
  ) %>%
  ungroup()

opd_df_wide <- opd_df3 %>%
  pivot_wider(
    names_from = diagnosis_type,
    values_from = diagnosis
  ) %>%
  arrange(pid)

opd_df <- left_join(opd_df_wide,opd_vist_df, by="pid") %>% distinct(pid, .keep_all = T)





