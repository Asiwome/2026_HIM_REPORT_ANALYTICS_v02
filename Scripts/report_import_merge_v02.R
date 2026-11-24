#-------------Extracting data --------------------#
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



# ---- importing IPD

# SURGERY SUB BMC

IPDDF <- read_csv("Database_Registers/IPD_register/2026-JAN-MAR_IPD_REGISTER_v02.csv", skip = 7) 

  IPDD_PAED <- IPDDF %>% 
   mutate(
    adm_date=trimws(`Date and Time of Admission`),
    adm_date = parse_date_time(adm_date, orders = c("mdy", "mdy HM")),
    adm_date = as.Date(adm_date),
    disc_date=trimws(`Date and Time of Discharge`),
    disc_date = parse_date_time(disc_date, orders = c("mdy", "mdy HM")),
    disc_date = as.Date(disc_date),
    adm_month = month(adm_date, label = T),
    disc_month = month(disc_date, label = T),
  ) %>% 
  filter(Ward %in% c("NICU", "PAEDIATRIC BABIES UNIT", "PAEDIATRIC CRITICAL CARE WARD",
                     "PAEDIATRIC EAST WARD", "PAEDIATRIC EMERGENCY WARD",
                     "PAEDIATRIC MALNUTRITION WARD", "PAEDIATRIC ONCOLOGY UNIT", 
                     "PAEDIATRIC UNDER 6 WARD", "PAEDIATRIC WEST WARD")) %>% 
    select(Patient_ID = `Sr.No.`, `Patient Name`, Age,`Date and Time of Admission`,adm_date,adm_month,
            `Date and Time of Discharge`,disc_date,disc_month, Ward)

write_csv(IPDD_PAED, "Database_Registers/IPD_register/2026-PAED-IPD_REGISTER_V03.csv")








allipd_paed <- bind_rows(ipdjan, ipdfeb, ipdmar) %>% 
  mutate(
    adm_date=trimws(`Date and Time of Admission`),
    adm_date = parse_date_time(adm_date, orders = c("mdy", "mdy HM")),
    disc_date=trimws(`Date and Time of Discharge`),
    disc_date = parse_date_time(disc_date, orders = c("mdy", "mdy HM")),
    adm_month = month(adm_date, label = T),
    disc_month = month(disc_date, label = T)
    ) %>% 
  filter(Ward %in% c("NICU", "PAEDIATRIC BABIES UNIT", "PAEDIATRIC CRITICAL CARE WARD",
                     "PAEDIATRIC EAST WARD", "PAEDIATRIC EMERGENCY WARD",
                     "PAEDIATRIC MALNUTRITION WARD", "PAEDIATRIC ONCOLOGY UNIT", 
                     "PAEDIATRIC UNDER 6 WARD", "PAEDIATRIC WEST WARD"))



# Importing OPD Register
opddf <- read_csv(
  "C:/HIShareFiles/Interns_repository/Asi/2026-JAN-MAR_CONSULT_REGISTER_v02.csv", skip = 7
) %>%clean_names()

write_csv(opddf,"Database_Registers/OPD_consulting_register/2026-JAN-MAR_CONSULT_REGISTER.csv")



# Importing Visit 


# Surgery SUb BMC

# Get the file paths
surg_file_paths <- list.files(
  path = "Database_Registers/Visit_register/Surgery_visit_register/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read all files and combine, then mutate
surg_visits <- surg_file_paths |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")



surg_visit_df <- surg_visits %>%
  mutate(
    # Remove the suffix from X2
    visit_no = str_remove(X2, "-C$"),
    
    # Fix and parse the date in X3
    visitdate = X3 %>%
      str_replace("([A-Za-z])([0-9]{3})$", "\\12\\2") %>%
      str_replace("([0-9]{2})([A-Za-z]{3})([0-9]{4})", "\\1 \\2 \\3") %>%
      dmy(),
    
  )  %>%
  # 1. Split at the "]" to separate ID from Name
  separate_wider_delim(
    cols = X6,
    delim = "]",
    names = c("patient_id", "name"),
    too_many = "merge",
    too_few = "align_start"
  ) %>%
  # 2. Clean up the results
  mutate(
    # Remove any stray "[" from the ID
    patient_id = str_remove_all(patient_id, "\\["),
    # Trim whitespace from both
    patient_id = trimws(patient_id),
    name = trimws(name)
  ) %>% 
  select( 
    sn=X1,patient_id, name,visit_no, visitdate, consult_type=X4,consult_fee=X5,
    branch_level=X7, sub_bmc_directorate=X8, 
    ) %>% filter(patient_id !="BREASTCONSULT") %>% mutate(sub_bmc_directorate ="Surgery")

write_csv(surg_visit_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_surgery_visit_v01.csv")




# DEENT SUb BMC

# Get the file paths
deent_file_paths <- list.files(
  path = "Database_Registers/Visit_register/deent_visit_register/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read all files and combine, then mutate
deent_visits <- deent_file_paths |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")


deent_visits_df <- deent_visits %>%
  mutate(
    # Remove the suffix from X2
    visit_no = str_remove(X2, "-C$"),
    
    # Fix and parse the date in X3
    visitdate = X3 %>%
      str_replace("([A-Za-z])([0-9]{3})$", "\\12\\2") %>%
      str_replace("([0-9]{2})([A-Za-z]{3})([0-9]{4})", "\\1 \\2 \\3") %>%
      dmy(),
    
  )  %>%
  # 1. Split at the "]" to separate ID from Name
  separate_wider_delim(
    cols = X6,
    delim = "]",
    names = c("patient_id", "name"),
    too_many = "merge",
    too_few = "align_start"
  ) %>%
  # 2. Clean up the results
  mutate(
    # Remove any stray "[" from the ID
    patient_id = str_remove_all(patient_id, "\\["),
    # Trim whitespace from both
    patient_id = trimws(patient_id),
    name = trimws(name)
  ) %>% select( 
    sn=X1,patient_id, name,visit_no, visitdate, consult_type=X4,consult_fee=X5,
    branch_level=X7, sub_bmc_directorate=X8, 
  ) %>% drop_na(visitdate)%>% mutate(sub_bmc_directorate ="DEENT")

write_csv(deent_visits_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_deent_visit_v01.csv")

# 2ND Version

 aud_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Audiology_visit.xlsx",sheet = 'Jan')
 aud_feb <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Audiology_visit.xlsx",sheet = 'Feb')
 aud_mar <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Audiology_visit.xlsx",sheet = 'Mar')
 
 dental_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Dental_visit.xlsx",sheet = 'Jan')
 dental_Feb <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Dental_visit.xlsx",sheet = 'Feb')
 dental_Mar <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Dental_visit.xlsx",sheet = 'Mar')
 
opht_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Ophtalmology_visit.xlsx",sheet = 'Jan')
opht_feb <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Ophtalmology_visit.xlsx",sheet = 'Feb')
opht_mar <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Ophtalmology_visit.xlsx",sheet = 'Mar')

 aud_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_ENT_visit.xlsx",sheet = 'Jan')
 aud_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani2026-Jan-Mar_ENT_visit.xlsx",sheet = 'Feb')
 aud_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_ENT_visit.xlsx",sheet = 'Mar')
 aud_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Audiology_visit.xlsx",sheet = 'Jan')
 aud_jan <- readxl::read_excel("C:/HIShareFiles/Interns_repository/Tani/2026-Jan-Mar_Audiology_visit.xlsx",sheet = 'Jan')
 
deent_visits <- deent_file_paths2 |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")


# INT MED SUb BMC

# Get the file paths

intmed_file_paths <- list.files(
  path = "C:/HIShareFiles/INT_MED_visits_extraction/Jan_Mar_visits_dataset/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read all files and combine, then mutate
intmed_visits <- intmed_file_paths |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")



intmed_visits_df <- intmed_visits %>%
  mutate(
    # Remove the suffix from X2
    visit_no = str_remove(X2, "-C$"),
    
    # Fix and parse the date in X3
    visitdate = X3 %>%
      str_replace("([A-Za-z])([0-9]{3})$", "\\12\\2") %>%
      str_replace("([0-9]{2})([A-Za-z]{3})([0-9]{4})", "\\1 \\2 \\3") %>%
      dmy(),
    
  )  %>%
  # 1. Split at the "]" to separate ID from Name
  separate_wider_delim(
    cols = X6,
    delim = "]",
    names = c("patient_id", "name"),
    too_many = "merge",
    too_few = "align_start"
  ) %>%
  # 2. Clean up the results
  mutate(
    # Remove any stray "[" from the ID
    patient_id = str_remove_all(patient_id, "\\["),
    # Trim whitespace from both
    patient_id = trimws(patient_id),
    name = trimws(name)
  ) %>% select( 
    sn=X1,patient_id, name,visit_no, visitdate, consult_type=X4,consult_fee=X5,
    branch_level=X7, sub_bmc_directorate=X8
  ) %>% mutate(sub_bmc_directorate ="Internal Medicine")

write_csv(intmed_visits_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_Int_Med_visit_v01.csv")

psy_visits_df2 <- intmed_visits_df %>% 
  filter(consult_type %in% c("CLINICALPSYCHOLOGICALCONSULT", "CLINICALSYCHOLOGICALONSULT","PSYCHIATRYCONSULT","PSYCHIATRYONSULT" )) %>% 
  distinct(visit_no, .keep_all = T)

write_csv(psy_visits_df2, "Database_Registers/visit_register_merged/2026-Jan-Mar_MentalHealth_visit_v01.csv")






#PAED SUb BMC

# Get the file paths
paed_file_paths <- list.files(
  path = "Database_Registers/Visit_register/paed_visit_register/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read all files and combine, then mutate
paed_visits <- paed_file_paths |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")



paed_visits_df <- paed_visits %>%
  mutate(
    # Remove the suffix from X2
    visit_no = str_remove(X2, "-C$"),
    
    # Fix and parse the date in X3
    visitdate = X3 %>%
      str_replace("([A-Za-z])([0-9]{3})$", "\\12\\2") %>%
      str_replace("([0-9]{2})([A-Za-z]{3})([0-9]{4})", "\\1 \\2 \\3") %>%
      dmy(),
    
  )  %>%
  # 1. Split at the "]" to separate ID from Name
  separate_wider_delim(
    cols = X6,
    delim = "]",
    names = c("patient_id", "name"),
    too_many = "merge",
    too_few = "align_start"
  ) %>%
  # 2. Clean up the results
  mutate(
    # Remove any stray "[" from the ID
    patient_id = str_remove_all(patient_id, "\\["),
    # Trim whitespace from both
    patient_id = trimws(patient_id),
    name = trimws(name)
  ) %>% select( 
    sn=X1,patient_id, name,visit_no, visitdate, consult_type=X4,consult_fee=X5,
    branch_level=X7, sub_bmc_directorate=X8
  )%>% mutate(sub_bmc_directorate ="Paediatrics")

write_csv(paed_visits_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_paed_visit_v01.csv")


#POLY CLINIC SUb BMC

# Get the file paths
polyclinic_file_paths <- list.files(
  path = "Database_Registers/Visit_register/polyclinic_visit_register/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read all files and combine, then mutate
polyclinic_visits <- polyclinic_file_paths |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")



polyclinic_visits_df <- polyclinic_visits %>%
  mutate(
    # Remove the suffix from X2
    visit_no = str_remove(X2, "-C$"),
    
    # Fix and parse the date in X3
    visitdate = X3 %>%
      str_replace("([A-Za-z])([0-9]{3})$", "\\12\\2") %>%
      str_replace("([0-9]{2})([A-Za-z]{3})([0-9]{4})", "\\1 \\2 \\3") %>%
      dmy(),
    
  )  %>%
  # 1. Split at the "]" to separate ID from Name
  separate_wider_delim(
    cols = X6,
    delim = "]",
    names = c("patient_id", "name"),
    too_many = "merge",
    too_few = "align_start"
  ) %>%
  # 2. Clean up the results
  mutate(
    # Remove any stray "[" from the ID
    patient_id = str_remove_all(patient_id, "\\["),
    # Trim whitespace from both
    patient_id = trimws(patient_id),
    name = trimws(name)
  ) %>% select( 
    sn=X1,patient_id, name,visit_no, visitdate, consult_type=X4,consult_fee=X5,
    branch_level=X7, sub_bmc_directorate=X8
  ) %>% drop_na(visitdate)%>% mutate(sub_bmc_directorate ="Poly Clinic")

write_csv(polyclinic_visits_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_polyclinic_visit_v01.csv")




#OBS & GYNEC SUb BMC

# Get the file paths
obs_gynec_file_paths <- list.files(
  path = "Database_Registers/Visit_register/OG_visit_register/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read all files and combine, then mutate
obs_gynec_visits <- obs_gynec_file_paths |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")



obs_gynec_visits_df <- obs_gynec_visits %>%
  mutate(
    # Remove the suffix from X2
    visit_no = str_remove(X2, "-C$"),
    
    # Fix and parse the date in X3
    visitdate = X3 %>%
      str_replace("([A-Za-z])([0-9]{3})$", "\\12\\2") %>%
      str_replace("([0-9]{2})([A-Za-z]{3})([0-9]{4})", "\\1 \\2 \\3") %>%
      dmy(),
    
  )  %>%
  # 1. Split at the "]" to separate ID from Name
  separate_wider_delim(
    cols = X6,
    delim = "]",
    names = c("patient_id", "name"),
    too_many = "merge",
    too_few = "align_start"
  ) %>%
  # 2. Clean up the results
  mutate(
    # Remove any stray "[" from the ID
    patient_id = str_remove_all(patient_id, "\\["),
    # Trim whitespace from both
    patient_id = trimws(patient_id),
    name = trimws(name)
  ) %>% select( 
    sn=X1,patient_id, name,visit_no, visitdate, consult_type=X4,consult_fee=X5,
    branch_level=X7, sub_bmc_directorate=X8
  ) %>% mutate(sub_bmc_directorate ="Obstetrics and Gynaecology")
write_csv(obs_gynec_visits_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_obs_gynec_visit_v01.csv")

############### Appending ALL set

all_visit_df <- bind_rows(
  surg_visit_df, deent_visits_df, intmed_visits_df, paed_visits_df, polyclinic_visits_df, obs_gynec_visits_df
) 

write_csv(all_visit_df, "Database_Registers/visit_register_merged/2026-Jan-Mar_Allvisit_v01.csv")







