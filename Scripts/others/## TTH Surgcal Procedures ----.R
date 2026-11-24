## TTH Surgcal Procedures ----
#  Data Wrangling ---- 
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
jan_surg01<- readxl::read_xlsx("../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/Share2HI_repo/03_Datasets/Raw_Submissions/Surgical-Procedures/2026-MAIN-THEATRE-SURGERY-REGISTER.xlsx")

icd109   <- readxl::read_xlsx("../OneDrive - Smart Workplace/Documents/2026 TTH DATA REPORTS/DataFiles/2019-icd10-cm.xlsx") |> clean_names() |> 
  mutate(icd10_code= code)

