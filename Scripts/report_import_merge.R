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

# ---- importing data by clinics

# SURGERY SUB BMC

ipdjan <- read_csv(
  "Database_Registers/IPD_register/2026-JAN-IPD_REGISTER_v01.csv"
) |>
  mutate(across(everything(), as.character))
ipdfeb <- read_csv(
  "Database_Registers/IPD_register/2026-FEB-IPD_REGISTER_v01.csv"
) |>
  mutate(across(everything(), as.character))
ipdmar <- read_csv(
  "Database_Registers/IPD_register/2026-MAR-IPD_REGISTER_v01.csv"
) |>
  mutate(across(everything(), as.character))


allipd <- bind_rows(ipdjan, ipdfeb, ipdmar) |>
  filter(Ward == "Plastic / Thoracic Ward")

writexl::write_xlsx(allipd, "Plastics_Thoracic_IPD_Register_2026-JAN-MAR.xlsx")


ana_df1 <- list.files(
  path = "Database_Registers/Visit_register/",
  pattern = "\\.csv$",
  full.names = TRUE
)

data_all <- ana_df1 |>
  map_dfr(read_csv, clean_names) |>
  mutate(across(where(is.character), ~ gsub("[\r\n]", "", .x))) |>
  filter(X1 != "No.")
