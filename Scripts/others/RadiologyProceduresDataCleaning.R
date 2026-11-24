############################################################
# RADIOLOGY DATA QUALITY AUDIT SCRIPT
# Tamale Teaching Hospital
# Author: Asiwome
# Date : 2026/07/19
############################################################


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
library(skimr)


#-------------- Import data Consulting Room Data 
rad0   <- readxl::read_xlsx("Database_Registers/Radiology/RadiologyRegister_01 Jan 2026 00_00_00_to_30 Jun 2026 23_59_59.xlsx") |> clean_names()

# View structure

glimpse(rad)

############################################################
# DATE MANAGEMENT
############################################################

# renaming variables
rad1 <- rad0 |> 
  rename(
    
    pid      = patient_id,
    pname    = patient_name,
    vid      = visitation_id,
    
    sex      = sex,
    age      = age,
    
    pcontact = patient_contact,
    econtact = emergency_contact_no,
    ecname   = emergency_contact_name,
    
    clinician = name_of_requesting_clinician,
    clinician_phone = clinicians_contact_number,
    
    request  = doctors_request,
    source   = source_of_request_ipd_name_of_ward_opd_name_of_clinic,
    
    prov_dx  = provisional_diagnosis,
    princ_dx = principal_diagnosis,
    add_dx1  = additional_diagnosis_old,
    add_dx2  = additional_diagnosis_new,
    
    test     = type_of_investigation,
    dur_s2i = duration_of_testing_starting_from_scheduled_to_imaging_collection,
    dur_i2c = duration_of_testing_starting_from_imaging_collection_to_completed,
    dur_s2c = duration_of_testing_starting_from_scheduled_to_completed,
    
    status   = status,
    validator = name_of_validator,
    
    cost     = cost_of_test
    
  )

rad2 <- rad1 |> 
   mutate(
     request = case_when(
       request =="None" ~ NA,
       TRUE ~request
     ),
     prov_dx = case_when(
       prov_dx =="N/A" ~ NA,
       TRUE ~prov_dx
     ),
     princ_dx = case_when(
       princ_dx =="N/A" ~ NA,
       TRUE ~princ_dx
     ),
     add_dx1 = case_when(
       add_dx1 =="N/A" ~ NA,
       TRUE ~add_dx1
     ),
     add_dx2 = case_when(
       add_dx2 =="N/A" ~ NA,
       TRUE ~add_dx2
     )
   ) |> 
  mutate(
    request = str_squish(request),
    request = str_to_upper(request)
  ) |>
  mutate(
    modality = case_when(
      
      str_detect(request, "^CT")| test=="CT SCAN" ~ "CT",
      
      str_detect(request, "ULTRASOUND|DOPPLER|FETAL|OBSTETRICS|NEONATAL") ~ "Ultrasound",
      
      str_detect(request, "^X-RAYS") ~ "X-Ray",
      
      str_detect(request, "ECG") ~ "ECG",
      
      str_detect(request, "ECHOCARDIOGRAM|TTE") ~ "Echocardiography",
      
      str_detect(request, "ERCP") ~ "ERCP",
      
      TRUE ~ "Other"
    )
  ) |> 
  mutate(
    test_std = case_when(
      # CT
      str_detect(request, "BRAIN|HEAD") ~ "CT Brain",
      
      str_detect(request, "SINUSES") ~ "CT Sinuses",
      
      str_detect(request, "CHEST") ~ "CT Chest",
      
      str_detect(request, "ABDOMEN") &
        str_detect(request, "PELVIS") ~ "CT Abdomen & Pelvis",
      
      str_detect(request, "PELVIS") ~ "CT Pelvis",
      
      str_detect(request, "CERVICAL") ~ "CT Cervical Spine",
      
      str_detect(request, "THORACIC") ~   "CT Thoracic Spine",
      
      str_detect(request, "LUMBER") ~ "CT Lumbar Spine",
      
      str_detect(request, "ANKLE") ~   "CT Ankle",
      
      str_detect(request, "KNEE") ~  "CT Knee",
      
      str_detect(request, "WRIST") ~ "CT Wrist",
      
      str_detect(request, "ELBOW") ~  "CT Elbow",
      
      str_detect(request, "SHOULDER") ~  "CT Shoulder",
      
      str_detect(request, "FOOT") ~ "CT Foot",
      
      str_detect(request, "FEMUR") ~ "CT Femur",
      # USS
      str_detect(request, "ULTRASOUND ABDOMEN & PELVIS") ~ "USS Abdomen & Pelvis",
      
      str_detect(request, "ULTRASOUND ABDOMEN") ~   "USS Abdomen",
      
      str_detect(request, "OBSTETRICS") ~  "USS Obstetric",
      
      str_detect(request, "FETAL") ~  "USS Fetal Assessment",
      
      str_detect(request, "SCROTAL") ~   "USS Scrotal Doppler",
      
      str_detect(request, "THYROID") ~  "USS Thyroid",
      
      str_detect(request, "BREAST") ~ "USS Breast",
      
      str_detect(request, "PROSTRATE|PROSTATE") ~ "USS Prostate",
      
      str_detect(request, "VENOUS") ~  "Venous Doppler",
      
      str_detect(request, "ARTERIES") ~  "Arterial Doppler",
      # X-Ray
      
      request == "X-RAYS CHEST" ~ "X-Ray Chest",
      
      request == "X-RAYS ABDOMEN" ~ "X-Ray Abdomen",
      
      request == "X-RAYS PELVIS" ~ "X-Ray Pelvis",
      
      str_detect(request, "CERVICAL SPINE") ~ "X-Ray Cervical Spine",
      
      str_detect(request, "LUMBER SPINE") ~ "X-Ray Lumbar Spine",
      
      str_detect(request, "THORACIC SPINE") ~ "X-Ray Thoracic Spine",
      
      str_detect(request, "SHOULDER") ~  "X-Ray Shoulder",
      
      str_detect(request, "ELBOW") ~ "X-Ray Elbow",
      
      str_detect(request, "WRIST") ~  "X-Ray Wrist",
      
      str_detect(request, "HAND") ~  "X-Ray Hand",
      
      str_detect(request, "ANKLE") ~  "X-Ray Ankle",
      
      str_detect(request, "KNEE") ~  "X-Ray Knee",
      
      str_detect(request, "FEMUR") ~  "X-Ray Femur",
      
      str_detect(request, "HUMERUS") ~  "X-Ray Humerus",
      
      TRUE ~ NA
      
    ),
    contrast =
      if_else(
        str_detect(request, "CONTRAST"),
        "With Contrast",
        "Without Contrast"
      ),
    request_source  = case_when(
      source %in% c("A&E Orange", "A&E Red","A&E Yellow","ADMISSION VISIT","DEENT Ward",
                    "EMERGENCY","EMERGENCY TRIAGE DETENTION WARD","Family Medicine Female Ward",
                    "Family Medicine Male Ward","General Surgery Ward","Gynaecology A Ward",
                    "Labour Ward","Maternity East Ward (Prenatal)","Maternity Intensive Care Unit (MICU)",
                    "Maternity West Ward (Postnatal)","Medical 1 Ward","Medical 2 Ward","Medical 3 Ward",
                    "Neurosurgery Ward","O&G Emergency","OBSTETRICS THEATRE","PAEDIATRIC BABIES UNIT",
                    "PAEDIATRIC EAST WARD","PAEDIATRIC EMERGENCY","PAEDIATRIC EMERGENCY WARD",
                    "PAEDIATRIC MALNUTRITION WARD","PAEDIATRIC ONCOLOGY UNIT","PAEDIATRIC UNDER 6 WARD",
                    "PAEDIATRIC WEST WARD","POLYCLINIC DETENTION WARD","Plastic / Thoracic Ward",
                    "Urology Ward"
    ) ~ "IPD", TRUE ~"OPD"
  
))

############################################################
# MISSING DATA AUDIT
############################################################

missing_audit <-
  rad2 |> 
  summarise(
    across(
      everything(),
      ~sum(is.na(.))
    )
  ) |> 
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) |> 
  mutate(
    percent_missing =
      round(
        missing_count /
          nrow(rad) * 100,
        2
      )
  ) |> 
  arrange(desc(percent_missing))

print(missing_audit)

# Export

write_csv(
  missing_audit,
  "missing_data_audit.csv"
)

############################################################
# DUPLICATE DETECTION
############################################################

# Potential duplicates based on:
# Patient + Investigation + Schedule Date

duplicate_records <-
  rad2 |> 
  group_by(
    pid,vid,
    request,
    schedule_date
  ) |> 
  filter(n() > 1) |> 
  arrange(pid)

# Create duplicate flag

rad2 <- rad2 |> 
  add_count(
    pid,vid,
    request,
    schedule_date,
    name = "dup_count"
  ) |> 
  mutate(
    duplicate_flag =
      case_when(
        dup_count > 1 ~ "Possible Duplicate",
        TRUE ~ "Unique"
      )
  )

# Duplicate summary

duplicate_summary <-
  rad2 |> 
  count(duplicate_flag)

print(duplicate_summary)

############################################################
# ADVANCED DUPLICATE DETECTION
############################################################

# Same patient, same test within 7 days

possible_repeat_requests <-
  rad2 |> 
  arrange(
    pid,vid,
    request,
    schedule_date
  ) |> 
  group_by(
    pid,vid,
    request,
    schedule_date
  ) |> 
  mutate(
    days_since_previous =
      as.numeric(
        schedule_date -
          lag(schedule_date)
      )
  ) |> 
  filter(
    days_since_previous <= 3
  )

############################################################
# DATE VALIDATION
############################################################

rad2 <- rad2 |> 
  mutate(
    
    completion_before_schedule =
      completed_date < schedule_date,
    
    sample_before_schedule =
      sample_date < schedule_date,
    
    completion_before_sample =
      completed_date < sample_date
    
  )

date_errors <-
  rad2 |> 
  filter(
    completion_before_schedule |
      sample_before_schedule |
      completion_before_sample
  )

############################################################
# STATUS VALIDATION
############################################################

status_completed_no_date <-
  rad2 |> 
  filter(
    status == "Completed",
    is.na(completed_date)
  )

status_not_completed_with_date <-
  rad2 |> 
  filter(
    status != "Completed",
    !is.na(completed_date)
  )

############################################################
# TURNAROUND TIME RECALCULATION
############################################################

rad2 <- rad2 |> 
  mutate(
    # 
    # tat_schedule_to_sample =
    #   as.numeric(
    #     sample_date -
    #       schedule_date
    #   ),
    # 
    # tat_sample_to_complete =
    #   as.numeric(
    #     completed_date -
    #       sample_date
    #   ),
    
    tat_schedule_to_complete =
      as.numeric(
        completed_date -
          schedule_date
      )
    
  )

############################################################
# NEGATIVE DURATION CHECKS
############################################################

negative_duration_records <-
  rad2 |> 
  filter(
    #tat_schedule_to_sample < 0 |
    #  tat_sample_to_complete < 0 |
      tat_schedule_to_complete < 0
  )

############################################################
# VALIDATOR AUDIT
############################################################

validator_audit <-
  rad2 |> 
  count(
    name_of_validator,
    sort = TRUE
  )

missing_validators <-
  rad |> 
  filter(
    is.na(name_of_validator)
  )

############################################################
# COST AUDIT
############################################################

cost_summary <-
  rad |> 
  summarise(
    
    minimum_cost =
      min(
        cost_of_test,
        na.rm = TRUE
      ),
    
    maximum_cost =
      max(
        cost_of_test,
        na.rm = TRUE
      ),
    
    average_cost =
      mean(
        cost_of_test,
        na.rm = TRUE
      )
    
  )

invalid_costs <-
  rad2 |> 
  filter(
    cost <= 0
  )

############################################################
# COMPLETION RATE
############################################################

completion_rate <-
  rad2 |> 
  summarise(
    
    total_requests = n(),
    
    completed =
      sum(
        status == "Completed",
        na.rm = TRUE
      ),
    
    completion_rate =
      round(
        completed /
          total_requests * 100,
        2
      ),
    in_draft =
      sum(
        status == "Draft",
        na.rm = TRUE
      ),
    draft_rate =
      round(
        in_draft /
          total_requests * 100,
        2
      )
  )

print(completion_rate)

############################################################
# WORKLOAD ANALYSIS
############################################################

top_investigations <-
  rad2 |> 
  count(
    modality,
    sort = TRUE
  )

top_request_sources <-
  rad |> 
  count(
    source_of_request,
    sort = TRUE
  )

############################################################
# MONTHLY WORKLOAD
############################################################

monthly_workload <-
  rad |> 
  mutate(
    month =
      floor_date(
        schedule_date,
        "month"
      )
  ) |> 
  count(month)

############################################################
# MONTHLY TREND PLOT
############################################################

monthly_workload |> 
  ggplot(
    aes(
      x = month,
      y = n
    )
  ) +
  geom_line(
    linewidth = 1.2,
    colour = "steelblue"
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Monthly Radiology Requests",
    x = "Month",
    y = "Number of Requests"
  ) +
  theme_minimal()

############################################################
# DATA QUALITY DASHBOARD
############################################################

dqa_summary <-
  tibble(
    
    Indicator = c(
      "Total Records",
      "Possible Duplicates",
      "Missing Completion Date",
      "Completed Without Completion Date",
      "Not Completed But Has Completion Date",
      "Date Errors",
      "Negative Duration Records",
      "Missing Validator",
      "Invalid Costs"
    ),
    
    Value = c(
      
      nrow(rad),
      
      sum(
        rad$duplicate_flag ==
          "Possible Duplicate",
        na.rm = TRUE
      ),
      
      sum(
        is.na(rad$completed_date)
      ),
      
      nrow(
        status_completed_no_date
      ),
      
      nrow(
        status_not_completed_with_date
      ),
      
      nrow(
        date_errors
      ),
      
      nrow(
        negative_duration_records
      ),
      
      nrow(
        missing_validators
      ),
      
      nrow(
        invalid_costs
      )
      
    )
    
  )

print(dqa_summary)

############################################################
# EXPORT AUDIT FILES
############################################################

write_csv(
  duplicate_records,
  "duplicate_records.csv"
)

write_csv(
  date_errors,
  "date_errors.csv"
)

write_csv(
  negative_duration_records,
  "negative_duration_records.csv"
)

write_csv(
  dqa_summary,
  "radiology_dqa_summary.csv"
)

############################################################
# END OF SCRIPT
############################################################