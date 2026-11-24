## TTH Surgery SubBMC Monthly Reports ----
#  Data Wrangling ---- IPD 
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




## Importing IPD data sets 2025, JAN-MAR
ipd0 <- readxl::read_xlsx("Database_Registers/Data_Warehouse/IPD_AD_register/IPD_Addm_PEADEAST.xlsx")

tranIn0 <- readxl::read_xlsx("Database_Registers/Data_Warehouse/IPD_Transfer_register/PAEDTRANSIN.xlsx")
tranIn1 <- tranIn0 |>  mutate(fward = stringr::str_squish(`From Ward`)) |> 
  filter(`From Ward` %in% c("PAEDIATRIC EMERGENCY WARD", "Plastic / Thoracic Ward",
                                                 "PAEDIATRIC ORTHOPAEDICS WARD"))
 
ipdReg1 <- readxl::read_xlsx("Database_Registers/IPD_register/InpatientMorbidityRegisterJANJUN.xlsx")
tranIn1 <- tranIn1 |>  
  mutate(
    admission_date = as_date(`Admission Date`),
    admission_month = lubridate::month(admission_date),
    admission_month_w = lubridate::month(admission_date, label = TRUE)
  )  |> distinct(`Visit No`, .keep_all = T)


# Cleaning variables and creating new ones
# 
ipd <- ipd0 |>  
  mutate(
    admission_date = as_date(`Admission Date`),
    admission_month = lubridate::month(admission_date),
    admission_month_w = lubridate::month(admission_date, label = TRUE),
    )  |> distinct(`Visit No`, .keep_all = T)


ad_tab <- tabyl(ipd, admission_month_w)










admission_day = day(admission_date),
    admission_day_week = lubridate::wday(admission_date, label = T),
    discharge_date = dmy(`Date of Discharge`),
    discharge_month = lubridate::month(discharge_date),
    discharge_month_w = lubridate::month(discharge_date, label = T),
    discharge_day = day(discharge_date),
    discharge_day_week = lubridate::wday(discharge_date, label = T),
    clinic_unit = stringr::str_to_title(`Ward/Room`),
    clinic_sub = case_when(
      clinic_unit %in% 
        c("A/E Clinical Decision Unit", "A/E Orange",
          "A/E Orange","A/E Red",  "A/E Yellow"
        ) ~ "Accident & Emergency",
      clinic_unit %in% 
        c("Female General Surgical Ward", "Female Neurosurgery Ward","Female Paed Surgery",
          "Female Urology Ward", "General Surgery Side Ward", "Icu Vip","Intensive Care Unit (Icu)",
          "Main Theatre Recovery", "Male General Surgical Ward","Male Neurosurgery Ward","Male Paed Surgery",
          "Male Urology Ward","Paeditrics Urology Ward","Plastic Surgery Ward","Thoracic Surgery Ward",
          "Urology Staff Ward", "Urology Theatre Recovery"
        ) ~ "Surgery",
      clinic_unit %in%
        c("Dental / Maxillofacial Ward", "Ent, Otolaryngology Ward", "Eye (Ophthalmology)") ~ "DEENT"
    ),
    clinic_ward = case_when(
      clinic_unit %in% c("Female General Surgical Ward","General Surgery Side Ward","Male General Surgical Ward")~"General Surgery Ward",
      clinic_unit %in% c("Female Paed Surgery","Male Paed Surgery")~"Paediatric Surgery Ward",
      clinic_unit %in% c("Female Neurosurgery Ward", "Male Neurosurgery Ward")~"Neurosurgery Ward",
      clinic_unit %in% c("Female Urology Ward", "Male Urology Ward", "Paeditrics Urology Ward", "Urology Staff Ward")~"Urology Ward",
      clinic_unit %in% c("Icu Vip", "Intensive Care Unit (Icu)")~"Intensive Care Unit Ward",
      clinic_unit %in% c("Plastic Surgery Ward")~"Plastics Ward",
      clinic_unit %in% c("Thoracic Surgery Ward")~"Thoracic Ward",
      clinic_unit %in% c("Main Theatre Recovery", "Urology Theatre Recovery")~"Theatre Recovery",
      clinic_unit %in% c("Dental / Maxillofacial Ward")~"Maxillofacial Ward",
      clinic_unit %in% c("Ent, Otolaryngology Ward")~"ENT Ward",
      clinic_unit %in% c("Eye (Ophthalmology)")~"EYE Ward"
    ),
    discharge_outcome = `Outcome of Discharge`,
    length_of_stay = as.numeric((discharge_date-admission_date)),
    length_of_stay = ifelse(length_of_stay<1, 1,length_of_stay),
    los = ifelse(length_of_stay >90, 60,length_of_stay)
  )|>  
  mutate( ## Extracting ICD10-codes
    dx1 = str_extract_all(`Principal Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
    dx2 = str_extract_all(`Principal Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
    dx3 = str_extract_all(`Principal Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
    dx4 = str_extract_all(`Principal Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
    dx5 = str_extract_all(`Principal Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,5],
    dx6 = str_extract_all(`Additional Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,1],
    dx7 = str_extract_all(`Additional Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,2],
    dx8 = str_extract_all(`Additional Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,3],
    dx9 = str_extract_all(`Additional Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,4],
    dx10 = str_extract_all(`Additional Diagnosis`,"([A-TV-Z][0-9][A-Z0-9](\\.?[A-Z0-9]{0,4})?)", simplify = TRUE)[,5]
  ) |>  
  mutate(
    across(starts_with("dx"), ~na_if(., ""))
  )|> filter(
    !is.na(dx1) | !is.na(dx2) | !is.na(dx3) & !is.na(dx4) | !is.na(dx5) | !is.na(dx6) | !is.na(dx7)|!is.na(dx8) | !is.na(dx9)|!is.na(dx10)
  )

ipdreg <- write_rds(ipd, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025-JAN-SEP-TTH-IPD-MIS-ANALYTIC-DATASET.rds")

ipd2 <- ipd |> 
  pivot_longer(
    cols = c(dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10),
    names_to = "diagnosis_type",
    values_to = "diagnosis"
  ) |> 
  mutate(
    pid = paste(`Patient No.`,clinic_ward, discharge_date, diagnosis, sep="_")
  ) |> 
  distinct(pid, .keep_all = TRUE) |> 
  # dropping No Code/ Diagnoses codes
  drop_na(diagnosis) |> 
  mutate(
    diagnosis = gsub("\\.", "", diagnosis)
  )

combined_regex <- "^[A-Z]{1}[0-9]{2}[A-Z0-9]{0,4}(\\.[A-Z0-9]{1,4})?$"

# Validate mixed codes
df <- ipd2 %>%
  mutate(
    valid_code = grepl(combined_regex, diagnosis),
    code = diagnosis
  ) |> filter(valid_code=="TRUE") |> 
  mutate(code = recode(code,"J3009"="J309")) |>
  mutate(code = recode(code,"N4001"="N400")) |>
  mutate(code = recode(code,"J304"="J309")) |>
  mutate(code = recode(code,"N6101"="N611")) |>
  mutate(code = recode(code,"N3514"="N35014")) |>
  mutate(code = recode(code,"K028"="K029")) |>
  mutate(code = recode(code,"G551"="G55")) |>
  mutate(code = recode(code,"H2519"="H25019")) |>
  mutate(code = recode(code,"M4862"="M48062")) |>
  mutate(code = recode(code,"E0808"="E088")) |>
  mutate(code = recode(code,"T814XXD"="T8149XD")) |>
  mutate(code = recode(code,"D2409"="D249")) |>
  mutate(code = recode(code,"D3730"="D37030")) |>
  mutate(code = recode(code,"H131"="B309")) |>
  mutate(code = recode(code,"H4059"="H40059")) |>
  mutate(code = recode(code,"I64"="I639")) |>
  mutate(code = recode(code,"K6401"="K641")) |>
  mutate(code = recode(code,"M490"="A1801")) |>
  mutate(code = recode(code,"M840"="S6290XP")) |>
  mutate(code = recode(code,"A099"="A09")) |>
  mutate(code = recode(code,"C857"="C858")) |>
  mutate(code = recode(code,"D4903"="D493")) |>
  mutate(code = recode(code,"G406"="G40309")) |>
  mutate(code = recode(code,"H1161"="H11061")) |>
  mutate(code = recode(code,"H1652"="H16042")) |>
  mutate(code = recode(code,"H2593"="H25099")) |>
  mutate(code = recode(code,"H4009"="H40009")) |>
  mutate(code = recode(code,"H6604"="H66001")) |>
  mutate(code = recode(code,"K021"="K029")) |>
  mutate(code = recode(code,"K291"="K296")) |>
  mutate(code = recode(code,"K6404"="K644")) |>
  mutate(code = recode(code,"L0319"="L03019")) |>
  mutate(code = recode(code,"L7203"="L723")) |>
  mutate(code = recode(code,"N3513"="N35013")) |>
  mutate(code = recode(code,"N4503"="N453")) |>
  mutate(code = recode(code,"T814XXA"="T8140XA")) |>
  mutate(code = recode(code,"A090"="J398")) |>
  mutate(code = recode(code,"B980"="B9681")) |>
  mutate(code = recode(code,"D3732"="D37032")) |>
  mutate(code = recode(code,"D6301"="D631")) |>
  mutate(code = recode(code,"F8004"="F804")) |>
  mutate(code = recode(code,"G552"="G55")) |>
  mutate(code = recode(code,"H0104"="H01004")) |>
  mutate(code = recode(code,"H0519"="H05019")) |>
  mutate(code = recode(code,"H1109"="H11009")) |>
  mutate(code = recode(code,"H1669"="H16069")) |>
  mutate(code = recode(code,"H2543"="H25043")) |>
  mutate(code = recode(code,"H288"="H279")) |>
  mutate(code = recode(code,"H4069"="H40069")) |>
  mutate(code = recode(code,"H5323"="H53023")) |>
  mutate(code = recode(code,"K076"="M2660")) |>
  mutate(code = recode(code,"K6402"="K642")) |>
  mutate(code = recode(code,"N3510"="N35014")) |>
  mutate(code = recode(code,"N3528"="N35028")) |>
  mutate(code = recode(code,"N4501"="N451")) |>
  mutate(code = recode(code,"N4609"="N469")) |>
  mutate(code = recode(code,"R1009"="R100")) |>
  mutate(code = recode(code,"S727"="S728X2A")) |>
  mutate(code = recode(code,"S827"="S8290XA")) |>
  mutate(code = recode(code,"T793"="T8149XA")) |>
  mutate(code = recode(code,"T936"="S88919S")) |>
  mutate(code = recode(code,"Z4702"="Z472")) |>
  mutate(code = recode(code,"C2208"="C228")) |>
  mutate(code = recode(code,"C5019"="C50119")) |>
  mutate(code = recode(code,"F072"="F0781")) |>
  mutate(code = recode(code,"G4309"="G43009")) |>
  mutate(code = recode(code,"G530"="G500")) |>
  mutate(code = recode(code,"G636"="G63")) |>
  mutate(code = recode(code,"H1029"="H10029")) |>
  mutate(code = recode(code,"H1152"="H11052")) |>
  mutate(code = recode(code,"H1153"="H11053")) |>
  mutate(code = recode(code,"H2539"="H25039")) |>
  mutate(code = recode(code,"H2549"="H25049")) |>
  mutate(code = recode(code,"H3043"="H30043")) |>
  mutate(code = recode(code,"H3119"="H31019")) |>
  mutate(code = recode(code,"H3192"="H31092")) |>
  mutate(code = recode(code,"H3308"="H338")) |>
  mutate(code = recode(code,"H4409"="H44009")) |>
  mutate(code = recode(code,"H4662"="H462")) |>
  mutate(code = recode(code,"H5329"="H53029")) |>
  mutate(code = recode(code,"H5939"="H59039")) |>
  mutate(code = recode(code,"H6603"="H66003")) |>
  mutate(code = recode(code,"H6609"="H66009")) |>
  mutate(code = recode(code,"H6702"="H672")) |>
  mutate(code = recode(code,"H6803"="H68003")) |>
  mutate(code = recode(code,"I842"="K649")) |>
  mutate(code = recode(code,"I848"="K649")) |>
  mutate(code = recode(code,"J303"="J3089")) |>
  mutate(code = recode(code,"J3409"="J349")) |>
  mutate(code = recode(code,"K0207"="K027")) |>
  mutate(code = recode(code,"K0622"="K06022")) |>
  mutate(code = recode(code,"K079"="M269")) |>
  mutate(code = recode(code,"K4305"="K435")) |>
  mutate(code = recode(code,"K6403"="K643")) |>
  mutate(code = recode(code,"K6408"="K649")) |>
  mutate(code = recode(code,"K914"="K9403")) |>
  mutate(code = recode(code,"M2159"="M21059")) |>
  mutate(code = recode(code,"M688"="M67")) |>
  mutate(code = recode(code,"M841"="M960")) |>
  mutate(code = recode(code,"M8751"="M87051")) |>
  mutate(code = recode(code,"M900"="A180")) |>
  mutate(code = recode(code,"M9312"="M93012")) |>
  mutate(code = recode(code,"N5003"="N503")) |>
  mutate(code = recode(code,"N5209"="N529")) |>
  mutate(code = recode(code,"R0709"="R079")) |>
  mutate(code = recode(code,"R074"="R079")) |>
  mutate(code = recode(code,"R1502"="R152")) |>
  mutate(code = recode(code,"R2509"="R259")) |>
  mutate(code = recode(code,"R2609"="R269")) |>
  mutate(code = recode(code,"R3109"="R319")) |>
  mutate(code = recode(code,"R3309"="R339")) |>
  mutate(code = recode(code,"R3609"="R369")) |>
  mutate(code = recode(code,"R529"="R52")) |>
  mutate(code = recode(code,"R569"="R5609")) |>
  mutate(code = recode(code,"S027"="S028")) |>
  mutate(code = recode(code,"S682"="S68")) |>
  mutate(code = recode(code,"T12"="S82")) |>
  mutate(code = recode(code,"T292"="T300")) |>
  mutate(code = recode(code,"T931"="S7292XS")) |>
  mutate(code = recode(code,"Y658"="Y618")) |>
  mutate(code = recode(code,"Z108"="Z008")) |>
  mutate(code = recode(code,"Z470"="Z472")) |>
  mutate(code = recode(code,"Z470"="Z472")) |>
  mutate(code = recode(code,"Z4701"="Z471")) |>
  mutate(code = recode(code,"Z489"="Z471")) |>
  mutate(code = recode(code,"Z86010"="Z8610")) |> 
  mutate(code = recode(code,"K4302"="K432")) |> 
  mutate(code = recode(code,"M2169"="M21069")) |> 
  mutate(code = recode(code,"M2169"="M21069")) |> 
  mutate(code = recode(code,"H6602"="H66002")) |> 
  mutate(code = recode(code,"I849"="K649")) |> 
  mutate(code = recode(code,"R5609"="R569")) |> 
  mutate(code = recode(code,"Y618"="Y638")) |> 
  mutate(code = recode(code,"Z8610"="Z86010")) |> 
  mutate(code = recode(code,"C151"="C155")) |>
  mutate(code = recode(code,"H5407"="H547")) |>
  mutate(code = recode(code,"T8709"="T879")) |> 
  mutate(code = recode(code,"C6502"="C652")) |> 
  mutate(code = recode(code,"G433"="G43801")) |> 
  mutate(code = recode(code,"M2169"="M21069")) |> 
  mutate(code = recode(code,"H6602"="H66002")) |> 
  mutate(code = recode(code,"H20A"="")) |> 
  mutate(code = recode(code,"M2702"="M272")) |> 
  mutate(code = recode(code,"N5208"="N528")) |>
  mutate(code = recode(code,"R9208"="R928")) |>
  mutate(code = recode(code,"C97"="C768")) |> 
  mutate(code = recode(code,"F83"="F88")) |> 
  mutate(code = recode(code,"G820"="G8220")) |> 
  mutate(code = recode(code,"M2169"="M21069")) |> 
  mutate(code = recode(code,"H0103"="H01003")) |> 
  mutate(code = recode(code,"H0106"="H01006")) |> 
  mutate(code = recode(code,"H0409"="H04009")) |> 
  mutate(code = recode(code,"H1609"="H16009")) |>
  mutate(code = recode(code,"H2051"="H20051")) |>
  mutate(code = recode(code,"H2602"="H26002")) |> 
  mutate(code = recode(code,"H3029"="H30029")) |> 
  mutate(code = recode(code,"H3309"="H33009")) |> 
  mutate(code = recode(code,"H335"="H338")) |> 
  mutate(code = recode(code,"H450"="H4310")) |> 
  mutate(code = recode(code,"H4791"="H47091")) |> 
  mutate(code = recode(code,"I01A"="")) |> 
  mutate(code = recode(code,"I32A"="")) |>
  mutate(code = recode(code,"I35A"="")) |>
  mutate(code = recode(code,"I8712"="I87012")) |> 
  mutate(code = recode(code,"J068"="J06")) |> 
  mutate(code = recode(code,"J178"="J17")) |> 
  mutate(code = recode(code,"J9108"="J918")) |> 
  mutate(code = recode(code,"K020"="K029")) |> 
  mutate(code = recode(code,"K5802"="K580")) |> 
  mutate(code = recode(code,"K5808"="K580")) |> 
  mutate(code = recode(code,"M11A"="")) |>
  mutate(code = recode(code,"M1919"="M19019")) |>
  mutate(code = recode(code,"M819"="M869")) |> 
  mutate(code = recode(code,"N4003"="N403")) |> 
  mutate(code = recode(code,"R08A"="")) |> 
  mutate(code = recode(code,"R104"="")) |> 
  mutate(code = recode(code,"R10A"="")) |> 
  mutate(code = recode(code,"R3508"="R358")) |> 
  mutate(code = recode(code,"R4709"="R4702")) |> 
  mutate(code = recode(code,"R9101"="R911")) |>
  mutate(code = recode(code,"T911"="S32009A")) |>
  mutate(code = recode(code,"Y01A"="")) |> 
  mutate(code = recode(code,"Z019"="Z0189")) |> 
  mutate(code = recode(code,"H6703"="H673")) |> 
  mutate(code = recode(code,"I843"="K645")) |> 
  mutate(code = recode(code,"N4000"="N400")) |> 
  mutate(code = recode(code,"K6501"="K650")) |>
  mutate(code = recode(code,"R5300"="R530")) |>
  mutate(code = recode(code,"R20A"="")) |>
  mutate(code = recode(code,"R1500"="R150")) |>
  mutate(code = recode(code,"N4608"="N46029")) |>
  mutate(code = recode(code,"N2602"="N262")) |>
  mutate(code = recode(code,"K6400"="K640")) |> 
  mutate(code = recode(code,"K3600"="K36")) |>
  mutate(code = recode(code,"I12A"="")) |>
  mutate(code = recode(code,"G942"="G942")) |>
  mutate(code = recode(code,"G9104"="G910")) |>
  mutate(code = recode(code,"G8904"="G890")) |>
  mutate(code = recode(code,"G407"="G408")) |>
  mutate(code = recode(code,"E4300"="E43")) |>
  mutate(code = recode(code,"T814XXS"="T8140XA")) |>
  mutate(code = recode(code,"R02"="I96")) |> 
  mutate(code = recode(code,"N510"="N428")) |>
  mutate(code = recode(code,"N6100"="N610")) |>
  mutate(code = recode(code,"I1000"="I10")) |>
  mutate(code = recode(code,"D2400"="D249")) |>
  mutate(code = recode(code,"I845"="K649")) |>
  mutate(code = recode(code,"C6100"="C61")) |> 
  mutate(code = recode(code,"A152"="A150")) |> 
  mutate(code = recode(code,"B5400"="B54")) |> 
  mutate(code = recode(code,"G942"="G914")) |> 
  mutate(code = recode(code,"I500"="I502")) |> 
  mutate(code = recode(code,"Z039"="Z03")) |> 
  mutate(code = recode(code,"D3400"="D34"))|> 
  mutate(code = recode(code,"N4600"="N4601")) |> 
  mutate(code = recode(code,"C7300"="C73")) |> 
  mutate(code = recode(code,"K3700"="K37")) |> 
  mutate(code = recode(code,"R1700"="R17")) |> 
  mutate(code = recode(code,"A153"="A150")) |> 
  mutate(code = recode(code,"I840"="K648")) |> 
  mutate(code = recode(code,"I844"="K648")) |> 
  mutate(code = recode(code,"J2200"="J22")) |> 
  mutate(code = recode(code,"N4002"="N402")) |> 
  mutate(code = recode(code,"N4502"="N452")) |> 
  mutate(code = recode(code,"N459"="N453")) |> 
  mutate(code = recode(code,"N6200"="N62")) |> 
  mutate(code = recode(code,"N6300"="N630")) |> 
  mutate(code = recode(code,"P3809"="P389")) |> 
  mutate(code = recode(code,"R2100"="R21")) |> 
  mutate(code = recode(code,"R3308"="R338")) |> 
  mutate(code = recode(code,"R629"="R625")) |> 
  mutate(code = recode(code,"W80"="T1789")) |> 
  mutate(code = recode(code,"Z090"="Z488")) |> 
  mutate(code = recode(code,"B0700"="B07")) |>
  mutate(code = recode(code,"B0709"="B079")) |>
  mutate(code = recode(code,"B212"="B20")) |>
  mutate(code = recode(code,"B4900"="B49")) |> 
  mutate(code = recode(code,"C2000"="C20")) |>
  mutate(code = recode(code,"C5600"="C56")) |>
  mutate(code = recode(code,"C5800"="C58")) |>
  mutate(code = recode(code,"C6400"="C64")) |>
  mutate(code = recode(code,"C968"="C96A")) |>
  mutate(code = recode(code,"D057"="D0580")) |>
  mutate(code = recode(code,"D307"="D308")) |>
  mutate(code = recode(code,"D6308"="D630")) |>
  mutate(code = recode(code,"E0200"="E02")) |> 
  mutate(code = recode(code,"E149"="")) |>
  mutate(code = recode(code,"E94D"="")) |>
  mutate(code = recode(code,"F811"="F81")) |>
  mutate(code = recode(code,"F8300"="F89")) |>
  mutate(code = recode(code,"G4419"="G44001")) |>
  mutate(code = recode(code,"I31A"="")) |> 
  mutate(code = recode(code,"I4300"="I43")) |> 
  mutate(code = recode(code,"I6400"="I60")) |> 
  mutate(code = recode(code,"I694"="I60")) |> 
  mutate(code = recode(code,"I841"="")) |> 
  mutate(code = recode(code,"I9600"="I96")) |> 
  mutate(code = recode(code,"J0000"="J00"))|> 
  mutate(code = recode(code,"J1300"="J13")) |> 
  mutate(code = recode(code,"J9000"="J90")) |> 
  mutate(code = recode(code,"K3000"="K30")) |> 
  mutate(code = recode(code,"K4306"="K436")) |> 
  mutate(code = recode(code,"L9700"="L97")) |> 
  mutate(code = recode(code,"M4861"="M48061")) |> 
  mutate(code = recode(code,"M671"="M67")) |> 
  mutate(code = recode(code,"M7162"="M7106")) |> 
  mutate(code = recode(code,"N2300"="N23")) |> 
  mutate(code = recode(code,"N450"="N45")) |> 
  mutate(code = recode(code,"N4504"="N454")) |> 
  mutate(code = recode(code,"N6200"="N62")) |> 
  mutate(code = recode(code,"N6300"="N630")) |> 
  mutate(code = recode(code,"P3809"="P389")) |> 
  mutate(code = recode(code,"R2100"="R21")) |> 
  mutate(code = recode(code,"R3308"="R338")) |> 
  mutate(code = recode(code,"R629"="R625")) |> 
  mutate(code = recode(code,"W80"="T1789")) |> 
  mutate(code = recode(code,"Z090"="Z488")) |> 
  mutate(code = recode(code,"N4703"="N473")) |> 
  mutate(code = recode(code,"N4903"="N493")) |> 
  mutate(code = recode(code,"N7200"="N72")) |> 
  mutate(code = recode(code,"N851"="N850")) |> 
  mutate(code = recode(code,"P219"="")) |> 
  mutate(code = recode(code,"R1300"="R0901")) |> 
  mutate(code = recode(code,"R3300"="R330")) |> 
  mutate(code = recode(code,"R3400"="R34")) |> 
  mutate(code = recode(code,"R4200"="R42")) |> 
  mutate(code = recode(code,"R5500"="R55")) |> 
  mutate(code = recode(code,"S327"="S32")) |> 
  mutate(code = recode(code,"T302"="T30")) |> 
  mutate(code = recode(code,"N490"="N493")) |> 
  mutate(code = recode(code,"R3300"="R33")) |> 
  mutate(code = recode(code,"Z8528"="Z8502")) |> 
  mutate(code = recode(code,"C150"="C15")) |>
  mutate(code = recode(code,"C152"="C15")) |> 
  mutate(code = recode(code,"C2300"="C23")) |> 
  mutate(code = recode(code,"D4904"="D494")) |> 
  mutate(code = recode(code,"E117"="E11")) |> 
  mutate(code = recode(code,"E140"="")) |> 
  mutate(code = recode(code,"G4009"="G400")) |> 
  mutate(code = recode(code,"H24A"="")) |> 
  mutate(code = recode(code,"I846"="")) |> 
  filter(code !="")

dat_icd10cm <-left_join(df, icdver, 
                        by="code") |> #making the arg explicit 
  rename(
    disease_name=major, disease_type=sub_chapter, disease_class=chapter
  ) |> 
  mutate(
    disease_name = as.character(disease_name),
    disease_type = as.character(disease_type),
    disease_class = as.character(disease_class)
  )

ipdWONA <- dat_icd10cm |> filter(!is.na(disease_name)) |> view()
ipdNA <- dat_icd10cm |> filter(is.na(disease_name)) 

ipdNA$truncated_codes <- substr(ipdNA$code, 1, 3)

ipdNA <- ipdNA |> 
  mutate(
    code = ifelse(is.na(disease_name), truncated_codes, code)
  ) |> 
  select(-c(short_desc,long_desc,three_digit,disease_name,disease_type,disease_class))


ipdcm2 <-left_join(ipdNA, icdver, 
                   by="code") |> #making the arg explicit 
  rename(
    disease_name=major, disease_type=sub_chapter, disease_class=chapter
  ) |> 
  mutate(
    disease_name = as.character(disease_name),
    disease_type = as.character(disease_type),
    disease_class = as.character(disease_class)
  ) |> drop_na(disease_name)

final_ipddf <- bind_rows(ipdcm2,ipdWONA)

pidoc <- read_rds("DATABASE/IPD CONDITIONS/MIS/MIS IPD RDATA-RAW/2025-OCT-DEC_SURGERY-IPD-MORBIDITY-DATA-WITH-ICD10-MATCHED.rds")
final_ipddf3 <- bind_rows(final_ipddf, pidoc) |> 
  mutate(
    clinic_ward = case_when(
      clinic_ward =="General Surgery Ward" | ward_room =="general surgery" ~"General Surgery Ward",
      clinic_ward =="Intensive Care Unit Ward" | ward_room =="ICU" ~"Intensive Care Unit Ward",
      clinic_ward =="Neurosurgery Ward" | ward_room =="Neurosurgery" ~"Neurosurgery Ward",
      clinic_ward =="Paediatric Surgery Ward" | ward_room =="Paediatric Surgery" ~"Paediatric Surgery Ward",
      clinic_ward =="Plastics Ward" | ward_room =="plastics" ~"Plastics Ward",
      clinic_ward =="Paediatrics Surgery" | ward_room =="Paediatric Surgery" ~"Paediatrics Surgery",
      clinic_ward =="Thoracic Ward" | ward_room =="thoracic" ~"Thoracic Ward",
      clinic_ward =="Urology Ward" | ward_room =="Urology" ~"Urology Ward"
    )
  ) |> drop_na(clinic_ward)
# ALL DATASET
write_csv(final_ipddf3, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025-JAN-DEC_IPD-MORBIDITY-DATA-WITH-ICD10-MATCHED.csv")
write_rds(final_ipddf3, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025-JAN-DEC_IPD-MORBIDITY-DATA-WITH-ICD10-MATCHED.rds")


### Patient days dataset
patientdays <- final_ipddf |> 
  filter(clinic_sub=="Surgery") |> 
  dplyr::group_by(discharge_month_w, clinic_ward) |> 
  dplyr::summarise(pat_days =sum(los),.groups = "drop")

patday_jan <-  patientdays |>
  dplyr::filter(discharge_month_w=="Jan") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_jan=`General Surgery Ward`,
    pead_surg_jan=`Paediatric Surgery Ward`,
    icu_jan=`Intensive Care Unit Ward`, 
    plastic_jan=`Plastics Ward`,
    neurosurgery_jan=`Neurosurgery Ward`,
    thoracic_jan=`Thoracic Ward`,
    urology_jan=`Urology Ward`,
    surg_jan=total
  )
write_rds(patday_jan, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_jan_patient_days_v01.rds") 

patday_feb <-  patientdays |>
  dplyr::filter(discharge_month_w=="Feb") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_feb=`General Surgery Ward`,
    pead_surg_feb=`Paediatric Surgery Ward`,
    icu_feb=`Intensive Care Unit Ward`, 
    plastic_feb=`Plastics Ward`,
    neurosurgery_feb=`Neurosurgery Ward`,
    thoracic_feb=`Thoracic Ward`,
    urology_feb=`Urology Ward`,
    surg_feb=total
  )

write_rds(patday_feb, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_feb_patient_days.rds") 


patday_mar <-  patientdays |>
  dplyr::filter(discharge_month_w=="Mar") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_mar=`General Surgery Ward`,
    pead_surg_mar=`Paediatric Surgery Ward`,
    icu_mar=`Intensive Care Unit Ward`, 
    plastic_mar=`Plastics Ward`,
    neurosurgery_mar=`Neurosurgery Ward`,
    thoracic_mar=`Thoracic Ward`,
    urology_mar=`Urology Ward`,
    surg_mar=total
  )
write_rds(patday_mar, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_mar_patient_days.rds")

patday_apr <-  patientdays |>
  dplyr::filter(discharge_month_w=="Apr") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_apr=`General Surgery Ward`,
    pead_surg_apr=`Paediatric Surgery Ward`,
    icu_apr=`Intensive Care Unit Ward`, 
    plastic_apr=`Plastics Ward`,
    neurosurgery_apr=`Neurosurgery Ward`,
    thoracic_apr=`Thoracic Ward`,
    urology_apr=`Urology Ward`,
    surg_apr=total
  )
write_rds(patday_apr, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_apr_patient_days.rds")


patday_may <-  patientdays |>
  dplyr::filter(discharge_month_w=="May") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_may=`General Surgery Ward`,
    pead_surg_may=`Paediatric Surgery Ward`,
    icu_may=`Intensive Care Unit Ward`, 
    plastic_may=`Plastics Ward`,
    neurosurgery_may=`Neurosurgery Ward`,
    thoracic_may=`Thoracic Ward`,
    urology_may=`Urology Ward`,
    surg_may=total
  )
write_rds(patday_may, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_may_patient_days.rds")


patday_jun <-  patientdays |>
  dplyr::filter(discharge_month_w=="Jun") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_jun=`General Surgery Ward`,
    pead_surg_jun=`Paediatric Surgery Ward`,
    icu_jun=`Intensive Care Unit Ward`, 
    plastic_jun=`Plastics Ward`,
    neurosurgery_jun=`Neurosurgery Ward`,
    thoracic_jun=`Thoracic Ward`,
    urology_jun=`Urology Ward`,
    surg_jun=total
  )
write_rds(patday_jun, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_jun_patient_days.rds")


patday_jul <-  patientdays |>
  dplyr::filter(discharge_month_w=="Jul") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_jul=`General Surgery Ward`,
    pead_surg_jul=`Paediatric Surgery Ward`,
    icu_jul=`Intensive Care Unit Ward`, 
    plastic_jul=`Plastics Ward`,
    neurosurgery_jul=`Neurosurgery Ward`,
    thoracic_jul=`Thoracic Ward`,
    urology_jul=`Urology Ward`,
    surg_jul=total
  )
write_rds(patday_jul, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_jul_patient_days.rds")

patday_aug <-  patientdays |>
  dplyr::filter(discharge_month_w=="Aug") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_aug =`General Surgery Ward`,
    pead_surg_aug=`Paediatric Surgery Ward`,
    icu_aug =`Intensive Care Unit Ward`, 
    plastic_aug=`Plastics Ward`,
    neurosurgery_aug=`Neurosurgery Ward`,
    thoracic_aug=`Thoracic Ward`,
    urology_aug=`Urology Ward`,
    surg_aug=total
  )
write_rds(patday_aug, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_aug_patient_days.rds")

patday_sep <-  patientdays |>
  dplyr::filter(discharge_month_w=="Sep") |> 
  dplyr::select(clinic_ward, pat_days) |> 
  pivot_wider(names_from =clinic_ward, values_from = pat_days) |> 
  dplyr::mutate(total=(`Intensive Care Unit Ward`+`Neurosurgery Ward`+`General Surgery Ward`+ `Paediatric Surgery Ward`+`Urology Ward`+`Plastics Ward`+`Thoracic Ward`),
                Indicators="Patient days") |> 
  dplyr::select(
    Indicators,
    general_surg_sep=`General Surgery Ward`,
    pead_surg_sep=`Paediatric Surgery Ward`,
    icu_sep=`Intensive Care Unit Ward`, 
    plastic_sep=`Plastics Ward`,
    neurosurgery_sep=`Neurosurgery Ward`,
    thoracic_sep=`Thoracic Ward`,
    urology_sep=`Urology Ward`,
    surg_sep=total
  )


write_rds(patday_sep, "DATABASE/IPD CONDITIONS/MIS/MIS IPD ANALYTIC-DATASET/2025_sep_patient_days.rds")




















