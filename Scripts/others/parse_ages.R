library(dplyr)
library(stringr)

parse_ages <- function(age_vector) {
  # 1. Clean the text: trim spaces and force lowercase
  age_clean <- str_to_lower(str_trim(age_vector))
  
  # Extract the first number sequence found in the string
  num_value <- as.numeric(str_extract(age_clean, "\\d+"))
  
  # Initialize vectors to hold our final outputs
  final_age  <- rep(NA_real_, length(age_vector))
  final_unit <- rep(NA_character_, length(age_vector))
  
  # Step 2: Classify based on string keywords
  for(i in seq_along(age_clean)) {
    txt <- age_clean[i]
    
    if (is.na(txt) || txt == "") {
      next
    }
    
    # Case A: Complex combinations like "1YR9MNTS"
    if (str_detect(txt, "yr.*mn")) {
      yrs <- as.numeric(str_extract(txt, "^\\d+"))
      mns <- as.numeric(str_extract(txt, "\\d+(?=mn)"))
      
      # Convert combination to Months (e.g., 1 yr 9 months = 21 months)
      final_age[i]  <- (yrs * 12) + mns
      final_unit[i] <- "Months"
      
      # Case B: Days (e.g., "10DAYS", "11days")
    } else if (str_detect(txt, "day")) {
      final_age[i]  <- num_value[i]
      final_unit[i] <- "Days"
      
      # Case C: Months (e.g., "2 month")
    } else if (str_detect(txt, "mon|mnt")) {
      final_age[i]  <- num_value[i]
      final_unit[i] <- "Months"
      
      # Case D: Plain numbers or explicitly tagged Years (e.g., "11", "10yrs", "14YRS")
    } else {
      final_age[i]  <- num_value[i]
      final_unit[i] <- "Years"
    }
  }
  
  # Return both variables packed neatly in a tibble
  return(tibble(age = final_age, age_unit = final_unit))
}







