library(rvest)
library(openxlsx)

# 1. Define your folder path
folder_path <- "Database_Registers/Admission_Resgister/"
files <- list.files(path = folder_path, pattern = "\\.xls$", full.names = TRUE)

# 2. Your exact variable names used to scan and anchor the dataset
explicit_headers <- c(
  "No", "Visit No", "Patient No.", "Patient Name", "Age", "Sex", 
  "Emergency contact Name", "Emergency Relation", "Emergency contact No.", 
  "Insurance No", "Type of Insurance", "Directorate/Department", "Clinic", 
  "Ward/Room", "Bed Number", "Admission Date", "Provisional Diagnosis", 
  "Principal Diagnosis (New Case)", "Principal Diagnosis (Old Case)", 
  "Additional Diagnosis (New case)", "Additional Diagnosis (Old case)", "Outcome"
)

for (file in files) {
  
  page <- read_html(file)
  
  # 3. Some exports wrap the page in multiple <table> nodes (title block,
  # nav/export links, then the real data table). Pull ALL tables and pick
  # the one whose header row actually contains our anchor columns.
  all_tables <- page %>% html_nodes("table") %>%
    html_table(header = FALSE, fill = TRUE)   # fill=TRUE pads short rows
  # caused by colspan title rows
  
  raw_df <- NULL
  for (tbl in all_tables) {
    hits <- which(apply(tbl, 1, function(row) any(grepl("Visit No|Patient No\\.", row))))
    if (length(hits) > 0) {
      raw_df <- tbl
      header_row_index <- hits[1]
      break
    }
  }
  
  if (is.null(raw_df)) {
    warning(paste("Could not find a table with the expected header row in:", basename(file)))
    next
  }
  
  data_start_index <- header_row_index + 1
  
  if (data_start_index <= nrow(raw_df)) {
    clean_data <- raw_df[data_start_index:nrow(raw_df), ]
    
    # Drop fully-empty rows/columns that fill=TRUE padding can introduce
    clean_data <- clean_data[rowSums(!is.na(clean_data) & clean_data != "") > 0, ]
    clean_data <- clean_data[, colSums(!is.na(clean_data) & clean_data != "") > 0]
    
    if (ncol(clean_data) == length(explicit_headers)) {
      colnames(clean_data) <- explicit_headers
      rownames(clean_data) <- NULL
      
      new_filename <- sub("\\.xls$", ".xlsx", file)
      write.xlsx(clean_data, new_filename)
      
      # Optional: file.remove(file)
      
    } else {
      warning(paste("Column count mismatch in:", basename(file),
                    "- Expected", length(explicit_headers), "columns, but found", ncol(clean_data)))
    }
  } else {
    warning(paste("No data rows found beneath the header row in:", basename(file)))
  }
}

cat("Batch conversion complete! All files processed based on variable matching rather than row skips.")