      library(tidyverse)

      # G-DRG pattern: 4 letters (MDC) + 2 digits (No.) + optional 1 letter (Age split A/C)
      gdrg_pattern <- "[A-Z]{4}\\d{2}[AC]?\\b"

      extract_gdrg_parts <- function(df, text_col) {
        df %>%
          mutate(
            gdrg_code = str_extract({{ text_col }}, gdrg_pattern),
            has_gdrg  = !is.na(gdrg_code)
          ) %>%
          extract(
            gdrg_code,
            into = c("mdc", "gdrg_no", "split"),
            regex = "([A-Z]{4})(\\d{2})([AC])?",
            remove = FALSE
          ) %>%
          mutate(split = na_if(split, ""))
      }

