library(tidyverse)
library(readxl)

ilona_out <- read_csv("output_preprocessed_codes_ilona19_04.csv")
coded_export_data <- read_excel("attempt1_allRecoded.xlsx")

# MISSING QUESTION DATA ####

missing_Q <- ilona_out %>% 
  filter(is.na(Question)) %>% 
  # xtabs( ~ PID + VideoID, data = .)
  group_by(PID, VideoID) %>% 
  tally() # don't care about n, just want unique combos of PID and VideoID

# look at one PID/VideoID combo with missing question data
row_n <- 63
ilona_out %>% 
  filter(PID == missing_Q$PID[row_n] & VideoID == missing_Q$VideoID[row_n]) %>% View

# UNIQUENESS OF ROWS ####
duplicates <- ilona_out %>% 
  group_by(PID, VideoID, Segment, Question, Code) %>% 
  tally() %>% 
  filter(n!=1) # should be empty if every combo of above variables is unique

write_csv(duplicates, "duplicate_rows.csv")

# look at one example of non-unique data
row_n <- 1
ilona_out %>% 
  filter(
    PID == duplicates$PID[row_n] & 
      VideoID == duplicates$VideoID[row_n] &
      Segment == duplicates$Segment[row_n] &
      Question == duplicates$Question[row_n] &
      Code == duplicates$Code[row_n]
    ) %>% View

# compare with data exported from MaxQDA
coded_export_data %>% 
  filter(
    `Document name` == duplicates$PID[row_n] &
      Segment == duplicates$Segment[row_n]
  ) %>% View
