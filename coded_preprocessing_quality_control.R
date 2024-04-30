library(tidyverse)
library(readxl)

ilona_out <- read_csv("output_preprocessed_codes_ilona19_04.csv")
coded_export_data <- read_excel("attempt1_allRecoded.xlsx")

# MISSING QUESTION DATA ####

# number of rows with question missing
nrow(ilona_out %>% filter(is.na(Question)))

missing_Q_IDs <- ilona_out %>% 
  filter(is.na(Question)) %>% 
  # xtabs( ~ PID + VideoID, data = .)
  group_by(PID, VideoID) %>% 
  tally() # don't care about n, just want unique combos of PID and VideoID

# number of PID/VideoID combinations with question missing
nrow(missing_Q_IDs)

# look at one PID/VideoID combo with missing question data
row_n <- 2
ilona_out %>% 
  filter(
    PID == missing_Q_IDs$PID[row_n] & 
      VideoID == missing_Q_IDs$VideoID[row_n]
    ) #%>% View

# compare with data exported from MaxQDA
coded_export_data %>% 
  filter(
    `Document name` == missing_Q_IDs$PID[row_n] 
  ) %>% View

# Do they have "Situational > What" in the code?
missing_context <- ilona_out %>% 
  filter(is.na(Question) & str_detect(Code, "Situational > What")) #%>% View()

nrow(missing_context)

missing_context %>% 
  write_csv("missing_Q_situationalwhat60.csv")

# not yet accounted for
missing_Q2 <- ilona_out %>% 
  filter(is.na(Question) & !str_detect(Code, "Situational > What")) 

nrow(missing_Q2)

missing_Q2 %>% 
  write_csv("missing_Q_remaining38.csv")

missing_Q2 %>% 
  group_by(Code) %>% 
  tally() %>% 
  arrange(-n) %>% 
  select(Code)

# Solution
# 
# Get 60 from "Copy of missing_Q_situationalwhat60_wQuestionColumn_ADDED.xlsx"
# 
# Get remaining 38 from "Copy of missing_Q_remaining38_wQuestionColumn_ADDED.xlsx"

with_questions <- rbind(
  read_excel("Copy of missing_Q_situationalwhat60_wQuestionColumn_ADDED.xlsx"),
  read_excel("Copy of missing_Q_remaining38_wQuestionColumn_ADDED.xlsx")
)

# UNIQUENESS OF ROWS ####
duplicates <- ilona_out %>% 
  group_by(PID, VideoID, Segment, Question, Code) %>% 
  tally() %>% 
  filter(n!=1) # should be empty if every combo of above variables is unique

write_csv(duplicates, "duplicate_rows.csv")

# look at one example of non-unique data
row_n <- 9
ilona_out %>% 
  filter(
    PID == duplicates$PID[row_n] & 
      VideoID == duplicates$VideoID[row_n] &
      Segment == duplicates$Segment[row_n] &
      Question == duplicates$Question[row_n] &
      Code == duplicates$Code[row_n]
    ) %>% View


ilona_out %>% 
  filter(
    PID == duplicates$PID[row_n]
  ) %>% View

# compare with data exported from MaxQDA
coded_export_data %>% 
  filter(
    `Document name` == duplicates$PID[row_n] &
      Segment == duplicates$Segment[row_n]
  ) %>% View

coded_export_data %>% 
  filter(
    `Document name` == duplicates$PID[row_n] 
  ) %>% View


# Solution for duplicates

# WORD APPEARED TWICE IN ONE RESPONSE
# Solution – delete one row (doesn’t matter which one). Justification - for frequencies, we don’t want to count these extra times, e.g. if someone had written “love love love love love” we don’t want to count “love” 5 times, just once.
# 
# R_1nUPwvHLcXJJr3d
# R_1r7eF9ZpHmrt6a8
# R_24OvUwXWv7GhaOB
# R_28GxGerQJIriysE
# R_2cuRkGjt232mWsZ
# R_2zwlIg2JrvSMfHI
# R_UFnb3mIMesLjZhn
# R_W2jteDmtFbSIlwd
# R_bDztJQj40sDVsVr
# R_DTcUWkUWFQMxbYl
# 
# 
# MISTAKE IN AUTOCODING
# Solution - delete one row (doesn't matter which one).
# 
# R_3GdoeXDKp6jDh6o
# R_3pa2cwJrTm23zCF
# R_3kNBiPutOaE38wm
# 
# 
# THE SAME CODE APPLIES TO MULTIPLE SEGMENTS FROM THE SAME PERSON
# Not sure yet whether this is an isolated issue, it seems to just be one case where the video is wrong. Perhaps we can just correct it
# 
# R_1E1hmPQ0PvgTH68



