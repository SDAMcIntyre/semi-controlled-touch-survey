library(readxl)
library(openxlsx)
library(readr)
library(tidyverse)
library(dplyr)


##############################################################
# Script used to clean and re-format the coded export file 
#############################################################
PROCESSED_DATA_FOLDER <- "Processed Data/"
# original file exported from the coding software
coded_export_data_file <- "attempt1_allRecoded.xlsx"
# read the file
coded_export_data <- read_excel(coded_export_data_file)


########################################################
# keep only important columns
# columns to keep
columns_to_keep <- c("Document name", "Code", "Segment", 
                     "Other codes assigned to segment", "Age", "Gender", 
                     "Country", "Language", "Belongingness")

# Select only the specified columns from the large table
coded_export_data <- coded_export_data %>% select(all_of(columns_to_keep))
# change long col name: Other ... to just Other
names(coded_export_data)[names(coded_export_data) == "Other codes assigned to segment"] <- "Other"

#########################################################################
# remove rows with demographics. they mess up the structure of the data
# the information is also under columns with demographics
# demographic keywords in Code column
demographic_key_words <- c("Country","Language","Belongingness")

# Creating a regex pattern to match any of the demographic_key_words at the start of the string
pattern <- paste0("^(", paste(demographic_key_words, collapse="|"), ")")

# Filtering rows where 'Code' does not start with any of the demographic_key_words
coded_no_demographic_df <- coded_export_data %>% 
  filter(!grepl(pattern, Code))

#####################################################################################
# Add column with video id next to each video information
# each row will have the video ID from which it derived from
# get row indexes from which each video starts
video_rows <- grep("^Video", coded_no_demographic_df$Code)
# video information always starts 2 rows before that?
shift <- 2
total_rows <- nrow(coded_no_demographic_df)
VideoID_vector <- vector("integer",total_rows)
for (idx in video_rows) {
  video_text <- coded_no_demographic_df$Code[idx]
  current_video <- as.integer(gsub("\\D", "", video_text))
  VideoID_vector[(idx-shift):total_rows] <- current_video
}

# Add VideoID_vector as the first column
coded_no_demographic_df <- cbind(VideoID = VideoID_vector, coded_no_demographic_df)
# remove the rest of demographic info
# columns to keep
non_demographic_columns_to_keep <- c("Document name", "VideoID","Code", "Segment", "Other")
coded_no_demographic_df <- coded_no_demographic_df %>% select(all_of(non_demographic_columns_to_keep))
#########################################################################################
#########################################################################################
# Columns that we want in the organized file
# PID,VideoID,Segment,Code,Question

# list of questions for each video
video_questions <- c(
                    "Social_self", "Social_body", "Social_place", "Social_context", "Intention&Purpose", 
                    "Sensory", "Emotional_self", "Emotional_touch", "Appropriateness"
                    )
# function that will extract the question from the string
extract_question <- function(keywords, input_string) {
  # Initialize a vector to store matching keywords
  matching_keywords <- vector("character")
  
  # Iterate over each keyword to check if it is in the input string
  for (keyword in keywords) {
    if (grepl(keyword, input_string, fixed = TRUE)) {
      matching_keywords <- c(matching_keywords, keyword)
    }
  }
  
  # Check if there are any matches and return the first one
  if (length(matching_keywords) > 0) {
    return(matching_keywords[1])
  } else {
    return("NA")  # Return NA if no matches found
  }
}


# keep only rows that either have word RELATIONAL or Autocode in Code column
codes_only_df <- coded_no_demographic_df[grepl("RELATIONAL|Autocode", coded_no_demographic_df$Code), ]

# create a column with video questions for each row
no_rows <- nrow(codes_only_df)
questions_vector <- vector("character",no_rows)
# clean the code column from unnecessary data
codes_vector <- vector("character",no_rows)
for (ind in 1:no_rows) {
  # Get the cell value for the current row
  cell_value <- codes_only_df$Code[ind]
  # Check if the cell contains the word "Autocode"
  if (grepl("Autocode", cell_value)) {
    # get the question from that cell
    question <- extract_question(video_questions,cell_value)
    questions_vector[ind] <- question
    # get clean code
    # Split the string by ":"
    split_string <- unlist(strsplit(cell_value, ":"))
    # Get the last element of the split string
    code <- trimws(tail(split_string, 1))
    codes_vector[ind] <- code
  } # end if contains Autocode
  if (grepl("RELATIONAL", cell_value)) {
    # get the question from the cell in "Other" column
    cell_with_question <- codes_only_df$Other[ind]
    question <- extract_question(video_questions,cell_with_question)
    questions_vector[ind] <- question
    # add the whole line to codes
    codes_vector[ind] <- cell_value
  } # end if contains RELATIONAL
}
# Add Question as the first column
# Adding the vector as a new column
codes_only_df[["Question"]] <- questions_vector
# replace Code column with cleaner one
codes_only_df$Code <- codes_vector

new_col_order <- c("Document name", "VideoID", "Segment", "Code", "Question")
codes_only_df <- codes_only_df %>% select(all_of(new_col_order))


# export for checking:
write.csv(codes_only_df, paste0(PROCESSED_DATA_FOLDER,"output_preprocessed_codes_ilona.csv"), row.names = FALSE)



