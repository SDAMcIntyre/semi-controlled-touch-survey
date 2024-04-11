library(readxl)
library(openxlsx)
library(readr)
library(tidyverse)
library(dplyr)


##############################################################
# Script used to clean and re-format the coded export file 
#############################################################

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


# export for checking:
write.csv(coded_no_demographic_df, "output_preprocessed_codes_ilona_11_04_24.csv", row.names = FALSE)



