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


# ordered list of questions for each video
# in the segment column, in rows that start with "Video", number of rows matches the number and order of those questions
video_questions <- c(
  "Social_self", "Social_body", "Social_place", "Social_context", "Intention&Purpose", "Appropriateness", 
  "Sensory", "Emotional_self", "Emotional_touch"
)
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
# replace "Document name" with PID
names(coded_export_data)[names(coded_export_data) == "Document name"] <- "PID"
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

# keep only rows that either have word RELATIONAL or Autocode or Video in Code column
coded_no_demographic_df <- coded_no_demographic_df[grepl("RELATIONAL|Autocode|Video", coded_no_demographic_df$Code), ]

#####################################################################################
# Add column with video id next to each video information
# each row will have the video ID from which it derived from

# function that finds video id from detected question or 0 if question was not found
find_video_number_from_question <- function(keywords, input_string) {
  # Initialize a vector to store matching keywords
  matching_keywords <- vector("character")
  found_id <- 0
  # Iterate over each keyword to check if it is in the input string
  for (keyword in keywords) {
    # Create a regular expression pattern to match one or two digits followed by an underscore and then the keyword
    # dometimes followed by keyword and _ (23_Intention$Purpose_02)
    # pattern <- paste0("\\b[0-9]{1,2}_", keyword, "\\b")
    pattern <- paste0("\\b[0-9]{1,2}_", keyword, "(?:_\\d+)?\\b")
    
    # Find all matches of the pattern in the input string
    matches <- regmatches(input_string, gregexpr(pattern, input_string))
    
    # Check if any matches were found
    if (length(matches[[1]]) > 0) {
      # If match found, add it to the matching_keywords vector
      matching_keywords <- c(matching_keywords, matches[[1]])
    }
  }
  
  # Check if any matches were found and return the number
  if (length(matching_keywords) > 0) {
    found_id <- as.integer(gsub("\\D", "", regmatches(matching_keywords[1], regexpr("\\d+", matching_keywords[1]))))
    # found_id <- as.integer(gsub("\\D", "", matching_keywords[1]))
    if (length(found_id) > 0) {return(found_id)}
    else {
      # print("Could not create intiger")
      # print(matching_keywords[1])
      return(0)}
    
  } else {
    return(0)  # Return 0 if no matches found
  }
}
############################################
total_rows <- nrow(coded_no_demographic_df)
VideoID_vector <- vector("integer",total_rows)
ctr <- 0
for (idx in 1:total_rows) {
  id <- 0
  # check if question is in Code column
  my_string <- coded_no_demographic_df$Code[idx]
  id <- find_video_number_from_question(video_questions,my_string)
  if (id == 0) { # if id was not found under Code, check under other
    # check if question is in Other column
    my_string <- coded_no_demographic_df$Other[idx]
    id <- find_video_number_from_question(video_questions,my_string)
  }
  if (id == 0) { # if it is still 0, check after Video in Other column
    my_string <- coded_no_demographic_df$Other[idx]
    # One-liner to extract the number after "Video" and convert it to integer
    id <- as.integer(gsub("\\D", "", regmatches(my_string, regexpr("Video (\\d+)", my_string))))
  }

  if (length(id) == 0) { # I give up
    id <- 0
    print(my_string)
    ctr <- ctr +1
  }
  VideoID_vector[idx] <- id
}
###################################################################################################
# Note: there were 8 empty cells in "Other" columns, therefore they were assigned video id 0

# Add VideoID_vector as the first column
coded_no_demographic_df <- cbind(VideoID = VideoID_vector, coded_no_demographic_df)

# look for rows with video id 0, and try to fix them by looking at the surrounding video ids or PID
# get row indexes where video id == 0
no_video_id_rows_indexes <- which(coded_no_demographic_df$VideoID == 0)
for (idx in no_video_id_rows_indexes) {
  # Select the id before and after the current index
  id_before <- coded_no_demographic_df$VideoID[idx-1]
  id_after <- coded_no_demographic_df$VideoID[idx+1]
  if (id_before == id_after | id_after == 0) {
    coded_no_demographic_df$VideoID[idx] <- id_before
  }
  else if (id_before != id_after) {
    PID_before <- coded_no_demographic_df$PID[idx-1]
    PID_missing_video_id <- coded_no_demographic_df$PID[idx]
    PID_after <- coded_no_demographic_df$PID[idx+1]
    if (PID_before == PID_missing_video_id) {coded_no_demographic_df$VideoID[idx] <- coded_no_demographic_df$VideoID[idx-1]}
    else if (PID_missing_video_id == PID_after){coded_no_demographic_df$VideoID[idx] <- coded_no_demographic_df$VideoID[idx+1]}
  }
}
# check if there are no 0 videoID left
no_video_id_rows_indexes <- which(coded_no_demographic_df$VideoID == 0)

# remove the rest of demographic info
# columns to keep
non_demographic_columns_to_keep <- c("PID", "VideoID","Code", "Segment", "Other")
codes_only_df <- coded_no_demographic_df %>% select(all_of(non_demographic_columns_to_keep))
#############################################################################################
# check how many video cells are missing
# UNIQUENESS OF ROWS ####
unique_video_watches <- codes_only_df %>% 
  group_by(PID, VideoID)  %>% 
  tally()
# Filter rows where Code starts with "Video"
video_rows_df <- codes_only_df %>% 
  filter(grepl("^Video", Code))

# Calculate the number of combinations with missing "Video" codes
missing_video_count <- nrow(unique_video_watches) - nrow(video_rows_df)
# get which ones:
# Create a dataframe with all unique PID and VideoID combinations
all_combinations_df <- unique(codes_only_df[, c("PID", "VideoID")])

# Filter out combinations with rows in video_rows_df
missing_combinations_df <- anti_join(all_combinations_df, video_rows_df, by = c("PID", "VideoID"))
write.csv(missing_combinations_df, "missing_video_ilona25_04.csv", row.names = FALSE)


#########################################################################################
#########################################################################################
# Columns that we want in the organized file
# PID,VideoID,Segment,Code,Question

# function that will extract the unique questions from the string
extract_questions <- function(keywords, input_string) {
  # Initialize a vector to store matching keywords
  matching_keywords <- vector("character")
  
  # Iterate over each keyword to check if it is in the input string
  for (keyword in keywords) {
    if (grepl(keyword, input_string, fixed = TRUE)) {
      if (!(keyword %in% matching_keywords)) {
      matching_keywords <- c(matching_keywords, keyword)
      }
    }
  }
  
  # Check if there are any matches and return the first one
  if (length(matching_keywords) > 0) {
    return(matching_keywords)
  } else {
    matching_keywords <- c(matching_keywords, "NA")
    return(matching_keywords)  # Return NA if no matches found
  }
}

# return lines with answers in video cell's segment as a vector
process_video_row <- function(questions_vector, df, ind) {
  # find which video id and PID and row with Video information
  video_row <- df[df$PID == df$PID[ind] &
                    df$VideoID == df$VideoID[ind] &
                    grepl("^Video", df$Code), ]
  # Check if any rows were found
  if (nrow(video_row) == 0) {
    # print(df[ind, ])
    print(df$PID[ind])
    print(df$VideoID[ind])
    print("No matching rows found.")
    return(c("NA"))
  }
  
  # Extract and split the string
  split_string <- unlist(strsplit(video_row$Segment, "\r\n\r\n"))
  if (length(split_string) != length(questions_vector)) {
    print(df$PID[ind])
    print(df$VideoID[ind])
    print("Video cell does not have all questions answered")
    print(video_row$Segment)
    print(split_string)
    return(c("NA"))
  }
  return(split_string)
}

retrieve_question_based_on_answer_in_segment <- function(questions, segment, my_string) {
  # Find the index of my_string in the segment vector
  idx <- which(segment == my_string)
  
  # If my_string is found in the segment vector
  if (length(idx) > 0) {
    # Retrieve the corresponding question from the questions vector
    my_index <- idx[1]
    return(questions[idx])
  } else {
    # If my_string is not found, return NA
    return("NA")
  }
}

get_detailed_relation_df <- function(old_df, rows, video_questions, new_column_names) {
  # Create an empty data frame with the specified column names
  return_df <- data.frame(matrix(ncol = length(new_column_names), nrow = 0))
  colnames(return_df) <- new_column_names
  
  # Loop through each row index in the 'rows' vector
  for (ind in rows) {
    # Select the row from the old dataframe
    temp_row <- old_df[ind, ]
    
    # Extract the question and segment information
    cell_with_question <- temp_row$Other
    questions <- extract_questions(video_questions, cell_with_question)
    cell_with_answers <- temp_row$Segment
    segment <- unlist(strsplit(cell_with_answers, "\r\n\r\n"))
    
    
    
    # Ensure questions and segment have the same length
    if (length(questions) != length(segment)) {
      # check if removing Sensory will make it match
      if ("Sensory" %in% questions) {
        questions <- questions[questions != "Sensory"]
      }
      if (length(questions) != length(segment)) {
        video_segment <- process_video_row(video_questions,codes_only_df,ind)
        
        # reset questions
        questions <- c()  # Initialize an empty vector to store questions
        
        # Loop through each item in the segment vector
        for (item in segment) {
          if (video_segment[1] != "NA") {
            question <- retrieve_question_based_on_answer_in_segment(video_questions,video_segment, item)
          }
          else {question <- "NA"}
          # Add the new question to the questions vector
          questions <- c(questions, question)
        }
      
      }
      # print("There was Length mismatch between questions and segment")
    }
    
    if (length(questions) != length(segment)) {print("Still not solved")}
    
    # Create a data frame to store the rows for this iteration
    temp_df <- data.frame(matrix(ncol = length(new_column_names), nrow = length(questions)))
    colnames(temp_df) <- new_column_names
    
    # Populate the temp_df with values
    temp_df$PID <- temp_row$PID
    temp_df$VideoID <- temp_row$VideoID
    temp_df$Code <- temp_row$Code
    temp_df$Other <- temp_row$Other
    
    # Assign values from questions and segment
    temp_df$Segment <- segment
    temp_df$Question <- questions
    
    # Add the temp_df to return_df
    return_df <- rbind(return_df, temp_df)
  }
  
  # Return the resulting dataframe
  return(return_df)
}
##########################################################################

# create a column with video questions for each row
no_rows <- nrow(codes_only_df)
questions_vector <- vector("character",no_rows)
missing_video_cell_when_needed <- 0
# clean the code column from unnecessary data
# codes_vector <- vector("character",no_rows)
# vector with indexes for relationsl rows that have multiple questions
relational_rows_for_splitting <- c()
for (ind in 1:no_rows) {
  # Get the cell value for the current row
  cell_value <- codes_only_df$Code[ind]
  # Check if the cell contains the word "Autocode"
  if (grepl("Autocode", cell_value)) {
    # get the question from that cell
    question <- extract_questions(video_questions,cell_value)[1]
    questions_vector[ind] <- question
    # get clean code
    # Split the string by ":"
    split_string <- unlist(strsplit(cell_value, ":"))
    # # Get the last element of the split string
    # code <- trimws(tail(split_string, 1))
    # codes_vector[ind] <- code
  } # end if contains Autocode
  if (grepl("RELATIONAL", cell_value)) {
    # get the question from the cell in "Other" column
    cell_with_question <- codes_only_df$Other[ind]
    questions <- extract_questions(video_questions,cell_with_question)
    question <- questions[1]
    if (questions[1] == "NA") {
      video_segment <- process_video_row(video_questions,codes_only_df,ind)
      segment_cell <- codes_only_df$Segment[ind]
      if (video_segment[1] != "NA") {
        question <- retrieve_question_based_on_answer_in_segment(video_questions,video_segment, segment_cell)
      }
      else {
        question <- "NA"
        missing_video_cell_when_needed <- missing_video_cell_when_needed +1}
      } # end if question was not in Other
    if (length(questions) > 1) { # Other had multiple questions
      # for now assign the first question
      question <- questions[1]
      # add index for further processing and splitting
      relational_rows_for_splitting <- c(relational_rows_for_splitting, ind)
    } # end if len > 1
    questions_vector[ind] <- question
    # # add the whole line to codes
    # codes_vector[ind] <- cell_value
  } # end if RELATION
  if (grepl("Video", cell_value)) { # if starts with Video
    questions_vector[ind] <- "NA"
    # # add the whole line to codes
    # codes_vector[ind] <- cell_value
  }
}

# Add Question as the first column
# Adding the vector as a new column
codes_only_df[["Question"]] <- questions_vector

# # replace Code column with cleaner one
# codes_only_df$Code <- codes_vector

# split each row that started with RELATION and had multiple questions into multiple rows
new_temp_column_names <- c("PID", "VideoID","Code", "Segment", "Other", "Question")
separated_relation_df <- get_detailed_relation_df(codes_only_df,
                                                  relational_rows_for_splitting,
                                                  video_questions,
                                                  new_temp_column_names)
# how many after splitting don't have a question
separated_but_na <- separated_relation_df[separated_relation_df$Question == "NA", ]

# remove the origin rows that start with RELATION that had multiple questions
codes_only_df <- codes_only_df[-relational_rows_for_splitting, ]

# keep only rows that either have word RELATIONAL or Autocode in Code column
codes_only_df <- codes_only_df[grepl("RELATIONAL|Autocode", codes_only_df$Code), ]

# add rows from separated
joined_df <- rbind(codes_only_df, separated_relation_df)

# sort nicely
grouped_df <- joined_df %>% 
  arrange(PID, VideoID)


# remove Appropriateness question
grouped_df <- grouped_df %>%
  filter(Question != "Appropriateness")


# UNIQUENESS OF ROWS ####
duplicates <- codes_only_df %>% 
  group_by(PID, VideoID, Segment, Question, Code) %>% 
  tally() %>% 
  filter(n!=1) # should be empty if every combo of above variables is unique

# check if there are any missing questions
missing_questions <- grouped_df[grouped_df$Question == "NA", ] # 98
# export for checking:
write.csv(missing_questions, "missing_questions_ilona24_04.csv", row.names = FALSE)


# change order of columns
new_col_order <- c("PID", "VideoID", "Segment", "Code", "Question", "Other")
grouped_df <- grouped_df %>% select(all_of(new_col_order))


# check if all videos have all unique questions answered
# keep in mind that NA will be counted as unique question
# so check after removing NA
no_NA_questions <- grouped_df %>%
  filter(Question != "NA")

result_no_NA <- no_NA_questions %>%
  group_by(PID, VideoID) %>%
  summarize(unique_question_count = n_distinct(Question))

all_8 <- all(result_no_NA$unique_question_count == 8)

# Count the occurrences where unique_question_count is not equal to 8
not_equal_8 <- sum(result_no_NA$unique_question_count < 8) # 111
##############################################################
# How many I would have to exclude? Because of NA
unique_combinations <- missing_questions %>%
  distinct(PID, VideoID)
# exclude missing
excluded_videos_with_missing_questions_df <- grouped_df %>%
  anti_join(unique_combinations, by = c("PID", "VideoID"))
# how many unique PID+VideoID are there
total_unique_video_pid <- excluded_videos_with_missing_questions_df %>%
  distinct(PID, VideoID)
# distinct questions
result <- excluded_videos_with_missing_questions_df %>%
  group_by(PID, VideoID) %>%
  summarize(unique_question_count = n_distinct(Question))

all_8 <- all(result$unique_question_count == 8)
# Count the occurrences where unique_question_count is not equal to 8
less_than_8 <- sum(result$unique_question_count < 8) # 60
##############################################################
# export for checking:
write.csv(grouped_df, paste0(PROCESSED_DATA_FOLDER,"output_preprocessed_codes_ilona24_04.csv"), row.names = FALSE)

#############################################################
# check mismatch of lines in answers in video and video questions
# R_2RUsV3SU6UbZ3as 22
# R_sb3cXFomtybyKlj 14
# R_BM9eDkmZBgiYlpv 21
test <- grouped_df |> 
  filter(PID == "R_sb3cXFomtybyKlj" & VideoID == 14)
view(test)





 




