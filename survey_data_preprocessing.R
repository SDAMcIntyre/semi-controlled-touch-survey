library(readxl)
library(openxlsx)
library(readr)
library(tidyverse)


##############################################################
# Script used to clean and re-format the original survey file 
# so that it can be imported to maxqda software for coding
#############################################################



# original survey file exported from the platform
survey_data_file <- "SocialTouchProlific_February26_2024_original.xlsx"
# read the file
survey_data <- read_excel(survey_data_file)

# file with video descriptions
video_info_file <- "video_info.csv"
# read the file
video_data <- read_csv(video_info_file)

# ordered target column names in the final data frame
# Document group - "Survey" for all rows
# Document name - participant unique ID
# Video - video ID (1-25)
# Age - age group
# Gender 
# Belongingness - which country they identify themselves with (Q368 in original)
# Force (Intensity) - light or strong, video specific, from video_info_file
# Speed - video specific, from video_info_file
# Contact_area - hand or finger, video specific, from video_info_file
# Movenment_type - stroking or tapping, video specific, from video_info_file
# Social_self, Social_body, Social_place, Social_context, Intention&Purpose, Sensory, Emotional_self, Emotional_touch (Q373 in original) - video questions
# Appropriateness
# Valence&Arousal_x - valence&arousal x coordinate value
# Valence&Arousal_y - valence&arousal y coordinate value
target_columns <- c("Document_group", "Key", "Document_name","Video", "Age", "Gender", "Belongingness", 
                    "Force", "Speed", "Contact_area", "Movement_type",
                    "Social_self", "Social_body", "Social_place", "Social_context", "Intention&Purpose", 
                    "Sensory", "Emotional_self", "Emotional_touch", "Appropriateness",
                    "Valence&Arousal_x", "Valence&Arousal_y")


# Initialize a list to store data frames for each participant+video
participant_video_data_rows <- list()
total_rows <- nrow(survey_data)
#######################################################################
# Start looping here
for (i in 1:total_rows) {
  # For each participant(row), select non-empty columns
  selected_row <- survey_data[i, ]
  # Subset the row to keep only columns with non-missing values
  selected_row <- selected_row[, sapply(selected_row, function(x) !is.na(x) && nchar(x) > 0)]
  # assign interesting values
  document_group <- "Survey"
  document_name <- selected_row[["Document name"]]
  age <- selected_row[["Age"]]
  gender <- selected_row[["Gender"]]
  belongingness <- selected_row[["Q368"]]
  # Extract unique numbers from column names (these are video IDs = videos watched by that participant)
  unique_numbers <- unique(as.numeric(gsub("^([0-9]+)_.*", "\\1", colnames(selected_row))))
  unique_numbers <- unique_numbers[!is.na(unique_numbers)] # get rid of any leftover NA
  # Loop through unique numbers
  for (video_id in unique_numbers) {
    key_value <- paste(document_name, video_id,sep = "-")
    ctr <- 0 # count how many NA values were there
    # get row from video_data that contains info about the current video
    single_video_info <- video_data[video_data$VideoID %in% video_id, , drop = FALSE]
    force <- single_video_info$Intensity[1]
    speed <- single_video_info$Speed[1]
    contact_area <- single_video_info$BodyPart[1]
    movement_type <- single_video_info$MovementType[1]
    # get important information from survey
    # Find column names that start with the current number
    matching_cols <- grepl(paste0("^", video_id, "_"), colnames(selected_row))
    single_video_info_from_survey <- selected_row[, matching_cols, drop = FALSE]
    # if (ncol(single_video_info_from_survey) > 6) {
    # exclude cheaters that said 1,1,1 or a,a,a
    tryCatch({
        social_self <- pull(single_video_info_from_survey[, grep("Social_self", names(single_video_info_from_survey))])
        if (nchar(social_self) == 1) { 
          social_self <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        social_self <- NA
        ctr <- ctr +1
      })
    tryCatch({
        social_body <- pull(single_video_info_from_survey[, grep("Social_body", names(single_video_info_from_survey))])
        if (nchar(social_body) == 1) { 
          social_body <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        social_body <- NA
        ctr <- ctr +1
      })
    tryCatch({
        social_place <- pull(single_video_info_from_survey[, grep("Social_place", names(single_video_info_from_survey))])
        if (nchar(social_place) == 1) { 
          social_place <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        social_place <- NA
        ctr <- ctr +1
      })
    tryCatch({
        social_context <- pull(single_video_info_from_survey[, grep("Social_context", names(single_video_info_from_survey))])
        if (nchar(social_context) == 1) { 
          social_context <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        social_context <- NA
        ctr <- ctr +1
      })
    tryCatch({
        intention_purpose <- pull(single_video_info_from_survey[, grep("Intention&Purpose", names(single_video_info_from_survey))])
        if (nchar(intention_purpose) == 1) { 
          intention_purpose <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        intention_purpose <- NA
        ctr <- ctr +1
      })
    tryCatch({
        sensory <- pull(single_video_info_from_survey[, grep("Sensory", names(single_video_info_from_survey))])
        if (nchar(sensory) == 1) { 
          sensory <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        sensory <- NA
      })
    tryCatch({
        emotional_self <- pull(single_video_info_from_survey[, grep("Emotional_self", names(single_video_info_from_survey))])
        if (nchar(emotional_self) == 1) { 
          emotional_self <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        emotional_self <- NA
        ctr <- ctr +1
      })
    tryCatch({
        emotional_touch <- pull(single_video_info_from_survey[, grep("Q373", names(single_video_info_from_survey))])
        if (nchar(emotional_touch) == 1) { 
          emotional_touch <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        emotional_touch <- NA
        ctr <- ctr +1
      })
    tryCatch({
        approprietness <- pull(single_video_info_from_survey[, grep("Appropriateness", names(single_video_info_from_survey))])
        if (nchar(approprietness) == 1) { 
          approprietness <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        approprietness <- NA
        ctr <- ctr +1
      })
    tryCatch({
        val_arou_x <- pull(single_video_info_from_survey[, grep("_x", names(single_video_info_from_survey))])
        if (nchar(val_arou_x) == 1) { 
          val_arou_x <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        val_arou_x <- NA
        ctr <- ctr +1
      })
    tryCatch({
        val_arou_y <- pull(single_video_info_from_survey[, grep("_y", names(single_video_info_from_survey))])
        if (nchar(val_arou_y) == 1) { 
          val_arou_y <- NA
          ctr <- ctr +1}
    }, error = function(e) {
        # Handle the error by assigning NA to social_self
        val_arou_y <- NA
        ctr <- ctr +1
      })
    if (ctr > 4) {
        print("Too many NA values")
        print(document_name)
        print(video_id)
    }
    else { # create a row for each video+participant combination
      my_row <- c(document_group, key_value, document_name, video_id, age, gender, belongingness, 
                  force, speed, contact_area, movement_type,
                  social_self, social_body, social_place, social_context, intention_purpose, sensory, emotional_self, emotional_touch, approprietness,
                  val_arou_x, val_arou_y)
      participant_video_data_rows <- append(participant_video_data_rows, list(my_row))
    } # end adding row
  } # end looping through videos 
} # end looping through rows/participants
##########################################################################################################################
# combine all collected rows to a single dataframe
target_data_format <- do.call(rbind, participant_video_data_rows)
# add column names
colnames(target_data_format) <- target_columns

target_data_format <- as.data.frame(target_data_format)
# save the output excel file
write.xlsx(target_data_format, "output_ilona15_03_24.xlsx")

