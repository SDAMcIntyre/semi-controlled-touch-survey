library(readxl)
library(readr)
library(tidyverse)

# manually labelled data exported from MaxQDA
maxqda_data_file <- "MAXQDA 24 Coded Segments_NewProject.xlsx"
maxqda_data <- read_excel(maxqda_data_file)

#################################################################################

# column names
var_name_codes1 <- str_remove(names(maxqda_data), ".*> ") %>% str_remove(., "Code: ")
var_name_codes2 <- str_remove(names(maxqda_data), ".*> ")
# check what is in coulmn names but not in unique rows (in Code column)
setdiff(var_name_codes1, var_name_codes2)
# check what is in unique rows in Code column but not in columns
setdiff(var_name_codes2, var_name_codes1)


# in the code column, select unique rows and remove all text before >
Code_var_codes <- str_remove(unique(maxqda_data$Code), ".*> ")

# check what is in coulmn names but not in unique rows (in Code column)
setdiff(var_name_codes1, Code_var_codes)
# check what is in unique rows in Code column but not in columns
setdiff(Code_var_codes, var_name_codes1)


# find unique video questions in the columns
video_questions <- names(maxqda_data)[
  str_detect(names(maxqda_data), "Code:")] %>% 
  str_remove(., ">.*") %>% 
  str_remove(., "Code: ") %>% 
  trimws(.)  %>% # remove any white spaces before and after
  unique(.) # leave only unique 

# these will be the unique questions
unique_video_questions <- c()
# if it is as a clean code there then, it is a video question
ctr <- 0
for (q in video_questions) {
  my_q <- paste("Code:", q)
  if (my_q %in% names(maxqda_data)) {
    unique_video_questions <- append(unique_video_questions,q)
    ctr <- ctr +1
  }
  else {
    print(my_q)
  }
}
  
# get row indexes from which each video starts
video_rows <- which(maxqda_data$Code == "Video")

# create a new column with just video numbers
# each row will have the video ID from which it derived from
total_rows <- nrow(maxqda_data)
VideoID_vector <- vector("integer",total_rows)
for (idx in video_rows) {
  current_video <- maxqda_data$Segment[idx]
  VideoID_vector[idx:total_rows] <- current_video
}

# add that column to the original data frame
maxqda_data$VideoID <- VideoID_vector
# check how many times was video i.e 17 watched
sum(maxqda_data$Segment == 17)

# select example participant
user_id <- "R_1E1hmPQ0PvgTH68"
# select columns that start with "Code: "
code_questions <- names(maxqda_data)[str_detect(names(maxqda_data), "Code:")]
# create a smaller data frame for just that participant
maxqda_data %>% 
  group_by(`Document name`, Code, VideoID) %>% 
  filter(`Document name` == user_id) %>% 
  select(c("Document name", "Code", "Segment", "VideoID",code_questions)) -> participant_data
# choose one example video question
question <- "Social_self"
rows_for_each_video <- which(participant_data$Code == question)
# Select rows based on the identified condition
selected_data <- participant_data[rows_for_each_video, ]

# Find the column indices where the value is 1 in the selected rows
column_indices <- which(selected_data == 1, arr.ind = TRUE)[, "col"]

# Get the corresponding column names
columns_with_1 <- colnames(selected_data)[column_indices]

# Display the column names
print(columns_with_1)  # "Code: Social_self > Partner"      "Code: Social_self > Professional"
