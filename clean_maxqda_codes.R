library(readxl)
library(readr)
library(tidyverse)

# manually labelled data exported from MaxQDA
maxqda_data_file <- "/Users/sarmc72/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/semi-controlled social touch/online survey/Data/MAXQDA 24 Coded Segments_allcodes.xlsx"
maxqda_data <- read_excel(maxqda_data_file)

# processed data from qualtrics
touch_data_file <- "Processed Data/touch_data.txt"
touch_data <- read_tsv(touch_data_file)

n <- 992
seg <- maxqda_data$Segment[n]
person <- maxqda_data$`Document name`[n]

# this code doesn't work
touch_data %>% 
  # mutate(found = str_detect(, seg))
  mutate(found = across(Social_self:Emotional_touch, ~ str_detect(.x, seg))) %>% 
  View()


var_name_codes <- str_remove(names(maxqda_data), ".*> ") %>% str_remove(., "Code: ")
Code_var_codes <- str_remove(unique(maxqda_data$Code), ".*> ")

setdiff(var_name_codes, Code_var_codes)
setdiff(Code_var_codes, var_name_codes)

code_list <- intersect(var_name_codes, Code_var_codes)

video_questions <- names(maxqda_data)[
  str_detect(names(maxqda_data), "Code: [0-9]+_") & 
    !str_detect(names(maxqda_data), "Autocode")
  ]

# includes sub-codes
maxqda_data %>% 
  filter(str_detect(Code, "Annoy")) %>% 
  select(c("Document name", "Code", "Segment", video_questions)) %>% View()

# no sub-codes
maxqda_data %>% 
  filter(Code == "RELATIONAL INTIMACY > Situational > What > Annoy / Tease") %>% View()


# find video 8 segments that were coded with annoy /tease

video_8_questions <- video_questions[str_detect(video_questions, " 8_")]

annoy_data <- maxqda_data %>% 
  filter(Code == "RELATIONAL INTIMACY > Situational > What > Annoy / Tease") %>% 
  # mutate(freq = rowSums(pick(any_of(video_questions)))) %>% 
  select(c("Document name", "Code", "Segment", video_8_questions)) 


annoy_no_video <- annoy_data %>% 
# maxqda_data %>% 
  select(c("Document name", "Segment", video_8_questions)) %>% 
  pivot_longer(
    cols = video_8_questions,
    names_to = "Video_Q",
    values_to = "Coded"
  ) %>% 
  group_by(`Document name`, Segment) %>% 
  summarise(nVidCoded = sum(Coded)) %>% 
  filter((nVidCoded == 0)) %>% 
  arrange()

doc <- annoy_no_video[7,1] %>% pull
seg <- annoy_no_video[7,2] %>% pull

maxqda_data %>% 
  filter(`Document name` == doc & Segment == seg) %>% View()

maxqda_data %>% 
  group_by(`Document name`, Segment, Code) %>% 
  arrange() %>% 
  tally() %>% 
  filter(n>1)

doc <- "FS_1q1I24sWtBHTpUn"
seg <- "Yes, that would be okay."
  
maxqda_data %>% 
  group_by(`Document name`, Segment, Code) %>% 
  arrange() %>% 
  filter(`Document name` == doc & Segment == seg) %>% View()
