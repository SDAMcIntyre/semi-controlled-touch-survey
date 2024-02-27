# load libraries  ####
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(readr)

# input path ####
RAW_DATA_FOLDER <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/semi-controlled social touch/online survey/Data/"

# output paths ####
PROCESSED_DATA_FOLDER <- "Processed Data/"
FIGURES_FOLDER <- "Figures/"

# ===== MAIN ===== 

# read in raw survey data exported from qualtrics ####
raw_data_file <- paste0(
  RAW_DATA_FOLDER,
  'Social+Touch+-+Prolific_April+26,+2023_17.15(Recorded)_fixed.xlsx'
)

respondent_data <- read_excel(raw_data_file, range = "A1:W1070", col_types = "text")

touch_data <- read_excel(raw_data_file, range = "X1:LW1070", col_types = "text") %>% 
  mutate(ResponseID = respondent_data$ResponseID) %>% 
  filter(ResponseID != "FS_1IN4EwLvt1xrWtL") %>% # something wrong with this person's data, possibly misaligned
  pivot_longer(
    cols = `1_Social_self`:`25_Input`,
    names_to = c("Touch No.", "Question"),
    names_pattern = "([0-9]+)_(.+)"
  ) %>% 
  pivot_wider(
    names_from = Question,
    values_from = value
  ) %>% 
  filter(!if_all(`Social_self`:`Input`, is.na)) %>% 
  mutate(
    `Touch No.` = as.numeric(`Touch No.`),
    # min = 156.5, max = 1244.5
    Valence = as.numeric(`Valence&Arousal_x`), 
    Arousal = as.numeric(`Valence&Arousal_y`) * -1 # reverse so high arousal corresponds to high values
    ) %>% 
  mutate(Touch_desc = case_when(
    `Touch No.` == 1 ~ "finger vertical 3 light",
    `Touch No.` == 2 ~ "finger vertical 3 strong",
    `Touch No.` == 3 ~ "finger vertical 9 light",
    `Touch No.` == 4 ~ "finger vertical 9 strong",
    `Touch No.` == 6 ~ "finger vertical 18 light",
    `Touch No.` == 7 ~ "finger vertical 18 strong",
    `Touch No.` == 8 ~ "hand vertical 3 light",
    `Touch No.` == 9 ~ "hand vertical 3 strong",
    `Touch No.` == 10 ~ "hand vertical 9 light",
    `Touch No.` == 11 ~ "hand vertical 9 strong",
    `Touch No.` == 12 ~ "hand vertical 18 light",
    `Touch No.` == 13 ~ "hand vertical 18 strong",
    `Touch No.` == 14 ~ "finger horizontal 3 light",
    `Touch No.` == 15 ~ "finger horizontal 3 strong",
    `Touch No.` == 16 ~ "finger horizontal 9 light",
    `Touch No.` == 17 ~ "finger horizontal 9 strong",
    `Touch No.` == 18 ~ "finger horizontal 18 light",
    `Touch No.` == 19 ~ "finger horizontal 18 strong",
    `Touch No.` == 20 ~ "hand horizontal 3 light",
    `Touch No.` == 21 ~ "hand horizontal 3 strong",
    `Touch No.` == 22 ~ "hand horizontal 9 light",
    `Touch No.` == 23 ~ "hand horizontal 9 strong",
    `Touch No.` == 24 ~ "hand horizontal 18 light",
    `Touch No.` == 25 ~ "hand horizontal 18 strong"
  )) %>% 
  separate_wider_delim(
    cols = Touch_desc, 
    delim = " ",
    names = c("Contact", "Direction", "Speed (cm/s)", "Force"),
    cols_remove = FALSE
    ) %>% 
  unite("Type", c(Direction, Contact), sep = " ", remove = FALSE) %>% 
  mutate(
    `Touch No.` = if_else(`Touch No.` > 5, `Touch No.` - 1, `Touch No.`),
    `Speed (cm/s)` = factor(`Speed (cm/s)`, levels = c("3", "9", "18")),
    Force = factor(Force, levels = c("light", "strong"))
    ) 

touch_data %>% 
  write_tsv(paste0(PROCESSED_DATA_FOLDER,"touch_data.txt"))

n_by_touch <- touch_data %>% 
  group_by(`Touch No.`) %>% tally()

touch_data %>% 
  ggplot(aes(x = `Touch No.`)) + 
  geom_bar()

n_by_touch %>% 
  summarise(mean = mean(n), min = min(n), max = max(n))

n_by_respondent <- touch_data %>% 
  group_by(ResponseID) %>% tally()

n_by_respondent %>% 
  ggplot(aes(x = n)) + 
  geom_histogram()

Mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

n_by_respondent %>% 
  summarise(
    median = median(n),
    mode = Mode(n), 
  min = min(n), 
  max = max(n)
  )

theme_set(theme_bw())

# rank order valence ####

Valence_order <- touch_data %>% 
  group_by(Touch_desc) %>% 
  summarise(med = median(Valence, na.rm = TRUE)) %>% 
  arrange(-med) %>% 
  pull(Touch_desc)

touch_data %>% 
  mutate(Touch_desc = factor(Touch_desc, levels = Valence_order)) %>% 
  ggplot(aes(
    x = Touch_desc, y = Valence, 
    fill = `Speed (cm/s)`, alpha = Force,
    )) +
  #facet_grid(. ~ Type, scales = "free_x") +
  geom_boxplot() +  
  geom_hline(yintercept = 700, linetype = "dashed") +
  scale_y_continuous(limits = c(
    min(touch_data$Valence, na.rm = TRUE),
    max(touch_data$Valence, na.rm = TRUE)
  )) +
  scale_fill_ordinal(direction = -1) + 
  scale_alpha_manual(values = c(0.4, 0.8)) +
  theme(axis.text.x=element_text(angle=70, hjust = 1 )) +
  labs(x = NULL)


# rank order arousal ####

Arousal_order <- touch_data %>% 
  group_by(Touch_desc) %>% 
  summarise(med = median(Arousal, na.rm = TRUE)) %>% 
  arrange(-med) %>% 
  pull(Touch_desc)

touch_data %>% 
  mutate(Touch_desc = factor(Touch_desc, levels = Arousal_order)) %>% 
  ggplot(aes(
    x = Touch_desc, y = Arousal, 
    fill = `Speed (cm/s)`, alpha = Force,
  )) +
  #facet_grid(. ~ Type, scales = "free_x") +
  geom_boxplot() +  
  geom_hline(yintercept = 700, linetype = "dashed") +
  scale_y_continuous(limits = c(
    min(touch_data$Arousal, na.rm = TRUE),
    max(touch_data$Arousal, na.rm = TRUE)
  )) +
  scale_fill_ordinal(direction = -1) + 
  scale_alpha_manual(values = c(0.4, 0.8)) +
  theme(axis.text.x=element_text(angle=70, hjust = 1 )) +
  labs(x = NULL)


# valence direct comparisons ####

touch_data %>% 
  ggplot(aes(x = `Speed (cm/s)`, y = Valence, colour = Force)) +
  facet_grid(. ~ Type) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = 700, linetype = "dashed") +
  stat_summary(
    geom = "errorbar", 
    fun.data = "mean_cl_boot", 
    linewidth = 1.5,
    width = 0.3
  ) 


# arousal direct comparisons ####

touch_data %>% 
  ggplot(aes(x = `Speed (cm/s)`, y = Arousal, colour = Force)) +
  facet_grid(. ~ Type) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = -700, linetype = "dashed") +
  stat_summary(
    geom = "errorbar", 
    fun.data = "mean_cl_boot", 
    linewidth = 1.5,
    width = 0.3
  ) 


# combo ####
# https://stackoverflow.com/a/38555243
bootf <- function(x,var) {
  rbind(Hmisc::smean.cl.boot(x[[var]])) %>%
    data.frame %>%
    setNames(paste(var,c("mean","lwr","upr"),sep="_"))
}

valence_CIs <- touch_data %>% 
  group_by(`Touch No.`, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
  do(bootf(.,"Valence"))

arousal_CIs <- touch_data %>% 
  group_by(`Touch No.`, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
  do(bootf(.,"Arousal"))

valence_arousal_CIs <- full_join(valence_CIs, arousal_CIs)
  
valence_arousal_CIs %>%   
  ggplot(aes(
    x = Valence_mean, y = Arousal_mean, group = `Touch No.`,
    colour = Type
    )) +
  geom_hline(yintercept = -700) +
  geom_vline(xintercept = 700) +
  geom_point() +
  geom_errorbar(aes(ymin = Arousal_lwr, ymax = Arousal_upr), linewidth = 1, alpha = 0.8) +
  geom_errorbarh(aes(xmin = Valence_lwr, xmax = Valence_upr), linewidth = 1, alpha = 0.8) +
  labs(x = "Valence", y = "Arousal") +
  scale_colour_brewer(palette = "BrBG")


# word frequencies ####
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(stringr)
# https://www.youtube.com/watch?v=O6CGXnxPHok
# https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html
# https://stackoverflow.com/questions/69309605/using-quantedas-tokens-compound-to-join-multi-word-expressions-via-underscore-in

# text_data <- touch_data %>% 
#   group_by(`Touch No.`, Touch_desc, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
#   summarise(across(
#     .cols = c(starts_with("Social"), "Intention&Purpose", "Appropriateness", "Sensory", starts_with("Emotional")),
#     .fns = paste0,
#     collapse = " "
#     )) 

grouped_df <- touch_data %>% 
  group_by(`Touch No.`, Touch_desc, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
  tally() %>% select(-n)

my_stopwords <- c("na", "video", "touch", "tri", "someth", "just", "person")
my_phrases <- c("family member", "romantic partner", "get my attention", "attention-grabbing")

#. per question ####

for (QUESTION in c(
  "Social_self", 
  "Social_place", 
  "Social_context",
  "Social_body", 
  "Intention&Purpose", 
  "Sensory", 
  "Emotional_self", 
  "Emotional_touch"
  ))  {
  
  question_stopwords <- switch(QUESTION,
    "Social_body" = c("upper", "lower", "left", "area"),
    "Social_context" = c("get", "we'r", "someon"),
    "Intention&Purpose" = c("intent"),
    "Emotional_self" = c("make", "feel"),
    c())
  
  text_data <- touch_data %>% 
    group_by(`Touch No.`, Type) %>% 
    summarise(Question = paste0(.data[[QUESTION]], collapse = " "), .groups = "keep") %>% 
    corpus(
    docid_field = "Touch No.",
    text_field = "Question"
    ) %>% 
    tokens(remove_punct = TRUE) %>% 
    tokens_compound(pattern = phrase(my_phrases)) %>%
    dfm() %>% 
    dfm_wordstem() %>% 
    dfm_remove(c(stopwords("english"), my_stopwords, question_stopwords ))
  
  #. export ####
  text_data %>% 
    textstat_frequency(n = 15) %>% 
    arrange(-frequency) %>% 
    write_tsv(paste0(PROCESSED_DATA_FOLDER,QUESTION,"_word-freq.txt"))
    
  
  word_freq <- text_data %>% 
    textstat_frequency(n = 3, groups = text_data@docvars$docname_) %>% 
    mutate(feature = reorder(feature, -frequency))
  
  feature_order <- levels(word_freq$feature)
  
  #. word frequency plot ####
  word_freq_plot <- word_freq %>% 
    mutate(`Touch No.` = as.numeric(group)) %>% 
    full_join(grouped_df) %>% 
    mutate(feature_x = as.numeric(feature)) 
  
  #. export ###
  word_freq_plot %>% 
    write_tsv(paste0(PROCESSED_DATA_FOLDER,QUESTION,"_word-freq-plot-data.txt"))
  
  word_freq_plot %>%
    ggplot(aes(x = feature_x, y = frequency, fill = `Speed (cm/s)`, alpha = Force)) +
    stat_summary(
      geom = "bar", fun = "mean",
      position = position_dodge2(preserve = "single", width = 0.9), 
      colour = "black"
      ) +
    facet_grid( Type ~ ., scales = "free") +
    scale_fill_ordinal(direction = -1) + #BrBG
    scale_alpha_manual(values = c(0.4, 0.8)) +
    scale_x_continuous(breaks = 1:length(feature_order), labels = feature_order) +
    labs(title = QUESTION, x = NULL) +
    theme(axis.text.x=element_text(angle=45, hjust = 1 ))
  
  ggsave(paste0(FIGURES_FOLDER, QUESTION, "_word-freq.pdf"), height = 5.83, width = 8.27)
  
}


# appropriateness ####

appropriate_data <- touch_data %>% 
  filter(!is.na(Appropriateness)) %>% 
  mutate(Appropriate = case_when(
    str_detect(Appropriateness, "Yes") ~ "Yes",
    str_detect(Appropriateness, "It depends") ~ "It depends",
    str_detect(Appropriateness, "No") ~ "No",
    str_detect(Appropriateness, "I don\'t know") ~ "I don't know"
  )) %>% 
  mutate(Appropriate = factor(Appropriate, levels = c("Yes", "It depends", "No", "I don't know"))) 

appropriate_p <- appropriate_data %>% 
  group_by(Touch_desc, Type, `Speed (cm/s)`, Force, Appropriate) %>% 
  tally() %>% 
  mutate(total = sum(n)) %>% 
  mutate(p = n/total)

appropriate_order <- appropriate_p %>% 
  filter(Appropriate == "Yes") %>% 
  arrange(-p) %>% pull(Touch_desc)

appropriate_data %>% 
  mutate(Touch_desc = factor(Touch_desc, levels = appropriate_order)) %>% 
  ggplot(aes(x = Touch_desc, fill = Appropriate)) +
  geom_bar(position = "fill", width = 0.9, alpha = 0.7, colour = "black") +
  scale_fill_ordinal() +
  theme(axis.text.x=element_text(angle=70, hjust = 1 )) +
  labs(title = "Appropriateness", x = NULL)


#. appropriate direct comparison ####

appropriate_data %>% 
  mutate(Appropriate = if_else(Appropriate == "Yes", 1, 0)) %>% 
  ggplot(aes(x = `Speed (cm/s)`, y = Appropriate, colour = Force)) +
  facet_grid(. ~ Type) +
  scale_color_brewer(palette = "Set2") +
  stat_summary(
    geom = "errorbar", 
    fun.data = "mean_cl_boot", 
    linewidth = 1.5,
    width = 0.3
  ) +
  coord_cartesian(ylim = c(0.2,1)) +
  labs(title = "Proportion of responses \"Yes, it would be appropriate.\"")

appropriate_data %>% 
  mutate(Inappropriate = if_else(Appropriate == "No", 1, 0)) %>% 
  ggplot(aes(x = `Speed (cm/s)`, y = Inappropriate, colour = Force)) +
  facet_grid(. ~ Type) +
  scale_color_brewer(palette = "Set2") +
  stat_summary(
    geom = "errorbar", 
    fun.data = "mean_cl_boot", 
    linewidth = 1.5,
    width = 0.3
  ) +
  coord_cartesian(ylim = c(0,0.4)) +
  labs(title = "Proportion of responses \"No, it would not be appropriate.\"")
