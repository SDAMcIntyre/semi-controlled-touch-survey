library(tidyverse)
library(svglite)
library(readxl)
library(ggdark)

my_dark_theme <- dark_theme_light(base_size = 16) + 
  theme(
    panel.background = element_rect(fill = "#191919"),
    plot.background = element_rect(fill = "#191919"),
    legend.background=element_rect(fill="#191919", colour=NA),
    legend.key=element_rect(fill="#191919", colour = NA)
  )

theme_x45deg <- theme(
  axis.text.x=element_text(angle=45, hjust = 1)
)

theme_nofacetbox <- theme(
  strip.background = element_blank(),
  strip.text = element_text(colour = "white")
)

theme_set(my_dark_theme + theme_nofacetbox + theme_x45deg)

# coded_data_file <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/semi-controlled social touch/online survey/Data/Overview of Codes .xlsx"
# coded_data_file <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/semi-controlled social touch/online survey/Data/SocialContext_CodeFrequencies.xlsx"
# coded_data_file <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/semi-controlled social touch/online survey/Data/Frequencies_rearranged.xlsx"
coded_data_file <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/semi-controlled social touch/online survey/Data/Frequencies_perVideo.xlsx"
coded_data <- read_excel(coded_data_file) %>% 
  separate_wider_delim(
    cols = Video, 
    delim = " ",
    names = c("Contact", "Direction", "Speed (cm/s)", "Force"),
    cols_remove = FALSE
  ) %>% 
  mutate(
    Frequency = as.integer(Frequency),
    Direction = case_when(
      Direction == "tapping" ~ "vertical",
      Direction == "stroking" ~ "horizontal"
    ),
    Force = str_extract(Force, "(hard)|(light)") %>% factor(levels = c("light", "hard")),
    `Speed (cm/s)` = factor(`Speed (cm/s)`, levels = c("3", "9", "18"))
      ) %>%  
  unite("Type", c(Direction, Contact), sep = " ", remove = FALSE) %>% 
  rename(Code = `Coding tree`)
  # filter(!is.na(Video))


by_code <- coded_data %>% 
  group_by(Code, Video, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
  summarise(frequency = sum(Frequency)) %>% 
  ungroup() %>% 
  mutate(Code_no = as.numeric(factor(Code)))

# rank/order/filter by overall frequency 
by_code_overall <- by_code %>% 
  group_by(Code, Code_no) %>% 
  summarise(frequency = sum(frequency))

freq_order <- by_code_overall %>% 
  arrange(-frequency) %>% 
  pull(Code_no)

by_code_overall %>% 
  mutate(Code_no = factor(Code_no, levels = freq_order)) %>% 
  # filter(frequency > 100) %>% 
  ggplot(aes(x = Code_no, y = frequency)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 15, colour = "red") +
  theme_x45deg

exclude <- by_code_overall %>% 
  filter(frequency < 20) %>% 
  pull(Code_no)

set_n <- 6
top_9_no <- freq_order[(1+9*(set_n-1)):(9*set_n)]
(top_9 <- by_code_overall %>% 
  filter((Code_no %in% top_9_no)) %>% 
  pull(Code))
  
# quartz()
by_code %>% 
  filter((Code %in% top_9)) %>% 
  mutate(Code = factor(Code, levels = top_9)) %>% 
  ggplot(aes(x = Type, y = `Speed (cm/s)`)) +
  facet_wrap(~ Code) +
  geom_point(aes(size = frequency, fill = Force), shape = 21, alpha = 0.8, position = position_jitter(0.05, 0.05)) +
  scale_fill_manual(values = c("#17becf", "#EDC948")) +
  labs(x = NULL) 

ggsave(paste0("Figures/9set_",set_n,"_code_map.svg"), width = 9, height = 6.6)
