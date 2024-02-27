library(tidyverse)
library(svglite)
library(ggdark)

data_folder <- "Processed Data/"
files <- list.files(data_folder, "plot-data")
questions <- str_remove(files, "_word-freq-plot-data\\.txt")

word_freqs <- tibble()
for (f in seq_along(files)) {
  raw_data <- read_tsv(paste0(data_folder, files[f]), col_types = "ciiiiicccccdic") %>% 
    mutate(Question = questions[f])
  word_freqs <- rbind(
    word_freqs,
    raw_data
    )
}
rm(raw_data)

by_feature <- word_freqs %>% 
  group_by(feature, `Touch No.`, Touch_desc, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
  summarise(frequency = sum(frequency))

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

hist(x = by_feature$frequency)

by_feature %>% 
  ggplot(aes(x = feature, y = frequency)) +
  geom_col(position = "dodge")

by_descriptor <- by_feature %>% 
  mutate(
    descriptor = case_when(
      feature == "affect" ~ "affection",
      feature == "attent" ~ "attention",
      feature == "gentl" ~ "gentle",
      feature == "get_my_attent" ~ "attention",
      feature == "romantic_partn" ~ "romantic partner",
      feature == "partner" ~ "romantic partner",
      feature == "grab" ~ "attention",
      feature == "colleagu" ~ "acq-str-prof",
      feature == "doctor" ~ "acq-str-prof",
      feature == "acquaintance" ~ "acq-str-prof",
      feature == "stranger" ~ "acq-str-prof",
      feature == "family_memb" ~ "family",
      feature == "frustrat" ~ "frustration",
      feature == "happi" ~ "happy",
      feature == "house" ~ "home",
      feature == "hous" ~ "home",
      feature == "irrit" ~ "irritating",
      feature == "offic" ~ "workplace",
      feature == "work" ~ "workplace",
      feature == "surpris" ~ "surprise",
      feature == "tickl" ~ "tickle",
      feature == "forearm" ~ "arm",
      .default = as.character(feature)
    )
  ) %>% 
  mutate(
    Group = case_when(
      descriptor == "arm" ~ "body",
      descriptor == "home" ~ "place",
      descriptor == "comfort" ~ "emotion",
      descriptor == "attention" ~ "intention",
      descriptor == "calm" ~ "emotion",
      descriptor == "friend" ~ "person",
      descriptor == "soft" ~ "sensory",
      descriptor == "hand" ~ "body",
      descriptor == "rub" ~ "sensory",
      descriptor == "tap" ~ "sensory",
      descriptor == "shoulder" ~ "body",
      descriptor == "love" ~ "emotion",
      descriptor == "want" ~ "intention",
      descriptor == "warm" ~ "sensory",
      descriptor == "romantic partner" ~ "person",
      descriptor == "gentle" ~ "sensory",
      descriptor == "annoy" ~ "emotion",
      descriptor == "back" ~ "body",
      descriptor == "show" ~ "intention",
      descriptor == "workplace" ~ "place",
      descriptor == "feel" ~ "intention",
      descriptor == "pat" ~ "sensory",
      descriptor == "child" ~ "person",
      descriptor == "poke" ~ "sensory",
      descriptor == "watch" ~ "intention",
      descriptor == "affection" ~ "emotion",
      descriptor == "room" ~ "place",
      descriptor == "family" ~ "person",
      descriptor == "press" ~ "sensory",
      descriptor == "make" ~ "intention",
      descriptor == "talk" ~ "intention",
      descriptor == "irritating" ~ "emotion",
      descriptor == "sofa" ~ "place",
      descriptor == "acq-str-prof" ~ "person",
      descriptor == "stroke" ~ "sensory",
      descriptor == "fast" ~ "sensory",
      descriptor == "sit" ~ "place",
      descriptor == "smooth" ~ "sensory",
      descriptor == "bedroom" ~ "place",
      descriptor == "friction" ~ "sensory",
      descriptor == "sensual" ~ "emotion",
      descriptor == "tickle" ~ "emotion",
      descriptor == "pleasant" ~ "emotion",
      descriptor == "happy" ~ "emotion",
      descriptor == "surprise" ~ "emotion",
      descriptor == "curious" ~ "emotion",
      descriptor == "frustration" ~ "memotion",
      descriptor == "wonder" ~ "emotion"
    )
      ) %>% 
  group_by(Group, descriptor, `Touch No.`, Touch_desc, Type, Direction, Contact, Force, `Speed (cm/s)`) %>% 
  summarise(Frequency = sum(frequency)) %>% 
  mutate(
    Type = factor(Type, levels = c("horizontal hand", "vertical hand", "horizontal finger", "vertical finger")),
    Force = factor(Force, levels = c("strong", "light")),
    `Speed (cm/s)` = factor(`Speed (cm/s)`, levels = c("3", "9", "18"))
  ) 

by_descriptor %>% 
  write_tsv(paste0(data_folder, "descriptor_map_data.tsv"))

by_descriptor_overall <- by_descriptor %>% 
  group_by(descriptor) %>% 
  summarise(Frequency = sum(Frequency))

freq_order <- by_descriptor_overall %>% 
  arrange(-Frequency) %>% 
  pull(descriptor)

by_descriptor_overall %>% 
  mutate(Descriptor = factor(descriptor, levels = freq_order)) %>% 
  # filter(frequency > 20) %>% 
  ggplot(aes(x = Descriptor, y = Frequency)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 15, colour = "red") +
  theme_x45deg

exclude <- by_descriptor_overall %>% 
  filter(Frequency < 20) %>% 
  pull(descriptor)

library(ggthemes)
library(scales)
show_col(tableau_color_pal('Classic 20')(20))

sensory_descriptors <- c(
  "tap", "press", "warm",
  "pat", "rub", "soft",
  "poke", "stroke", "gentle"
  )

quartz()
by_descriptor %>% 
  filter((descriptor %in% sensory_descriptors)) %>% 
  mutate(Descriptor = factor(descriptor, levels = sensory_descriptors)) %>% 
  ggplot(aes(x = Type, y = `Speed (cm/s)`)) +
  facet_wrap(~ Descriptor) +
  geom_point(aes(size = Frequency, fill = Force), shape = 21, alpha = 0.8, position = position_jitter(0.05, 0.05)) +
  scale_fill_manual(values = c("#EDC948", "#17becf")) +
  labs(x = NULL) 

ggsave("Figures/sensory_map.svg", width = 6, height = 4.8)


person_descriptors <- c(
  "friend", "child",
  "romantic partner", "acq-str-prof"
)

by_descriptor %>% 
  filter((descriptor %in% person_descriptors)) %>% 
  mutate(Descriptor = factor(descriptor, levels = person_descriptors)) %>% 
  ggplot(aes(x = Type, y = `Speed (cm/s)`)) +
  facet_wrap(~ Descriptor) +
  geom_point(aes(size = Frequency, fill = Force), shape = 21, alpha = 0.8, position = position_jitter(0.05, 0.05)) +
  scale_fill_manual(values = c("#EDC948", "#17becf")) +
  labs(x = NULL) 

ggsave("Figures/person_map.svg", width = 6, height = 4.8)

affect_descriptors <- c(
  "comfort", "attention", "annoy",
  "calm", "love", "affection"
)

quartz()
by_descriptor %>% 
  filter((descriptor %in% affect_descriptors)) %>% 
  mutate(Descriptor = factor(descriptor, levels = affect_descriptors)) %>% 
  ggplot(aes(x = Type, y = `Speed (cm/s)`)) +
  facet_wrap(~ Descriptor) +
  geom_point(aes(size = Frequency, fill = Force), shape = 21, alpha = 0.8, position = position_jitter(0.1, 0.1)) +
  scale_fill_manual(values = c("#EDC948", "#17becf")) +
  labs(x = NULL) 

ggsave("Figures/affect_map.svg", width = 6.5, height = 4.5)




unique(by_descriptor$Group)


by_descriptor %>% 
  mutate(Descriptor = factor(descriptor, levels = freq_order)) %>% 
  filter(Group == "place" & !(Descriptor %in% exclude)) %>% 
  ggplot(aes(x = Type, y = `Speed (cm/s)`)) +
  facet_wrap(~ Descriptor) +
  geom_point(aes(size = Frequency, fill = Force), shape = 21, alpha = 0.8, position = position_jitter(0.05, 0.05)) +
  scale_fill_manual(values = c("#EDC948", "#17becf")) +
  labs(x = NULL) 

ggsave("Figures/place_map.svg", width = 8, height = 4.8)

by_descriptor %>% 
  mutate(Descriptor = factor(descriptor, levels = freq_order)) %>% 
  filter(Group == "intention" & !(Descriptor %in% exclude)) %>% 
  ggplot(aes(x = Type, y = `Speed (cm/s)`)) +
  facet_wrap(~ Descriptor) +
  geom_point(aes(size = Frequency, fill = Force), shape = 21, alpha = 0.8, position = position_jitter(0.05, 0.05)) +
  scale_fill_manual(values = c("#EDC948", "#17becf")) +
  labs(x = NULL) 

ggsave("Figures/intention_map.svg", width = 8, height = 4.8)


