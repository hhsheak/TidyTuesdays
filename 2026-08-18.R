if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-08-18')

#Best Performing Languages
best_performing_languages_general <- tuesdata$performance_by_first_language %>%
  filter(part == "overall",
         type == "General_Training") %>%
  group_by(language) %>%
  summarise(avg_score = mean(score)) %>%
  slice_max(order_by = avg_score, n = 10)

general_performance_language <- tuesdata$performance_by_first_language %>%
  filter(part == "overall",
         type == "General_Training") %>% #Looks at overall scores for general training
  mutate(avg_score = mean(score), .by = language) %>%
  filter(language %in% best_performing_languages_general$language) %>%
  mutate(language = fct_reorder(language, avg_score, .desc = TRUE))

general_performance_language_plot <- general_performance_language %>%
  ggplot(aes(x = language, y = score)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Language",
       y = "Score",
       title = "Best Performing Languages in General Training IELTS")

best_performing_languages_academic <- tuesdata$performance_by_first_language %>%
  filter(part == "overall",
         type == "Academic") %>%
  group_by(language) %>%
  summarise(avg_score = mean(score)) %>%
  slice_max(order_by = avg_score, n = 10)

academic_performance_language <- tuesdata$performance_by_first_language %>%
  filter(part == "overall",
         type == "Academic") %>% #Looks at overall scores for general training
  mutate(avg_score = mean(score), .by = language) %>%
  filter(language %in% best_performing_languages_academic$language) %>%
  mutate(language = fct_reorder(language, avg_score, .desc = TRUE))

academic_performance_language_plot <- academic_performance_language %>%
  ggplot(aes(x = language, y = score)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Language",
       y = "Score",
       title = "Best Performing Languages in Academic IELTS")

general_performance_language_plot / academic_performance_language_plot + plot_layout(axes = "collect")

#Most Difficult Section
performance_section <- tuesdata$performance_by_first_language %>%
  filter(part != "overall") %>%
  mutate(part = str_to_sentence(part),
         type = if_else(type == "General_Training", "General", type))

performance_section_plot <- performance_section %>%
  ggplot(aes(x = part, y = score, fill = type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Section",
       y = "Score",
       fill = "Type",
       title = "Performance by Section")

performance_section_plot

#Change in Performance 
performance_year_general <- tuesdata$performance_by_first_language %>%
  mutate(year = as_factor(str_remove(year, "-.*")),
         part = str_to_sentence(part)) %>%
  filter(type == "General_Training") %>%
  group_by(year, part) %>%
  summarise(avg_score = mean(score)) %>%
  mutate(year = factor(year, levels = c("2022", "2023", "2024")),
         part = factor(part, levels = c("Overall", "Reading", "Listening", "Speaking", "Writing")))

performance_year_general_plot <- performance_year_general %>%
  ggplot(aes(x = year, y = avg_score, colour = part, group = part)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Mean Score",
       colour = "Section",
       title = "General Training")

performance_year_academic <- tuesdata$performance_by_first_language %>%
  mutate(year = as_factor(str_remove(year, "-.*")),
         part = str_to_sentence(part)) %>%
  filter(type == "Academic") %>%
  group_by(year, part) %>%
  summarise(avg_score = mean(score)) %>%
  mutate(year = factor(year, levels = c("2022", "2023", "2024")),
         part = factor(part, levels = c("Overall", "Reading", "Listening", "Speaking", "Writing")))

performance_year_academic_plot <- performance_year_academic %>%
  ggplot(aes(x = year, y = avg_score, colour = part, group = part)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Mean Score",
       colour = "Section",
       title = "Academic")

performance_year_general_plot / performance_year_academic_plot + plot_layout(axes = "collect", guides = "collect") + plot_annotation(title = "Change in Section Mean Score Over Time")
