if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-07-21')

nde_experiences <- tuesdata$nde_experiences %>%
  filter(classification == "NDE") %>%
  mutate(across(ai_obe:ai_aliens, as.integer))

#Most Common Experiences in NDEs
nde_sum_experiences <- nde_experiences %>%
  select(ai_obe:ai_aliens) %>%
  pivot_longer(
    cols = starts_with("ai_"),
    names_to = "Experience",
    values_to = "Exists"
  ) %>%
  group_by(Experience) %>%
  summarise(Frequency = sum(Exists)) %>%
  mutate(Experience = fct_reorder(Experience, Frequency, .desc = TRUE ))

nde_sum_experiences_plot <- nde_sum_experiences %>%
  ggplot(aes(x = Experience, y = Frequency)) +
  geom_col() +
  labs(
    title = "Most Common Experience in NDEs"
  )

nde_sum_experiences_plot

#Correlation between OBEs, ESP and Unity
pacman::p_load(rcompanion)

OBE_ESP <- table(nde_experiences$ai_obe, nde_experiences$ai_esp,)
cramerV(OBE_ESP) #As these are categorical values, the Cramer's V test is the most applicable

OBE_unity <- table(nde_experiences$ai_obe, nde_experiences$ai_unity)
cramerV(OBE_unity)

#Correlation between Greyson Score and Gender
pacman::p_load(rstatix, ggpubr)

greyson_gender_dropna <- nde_experiences %>%
  select(gender, greyson_score) %>%
  drop_na()

greyson_gender <- greyson_gender_dropna %>%
  t_test(greyson_score ~ gender) %>% #As gender in this dataset is a binary value, a t-test is the most suitable
  add_significance() %>%
  add_xy_position("Gender")

greyson_gender_plot <- ggboxplot(greyson_gender_dropna, x = "gender", y = "greyson_score",
                                 colour = "gender", palette = c("blue", "red"),
                                 add = c("jitter", "mean"), xlab = "Gender", ylab = "Greyson Score") +
  stat_pvalue_manual(greyson_gender, tip.length = 0) +
  labs(title = "Correlation between Gender and Greyson Score",
    subtitle = get_test_label(greyson_gender, detailed = TRUE),
    colour = "Gender") +
  theme(legend.position = "bottom")

greyson_gender_plot

#Change in Number of NDE Submissions between 1999 and 2025
pacman::p_load(lubridate)

years <- nde_experiences %>%
  drop_na() %>%
  mutate(year = year(exp_date)) %>%
  filter(year >= 1999) %>%
  select(year) %>%
  count(year)

years_change_plot <- years %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  labs(title = "Change in NDE Submissions between 1999 and 2025",
       x = "Year",
       y = "Number of NDE Submissions")

years_change_plot

#Relationship between NDE depth and narrative length

scaled_data <- nde_experiences %>%
  select(greyson_score, narrative_length) %>%
  drop_na() %>%
  scale() %>%
  as_tibble() #Scales data to normal distribution

depth_length_plot <- ggscatter(scaled_data, x = "greyson_score", y = "narrative_length",
                               add = "reg.line",
                               cor.coef = TRUE,
                               title = "Correlation between Greyson Score and Narrative Length",
                               xlab = "Greyson Score (Scaled)", ylab = "Narrative Length (Scaled)")

depth_length_plot
