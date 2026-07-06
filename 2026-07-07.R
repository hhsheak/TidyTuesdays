if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, lubridate, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-07-07')

ufc_athletes <- tuesdata$ufc_athletes
ufc_fights <- tuesdata$ufc_fights
ufc_rankings_dataset <- tuesdata$ufc_rankings_dataset
ufcstats_data <- tuesdata$ufcstats_data
ultimate_ufc_dataset <- tuesdata$ultimate_ufc_dataset

#Betting Odds v. Actual Winner
betting_odds <- ultimate_ufc_dataset %>%
  mutate(red_prob = (100/(r_ev + 100))) %>% #Converts American odds to probability
  mutate(red_win = if_else(winner == "Red", 1, 0)) %>%
  select(red_prob, red_win) %>%
  drop_na()

betting_odds_plot <- betting_odds %>%
  ggplot(aes(x = red_prob, y = red_win)) +
  geom_point() +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial)) +
  labs(x = "Probabilty of Red Winning",
       y = "Whether Red Wins or Loses",
       title = "Probability of Red Winning v. Whether Red Wins")

betting_odds_plot

#Relationship between takedown accuracy and peak rankings
peak_rankings <- ufc_rankings_dataset %>%
  filter(rank != 0) %>%
  group_by(fighter) %>%
  summarize(peak = min(rank)) %>%
  rename(name = fighter)

takedown_rankings <- left_join(peak_rankings, ufcstats_data, by = "name") %>%
  select(name:peak, td_acc) %>%
  mutate(td_acc = (parse_number(td_acc)) / 100)

takedown_rankings_plot <- takedown_rankings %>%
  ggplot(aes(x = td_acc, y = peak)) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_reverse() +
  labs(x = "Takedown Accuracy",
       y = "Peak Ranking",
       title = "Relationship between Takedown Accuracy and Peak Ranking")

takedown_rankings_plot

#Relationship between physical attributes and winning
attributes_winning <- ultimate_ufc_dataset %>%
  mutate(red_prob = (100/(r_ev + 100))) %>% #Converts American odds to probability
  mutate(red_win = if_else(winner == "Red", 1, 0)) %>%
  select(height_dif:age_dif, red_win) %>%
  filter(height_dif > -100, #Removes outliers
         reach_dif > -150) %>%
  drop_na() 

height_winning_plot <- attributes_winning %>%
  ggplot(aes(x = height_dif, y = red_win)) +
  geom_point() +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial)) +
  labs(x = "Height Difference",
       y = "Probability of Red Winning",
       title = "Height")

reach_winning_plot <- attributes_winning %>%
  ggplot(aes(x = reach_dif, y = red_win)) +
  geom_point() +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial)) +
  labs(x = "Reach Difference",
       y = "Probability of Red Winning",
       title = "Reach")

age_winning_plot <- attributes_winning %>%
  ggplot(aes(x = age_dif, y = red_win)) +
  geom_point() +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial)) +
  labs(x = "Age Difference",
       y = "Probability of Red Winning",
       title = "Age")

height_winning_plot / reach_winning_plot / age_winning_plot + plot_layout(axes = "collect") + plot_annotation(title = "Relationship between Physical Attributes and Winning")

#Change in Fight Finishes
fight_finishes <- ufc_fights %>%
  select(date, method) %>%
  mutate(
    year = year(date),
    method = case_when(
    str_detect(method, "Decision") ~ "Decision",
    str_detect(method, "KO") ~ "KO",
    str_detect(method, "Submission") ~ "Submission", 
    TRUE ~ "Others" #Summarises the 10 possible outcomes into these 4 categories
  )) %>%
  mutate(method = factor(method, levels = c("Decision", "KO", "Submission", "Others"))) %>%
  select(year, method) %>%
  group_by(year, method) %>%
  count()

fight_finishes_plot <- fight_finishes %>%
  ggplot(aes(x = year, y = n, colour = method)) +
  geom_line() +
  labs(x = "Year",
       y = "Count",
       colour = "Fight Finishes",
       title = "Change in Fight Finishes (1994–2026)")

fight_finishes_plot
