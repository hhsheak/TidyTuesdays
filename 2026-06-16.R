if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, gghighlight, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-06-16')

england_wales_names <- tuesdata$england_wales_names %>%
  as_tibble()

ni_names <- tuesdata$ni_names %>%
  as_tibble()

scotland_names <- tuesdata$scotland_name %>%
  as_tibble()

#Most popular names in 2024

#Data for England and Wales
england_wales_names_2024 <- england_wales_names %>%
  filter(Year == 2024)

england_wales_names_2024_top5 <- england_wales_names_2024 %>%
  filter(Rank <= 5)

england_wales_names_2024_top5_plot <- england_wales_names_2024_top5 %>%
  ggplot(aes(x = Rank, y= Number)) +
  geom_col(aes(fill = Sex), position = "dodge") +
  scale_fill_manual(values = c("cyan", "magenta")) +
  geom_text(aes(label = Name, group = Sex), position = position_dodge(width = 0.9), hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = "England and Wales",
  )

england_wales_names_2024_top5_plot

#Data for Northern Ireland
ni_names_2024 <- ni_names %>%
  filter(Year == 2024)

ni_names_2024_top5 <- ni_names_2024 %>%
  filter(Rank <= 5)

ni_names_2024_top5_plot <- ni_names_2024_top5 %>%
  ggplot(aes(x = Rank, y= Number)) +
  geom_col(aes(fill = Sex), position = "dodge") +
  scale_fill_manual(values = c("cyan", "magenta")) +
  geom_text(aes(label = Name, group = Sex), position = position_dodge(width = 0.9), hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Northern Ireland",
  )

ni_names_2024_top5_plot

#Data for Scotland
scotland_names_2024 <- scotland_names %>%
  filter(Year == 2024)

scotland_names_2024_top5 <- scotland_names_2024 %>%
  filter(Rank <= 5)

scotland_names_2024_top5_plot <- scotland_names_2024_top5 %>%
  ggplot(aes(x = Rank, y= Number)) +
  geom_col(aes(fill = Sex), position = "dodge") +
  scale_fill_manual(values = c("cyan", "magenta")) +
  geom_text(aes(label = Name, group = Sex), position = position_dodge(width = 0.9), hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Scotland",
  )

scotland_names_2024_top5_plot

#Collects all three plots into one plot
england_wales_names_2024_top5_plot / scotland_names_2024_top5_plot / ni_names_2024_top5_plot + plot_annotation("Most Popular Baby Names in 2024") + plot_layout(guides = "collect") & theme(legend.position = "bottom")

#Uniqueness of boy v.s. girl names
england_wales_names_2024_boys_uniqueness <- england_wales_names_2024 %>%
  filter(Sex == "Boy") %>%
  mutate(Uniqueness = Number / sum(Number))

england_wales_names_2024_girls_uniqueness <- england_wales_names_2024 %>%
  filter(Sex == "Girl") %>%
  mutate(Uniqueness = Number / sum(Number))

england_wales_names_2024_uniqueness <- bind_rows(england_wales_names_2024_boys_uniqueness, england_wales_names_2024_girls_uniqueness) %>%
  group_by(Sex) %>%
  summarise(mean_uniqueness = mean(Uniqueness)) %>%
  mutate(country = "England and Wales")

ni_names_2024_boys_uniqueness <- ni_names_2024 %>%
  drop_na(Number) %>%
  filter(Sex == "Boy") %>%
  mutate(Uniqueness = Number / sum(Number))

ni_names_2024_girls_uniqueness <- ni_names_2024 %>%
  drop_na(Number) %>%
  filter(Sex == "Girl") %>%
  mutate(Uniqueness = Number / sum(Number))

ni_names_2024_uniqueness <- bind_rows(ni_names_2024_boys_uniqueness, ni_names_2024_girls_uniqueness) %>%
  group_by(Sex) %>%
  summarise(mean_uniqueness = mean(Uniqueness)) %>%
  mutate(country = "Northern Ireland")

scotland_names_2024_boys_uniqueness <- scotland_names_2024 %>%
  filter(Sex == "Boy") %>%
  mutate(Uniqueness = Number / sum(Number))

scotland_names_2024_girls_uniqueness <- scotland_names_2024 %>%
  filter(Sex == "Girl") %>%
  mutate(Uniqueness = Number / sum(Number))

scotland_names_2024_uniqueness <- bind_rows(scotland_names_2024_boys_uniqueness, scotland_names_2024_girls_uniqueness) %>%
  group_by(Sex) %>%
  summarise(mean_uniqueness = mean(Uniqueness)) %>%
  mutate(country = "Scotland")

mean_uniqueness_by_country <- bind_rows(england_wales_names_2024_uniqueness, ni_names_2024_uniqueness, scotland_names_2024_uniqueness)

uniqueness_plot <- mean_uniqueness_by_country %>%
  ggplot() +
  geom_col(aes(x = country, y = mean_uniqueness, fill = Sex), position = "dodge") +
  scale_fill_manual(values = c("turquoise", "magenta")) +
  labs(
    title = "Mean Uniqueness of Boy and Girl names in 2024",
    y = "Mean Uniqueness",
    x = "Country"
  )

uniqueness_plot

#Bridgerton trend
bridgerton_names <- c("Eloise", "Daphne", "Penelope")

#Data for England and Wales
bridgerton_names_england_wales <- england_wales_names %>%
  filter(Name %in% bridgerton_names) %>%
  select(Year:Number) %>%
  filter(Year >= 2022) #As 2022 is the earliest year where values exist for all three name for all datasets, I decided to use it as the starting year for ease

bridgerton_names_england_wales_plot <- bridgerton_names_england_wales %>%
  ggplot() +
  geom_line(aes(x = Year, y = Number, colour = Name)) +
  scale_x_continuous(breaks = seq(2022, 2025, 1)) +
  labs(
    title = "England and Wales"
  )

bridgerton_names_england_wales_plot

#Data for Northern Ireland
bridgerton_names_ni <- ni_names %>%
  filter(Name %in% bridgerton_names) %>%
  select(Year:Number) %>%
  filter(Year >= 2022) %>%
  mutate(Number = replace_na(Number, 0))

bridgerton_names_ni_plot <- bridgerton_names_ni %>%
  ggplot() +
  geom_line(aes(x = Year, y = Number, colour = Name)) +
  scale_x_continuous(breaks = seq(2022, 2025, 1)) +
  labs(
    title = "Northern Ireland"
  )

bridgerton_names_ni_plot

#Data for Scotland
bridgerton_names_scotland <- scotland_names %>%
  filter(Name %in% bridgerton_names) %>%
  select(Year:Number) %>%
  filter(Year >= 2022)

bridgerton_names_scotland_plot <- bridgerton_names_scotland %>%
  ggplot() +
  geom_line(aes(x = Year, y = Number, colour = Name)) +
  scale_x_continuous(breaks = seq(2022, 2025, 1)) +
  labs(
    title = "Scotland"
  )

bridgerton_names_scotland_plot

#Combines the three tibbles together
bridgerton_names_total <- bind_rows(bridgerton_names_england_wales, bridgerton_names_ni) %>%
  bind_rows(bridgerton_names_scotland) %>%
  filter(Year <= 2024) %>% #2025 data doesn't exist for England and Wales, so I set 2024 as the end date for the total graph
  group_by(Year, Name) %>%
  summarise(Number = sum(Number))

bridgerton_names_total

bridgerton_names_total_plot <- bridgerton_names_total %>%
  ggplot() +
  geom_line(aes(x = Year, y = Number, colour = Name)) +
  scale_x_continuous(breaks = seq(2022, 2025, 1))

#Combines the three plots together
(bridgerton_names_england_wales_plot + bridgerton_names_ni_plot + bridgerton_names_scotland_plot) / bridgerton_names_total_plot + plot_annotation("Bridgerton Effect in the UK") + plot_layout(guides = "collect") & theme(legend.position = "bottom")
