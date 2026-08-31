if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, ggrepel, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-09-01')

world_castles <- tuesdata$world_castles %>%
  as_tibble()

#Countries with the most castles
castles <- world_castles %>%
  filter(category == "castle") %>%
  group_by(country) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 10) %>%
  mutate(country = if_else(country == "Czech Republic", "Czechia", country))

castles_plot <- castles %>%
  ggplot(aes(x = fct_reorder(country, n, .desc = TRUE), y = n)) +
  geom_col() +
  labs(x = "Country",
       y = "Number of Castles",
       title = "Top 10 Countries with the Most Castles")

castles_plot

#Age of Palaces v. Fortresses v. Castles
age_plot <- world_castles %>%
  filter(category == "fortress" | category == "palace" | category == "castle") %>%
  mutate(category = str_to_sentence(category)) %>%
  ggplot(aes(x = year, fill = category)) +
  geom_histogram(bins = 100) +
  labs(x = "Year",
       y = "Count",
       fill = "Category")

age_plot

#Relationship between Site Links and Page Views

#Taken from this website: https://www.statology.org/label-outliers-in-boxplots-ggplot2/
find_outlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

link_view <- world_castles %>%
  drop_na() %>%
  mutate(link_view_ratio = pageviews / sitelinks) %>%
  mutate(outlier = if_else(find_outlier(link_view_ratio), name, NA))

link_view_plot <- link_view %>%
  ggplot(aes(x = sitelinks, y = pageviews)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, max.overlaps = 5, min.segment.length = 0)

link_view_plot
