if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, RColorBrewer, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-09-15') 

#Most Copied Biblical Books
books <- tuesdata$dead_sea_scrolls %>%
  drop_na() %>%
  count(biblical_book) %>%
  slice_max(n, n = 10) %>%
  mutate(biblical_book = fct_reorder(biblical_book, n, .desc = TRUE))

books_plot <- books %>%
  ggplot(aes(x = biblical_book, y = n)) +
  geom_col() +
  labs(title = "Most Copied Biblical Books",
       x = "Book",
       y = "Count of Copies")

books_plot

#Deuterocanonical v. Protocanonical Books by Scribal Period
deutorocanonical <- tuesdata$dead_sea_scrolls %>%
  filter(canon_status == "Deuterocanonical") %>%
  count(period) %>%
  mutate(period = fct_reorder(period, n, .desc = TRUE))

deutorocanonical_plot <- deutorocanonical %>%
  ggplot(aes(x = period, y = n)) +
  geom_col() +
  labs(title = "Deuterocanonical Books by Scribal Period",
       x = "Period",
       y = "Count of Copies")

protocanonical <- tuesdata$dead_sea_scrolls %>%
  filter(canon_status == "Protocanonical")  %>%
  count(period) %>%
  mutate(period = fct_reorder(period, n, .desc = TRUE))

protocanonical_plot <- protocanonical %>%
  ggplot(aes(x = period, y = n)) +
  geom_col() +
  labs(title = "Protocanonical Books by Scribal Period",
       x = "Period",
       y = "Count of Copies")

deutorocanonical_plot / protocanonical_plot + plot_layout(axes = "collect")

#Non-Canonical v. Canonical Copy Counts
canon_transformed <- tuesdata$dead_sea_scrolls %>%
  filter(canon_status != "Unidentified") %>%
  mutate(canon_status = if_else(canon_status == "Non-canonical", "Non-canonical", "Canonical")) %>%
  count(canon_status)

canon_transformed_plot <- canon_transformed %>%
  ggplot(aes(x = "", y = n, fill = canon_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Proportion of Canonical and Non-canonical Texts",
       fill = "Canon Status")

canon_transformed_plot

#Distribution of Manuscripts across Sites
caves <- tuesdata$dead_sea_scrolls %>%
  count(cave) %>%
  mutate(cave = as.character(cave)) %>%
  mutate(cave = replace_na(cave, "Non-Qumran")) %>%
  mutate(cave = fct_reorder(cave, n, .desc = TRUE)) 

caves_plot <- caves %>%
  ggplot(aes(x = cave, y = n)) +
  geom_col() +
  labs(title = "Distribution of Manuscripts by Site",
       x = "Cave",
       y = "Count of Copies")

caves_plot

#Language and Content Category
language <- tuesdata$dead_sea_scrolls

language_plot <- language %>%
  ggplot(aes(x = fct_infreq(content_category), fill = language)) +
  geom_bar() +
  labs(title = "Content Category and Manuscript Language",
       x = "Content Category",
       y = "Count of Copies",
       fill = "Language")

language_plot
