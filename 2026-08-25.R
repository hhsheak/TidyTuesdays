if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, RColorBrewer, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-08-25')

#Top figures
top_all_writers <- tuesdata$top_all_writers %>%
  as_tibble() %>%
  mutate(grouped = fct_lump_n(writer, n = 10, w = as.integer(song_count)))

all_writers_plot <- top_all_writers %>%
  ggplot(aes(x = "", y = song_count, fill = grouped)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Top Writers (n = 510)",
       fill = "Writers")

top_primary_writers <- tuesdata$top_primary_writers %>%
  as_tibble() %>%
  mutate(grouped = fct_lump_n(writer, n = 10, w = as.integer(song_count)))

top_writers_plot <- top_primary_writers %>%
  ggplot(aes(x = "", y = song_count, fill = grouped)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Top Primary Writers (n = 71)",
       fill = "Primary Writers")

top_producers <- tuesdata$top_producers %>%
  as_tibble() %>%
  mutate(grouped = fct_lump_n(producer, n = 10, w = as.integer(song_count)))

top_producers_plot <- top_producers %>%
  ggplot(aes(x = "", y = song_count, fill = grouped)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Top Producers (n = 110)",
       fill = "Producers")

all_writers_plot / top_writers_plot / top_producers_plot

#Most Common Words in Country Songs
pacman::p_load(tidytext, gganimate, lubridate, gifski)

lyrics <- tuesdata$country_lyrics

words <- tuesdata$country_lyrics %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  group_by(entered_top_30_in, word) %>%
  count() %>%
  group_by(entered_top_30_in) %>%
  arrange(desc(n), word) %>%
  mutate(rank = row_number(), #Prevents words with the same count from overlapping each other
         no_rel = n / n [rank == 1],,
         year = as.character(entered_top_30_in)) %>%
  ungroup() %>%
  slice_min(order_by = rank, n = 10, by = year)

#Taken from https://www.r-bloggers.com/2020/01/how-to-create-bar-race-animation-charts-in-r/
staticplot <- ggplot(words, aes(rank, group = word,
                                       fill = as.factor(word), color = as.factor(word))) +
  geom_tile(aes(y = n/2,
                height = n,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(word, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = n,label = n, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim <- staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Most Commonly Used Words in Country Songs in {closest_state}')

animate(anim, duration = 30, fps = 20,  width = 800, height = 500,
        renderer = gifski_renderer())
