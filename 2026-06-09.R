if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, ggrepel, gghighlight, scales, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-06-09')

game_films <- tuesdata$game_films %>%
  as_tibble()
  
#Video game publishers with the most adaptations and their median earnings
films_by_game_publisher <- game_films %>%
  filter(worldwide_box_office_currency == "$") %>% #I attempted to convert all JPY values into USD using priceR but I couldn't get it to work so I decided to drop all JPY value to avoid skewing the results
  drop_na(original_game_publisher) %>%
  group_by(original_game_publisher) %>%
  summarize(
    count = n(),
    average_earnings = median(worldwide_box_office, na.rm = TRUE)
    ) %>%
  filter(average_earnings != 0) %>%
  arrange(desc(count))

films_by_game_publisher_plot <- films_by_game_publisher %>%
  ggplot(aes(x = count, y = average_earnings)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = original_game_publisher)) +
  labs(title = "Game publishers by number of adaptations and median earnings", x = "Number of adaptations", y = "Median earnings")

films_by_game_publisher_plot  

#Comparing Metacritic and RottenTomatoes score
rated_films <- game_films %>%
  drop_na(rotten_tomatoes, metacritic) %>%
  arrange(desc(original_game_publisher))

most_common_publishers <- films_by_game_publisher %>%
  pull(original_game_publisher) %>%
  head(5)

rated_films_plotting <- rated_films %>%
  mutate(publisher = ifelse(original_game_publisher %in% most_common_publishers, original_game_publisher, "Others")) %>%
  mutate(publisher = fct_relevel(publisher, "Others", after = Inf)) #Groups everything other than the most common publishers into an "Others" category
  
rated_films_plot <- rated_films_plotting %>%
  ggplot(aes(x = metacritic, y = rotten_tomatoes, color = publisher)) +
  geom_label_repel(aes(label = title), show.legend = FALSE) + 
  geom_point(position = "jitter") +
  theme(legend.position = "bottom") +
  labs(title = "Metacritic and RottenTomatoes scores", x = "RottenTomatoes", y = "Metacritic", color = "Publisher")

# rated_films_plot
