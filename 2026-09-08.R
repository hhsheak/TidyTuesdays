if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-09-08') 

#Variance in Price of a Small Cappucino
cappucino_country <- tuesdata$cafe %>%
  as_tibble() %>%
  group_by(country) %>%
  summarise(avg_price = mean(price_gbp)) %>%
  slice_max(avg_price, n = 10) %>%
  pull(country) #Finds the cities with the highest mean cappucino price

cappucino_price <- tuesdata$cafe %>%
  as_tibble() %>%
  filter(country %in% cappucino_country) %>%
  group_by(country) %>%
  mutate(avg_price = mean(price_gbp)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, avg_price, .desc = TRUE))

cappucino_price_plot <- cappucino_price %>%
  ggplot(aes(x = country, y = price_gbp)) +
  geom_boxplot() +
  labs(x = "Country",
       y = "Price (GBP)",
       title = "Price of a Small Cappucino")

cappucino_price_plot

#Wage, Price and Locality
cappucino_loc_transformed <- tuesdata$cafe %>%
  as_tibble() %>%
  mutate(location = case_when(urban == TRUE ~ "Urban",
                              suburban == TRUE ~ "Suburban",
                              rural == TRUE ~ "Rural"))

wage_price_loc_plot <- cappucino_loc_transformed %>%
  ggplot(aes(x = price_gbp, y = hourly_wage_gbp, colour = location)) +
  geom_jitter() +
  geom_smooth(method = "lm", show.legend = FALSE) +
  theme(
    legend.background = element_blank(),
    legend.key = element_blank()
  ) +
  labs(x = "Price (GBP)",
       y = "Hourly Wage (GBP)",
       colour = "Location",
       title = "Relationship between Price, Hourly Wage and Location")


wage_price_loc_plot

#Cappucino Index by Country
cappucino_index <- tuesdata$cappuccino_index %>%
  as_tibble() %>%
  mutate(tier = ntile(n, 5)) %>%
  slice_max(index, n = 10) %>%
  mutate(country = fct_reorder(country, index, .desc = TRUE))

cappucino_index_plot <- cappucino_index %>%
  ggplot(aes(x = country, y = index, fill = tier)) +
  geom_col() +
  scale_fill_continuous(transform = "reverse") +
  guides(fill = "none") +
  labs(x = "Country", 
       y = "Cappucino Index (Minutes)",
       title = "Top 10 Highest Cappucino Indexes",
       caption= "Darker colours indicate a larger sample size")

cappucino_index_plot
