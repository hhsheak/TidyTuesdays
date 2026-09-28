if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, RColorBrewer, gganimate, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-09-22') 

#Change in Percentage of Green Area
increase <- tuesdata$urban %>%
  filter(year != 2025) %>% #Not all cities have values for 2025, so I used 2020 instead
  drop_na() %>%
  group_by(cityCode) %>%
  summarise(change = averageShareOfGreenAreaInCityUrbanAreaPct[year == 2020] - averageShareOfGreenAreaInCityUrbanAreaPct[year == 1990]) %>%
  slice_max(order_by = change, n = 10)

city_increase_data <- tuesdata$urban %>%
  filter(year != 2025) %>%
  inner_join(increase)

city_increase_data_plot <- city_increase_data %>%
  ggplot(aes(x = year, y = averageShareOfGreenAreaInCityUrbanAreaPct, colour = cityName)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Largest Increase in Average Share of Green Area in City Urban Area",
       x = "Year",
       y = "Average Share of Green Area in City Urban Area (%)",
       color = "City")

decrease <- tuesdata$urban %>%
  filter(year != 2025) %>%
  drop_na() %>%
  group_by(cityCode) %>%
  summarise(change = averageShareOfGreenAreaInCityUrbanAreaPct[year == 2020] - averageShareOfGreenAreaInCityUrbanAreaPct[year == 1990]) %>%
  slice_min(order_by = change, n = 10)

city_decrease_data <- tuesdata$urban %>%
  filter(year != 2025) %>%
  inner_join(decrease)

city_decrease_data_plot <- city_decrease_data %>%
  ggplot(aes(x = year, y = averageShareOfGreenAreaInCityUrbanAreaPct, colour = cityName)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Largest Decrease in Average Share of Green Area in City Urban Area",
       x = "Year",
       y = "Average Share of Green Area in City Urban Area (%)",
       color = "City")

city_increase_data_plot / city_decrease_data_plot + plot_layout(axes = "collect")

#Change in Rankings Over the Years
top_10_cities <- tuesdata$urban %>%
  filter(year != 2025) %>%
  drop_na(greenAreaPerCapitaM2, cityName) %>%
  group_by(year) %>%
  slice_max(order_by = greenAreaPerCapitaM2, n = 10) %>%
  ungroup() %>%
  mutate(cityName = fct_reorder(cityName, greenAreaPerCapitaM2))

top_10_plot <- top_10_cities %>%
  ggplot(aes(x = greenAreaPerCapitaM2, y = cityName, fill = sdgRegion)) +
  geom_col() +
  geom_text(aes(label = round(greenAreaPerCapitaM2, 1)), hjust = -0.1) +
  labs(
    title = "Cities with the Most Green Area per Capita",
    subtitle = "Year: {closest_state}",
    x = "Green area per capita (m²/person)",
    y = NULL,
    fill = "Region",
   ) +
  transition_states(year, transition_length = 5, state_length = 5) +
  theme_minimal()

animate(
  top_10_plot,
  nframes = 150,
  fps = 20,
  width = 900,
  height = 600
)

