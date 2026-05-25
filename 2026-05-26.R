if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-05-26')

energy_cleaned <- tuesdata$energy_cleaned %>%
  as_tibble()

#Singapore energy data
sg_energy <- energy_cleaned %>%
  filter(country_name == "Singapore") %>%
  mutate(renewables_pct = share_of_renewable_capacity_in_total_capacity_pct)

sg_renewable_capacity_plot <- sg_energy %>%
  ggplot(aes(x = yr, y = renewables_pct)) +
  geom_line() +
  labs(
    title = "Share of renewable capacity in Singapore's total energy capacity",
    x = "Year",
    y = "% of renewable capacity in Singpore's total energy capacity"
  )

sg_renewable_capacity_plot

#Changes in renewable usage
renewable_types <- energy_cleaned %>%
  filter(country_name == "World") %>%
  select(yr, geothermal_energy_consumption_tfec_pct:modern_biomass_energy_consumption_tfec_pct, solar_energy_consumption_tfec_pct) %>%
  select(year = yr, contains("tfec_pct"))

#Converting to long data to make graphing easier
renewable_types_long <- renewable_types %>%
  pivot_longer(cols = ends_with("tfec_pct")) %>%
  mutate(name = gsub("_consumption_tfec_pct", "", name)) %>%
  mutate(name = gsub("_", " ", name)) %>%
  mutate(name = str_to_title(name))
  
colour_values = c("Geothermal Energy" = "red", 
                  "Liquid Biofuels Energy" = "green", 
                  "Modern Biomass Energy" = "orange",
                  "Hydro Energy" = "turquoise",
                  "Marine Energy" = "blue",
                  "Solar Energy" = "#EDC001")

renewable_types_long %>% ggplot(aes(x = year, y = value, colour = name)) +
  geom_line(position=position_dodge(width=0.2)) +
  scale_color_manual(values = colour_values) + 
  labs(
    title = "Change in global consumption rates of different types of renewable energy",
    subtitle = "From 1990 to 2010",
    x = "Year",
    y = "% of energy consumed by end-users",
    color = "Energy type"
  ) +
  theme(legend.position = "bottom")

