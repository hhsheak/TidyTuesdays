if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-06-30')

wreck_inventory <- tuesdata$wreck_inventory

#Undiscovered wrecks
undiscovered_wrecks <- wreck_inventory %>%
  mutate(undiscovered = if_else(is.na(latitude), "Undiscovered", "Discovered")) %>% #If a wreck doesn't have a latitude (and correspondingly, longitude), I assume it to be undiscovered
  group_by(undiscovered) %>%
  count()

undiscovered_wrecks_plot <- undiscovered_wrecks %>%
  ggplot(aes(x = "", y = n, fill = undiscovered)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Number of Undiscovered Wrecks") +
  theme(legend.title = element_blank())

undiscovered_wrecks_plot

#Map
install.packages("ggOceanMaps")
pacman::p_load(sf, ggOceanMaps)

discovered_wrecks_location_sf <- wreck_inventory %>%
  drop_na() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ireland_map <- basemap(
  limits = c(-23, -3, 46, 58),
  bathymetry = TRUE,
)

wrecks_map <- ireland_map +
  geom_sf(data = discovered_wrecks_location_sf, alpha = 0.5) +
  labs(title = "Location of Shipwrecks around Ireland")

wrecks_map
