if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-07-28')

occurrences <- tuesdata$occurrences
tourism <- tuesdata$tourism
weather <- tuesdata$weather

#Gouldian Finches and Weather

gouldian_finch_occurrences <- occurrences %>%
  filter(organism_name == "Gouldian finch") %>%
  drop_na() %>%
  group_by(month) %>%
  count()

mean_temp <- weather %>%
  group_by(month) %>%
  summarize(avg_temp = mean(temp, na.rm = TRUE))

occurrences_temp <- ggplot() +
  geom_col(data = gouldian_finch_occurrences, aes(x = month, y = n)) +
  geom_line(data = mean_temp, aes(x = month, y = avg_temp * 20), col = "red") +
  scale_y_continuous(
    name = "Observations",
    sec.axis = sec_axis(transform = ~. / 20, name = "Mean Temperature (°C)")
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme(axis.title.y.right = element_text(colour = "red")) +
  labs(title = "Temperature",
       x = "Month")

rain <- weather %>%
  group_by(month) %>%
  summarize(mean_precip = mean(prcp, na.rm = TRUE))

occurrences_rain <- ggplot() +
  geom_col(data = gouldian_finch_occurrences, aes(x = month, y = n)) +
  geom_line(data = rain, aes(x = month, y = mean_precip * 20), col = "cyan") +
  scale_y_continuous(
    name = "Observations",
    sec.axis = sec_axis(transform = ~. / 20, name = "Mean Rainfall (mm)")
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme(axis.title.y.right = element_text(colour = "cyan")) +
  labs(title = "Rainfall",
       x = "Month")

rh <- weather %>%
  group_by(month) %>%
  summarize(avg_rh = mean(rh, na.rm = TRUE))

occurrences_rh <- ggplot() +
  geom_col(data = gouldian_finch_occurrences, aes(x = month, y = n)) +
  geom_line(data = rh, aes(x = month, y = avg_rh * 10), col = "orange") +
  scale_y_continuous(
    name = "Observations",
    sec.axis = sec_axis(transform = ~. / 10, name = "Mean Relative Humidity (%)")
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme(axis.title.y.right = element_text(colour = "orange")) +
  labs(title = "Relative Humidity",
       x = "Month")

occurrences_rh

mean_ws <- weather %>%
  group_by(month) %>%
  summarize(avg_ws = mean(wind_speed, na.rm = TRUE))

occurrences_wind <- ggplot() +
  geom_col(data = gouldian_finch_occurrences, aes(x = month, y = n)) +
  geom_line(data = mean_ws, aes(x = month, y = avg_ws * 100), col = "green") +
  scale_y_continuous(
    name = "Observations",
    sec.axis = sec_axis(transform = ~. / 100, name = "Mean Wind Speed (m/s)")
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme(axis.title.y.right = element_text(colour = "green")) +
  labs(title = "Wind Speed",
       x = "Month")

occurrences_temp / occurrences_rain / occurrences_rh / occurrences_wind + plot_layout(axes = "collect") + plot_annotation(title = "Gouldian Finches and Weather Conditions")

#Species and Tourist Numbers

#For more meaningful analysis, I decided to analyse tourist arrivals in the location (ws_id) where each species has the most observations (Except for manta rays)

glowworm_observations <- occurrences %>%
  filter(organism_name == "Glowworm", ws_id == "945820-99999") %>% #945820-99999 has the most observations
  mutate(quarter = ceiling(month / 3)) %>%
  group_by(quarter) %>%
  count()
  
glowworm_arrivals <- tourism %>%
  filter(ws_id == "945820-99999", purpose == "Holiday") %>%
  group_by(quarter) %>%
  summarize(total_arrivals = sum(trips, na.rm = TRUE))

glowworm_plot <- ggplot() +
  geom_col(data = glowworm_arrivals, aes(x = quarter, y = total_arrivals)) +
  geom_line(data = glowworm_observations, aes(x = quarter, y = n * 10), col = "orange") +
  scale_y_continuous(
    name = "Tourist Arrivals (k)",
    sec.axis = sec_axis(transform = ~. / 10, name = "Observations")
  ) +
  theme(axis.title.y.right = element_text(colour = "orange")) +
  labs(title = "Glowworm (Murwillumbah, Murwillumbah Surrounds and Pottsville)",
       x = "Quarter")

finch_observations <- occurrences %>%
  filter(organism_name == "Gouldian finch", ws_id == "941310-99999") %>%
  mutate(quarter = ceiling(month / 3)) %>%
  group_by(quarter) %>%
  count()

finch_arrivals <- tourism %>%
  filter(ws_id == "941310-99999", purpose == "Holiday") %>%
  group_by(quarter) %>%
  summarize(total_arrivals = sum(trips, na.rm = TRUE))

finch_plot <- ggplot() +
  geom_col(data = finch_arrivals, aes(x = quarter, y = total_arrivals)) +
  geom_line(data = finch_observations, aes(x = quarter, y = n), col = "cyan") +
  scale_y_continuous(
    name = "Tourist Arrivals (k)",
    sec.axis = sec_axis(transform = ~., name = "Observations")
  ) +
  theme(axis.title.y.right = element_text(colour = "cyan")) +
  labs(title = "Gouldian Finch (Elsey and Katherine)",
       x = "Quarter")

orchid_observations <- occurrences %>%
  filter(organism_name == "Orchid", ws_id == "946300-99999") %>%
  mutate(quarter = ceiling(month / 3)) %>%
  group_by(quarter) %>%
  count()

orchid_arrivals <- tourism %>%
  filter(ws_id == "946300-99999", purpose == "Holiday") %>%
  group_by(quarter) %>%
  summarize(total_arrivals = sum(trips, na.rm = TRUE))

orchid_plot <- ggplot() +
  geom_col(data = orchid_arrivals, aes(x = quarter, y = total_arrivals)) +
  geom_line(data = orchid_observations, aes(x = quarter, y = n / 10), col = "green") +
  scale_y_continuous(
    name = "Tourist Arrivals (k)",
    sec.axis = sec_axis(transform = ~. * 10, name = "Observations")
  ) +
  theme(axis.title.y.right = element_text(colour = "green")) +
  labs(title = "Orchid (Plantagenet and Stirling Range National Park)",
       x = "Quarter")

orchid_plot

manta_observations <- occurrences %>%
  filter(organism_name == "Manta ray") %>%
  mutate(quarter = ceiling(month / 3)) %>%
  group_by(quarter) %>%
  count()

manta_arrivals <- tourism %>% 
  filter(purpose == "Holiday", between(lon, 119, 153.6444), between(lat, -33.93000, -10.60212)) %>% #The specific ws_ids for the manta ray observations don't seem to exist in tourism.csv, so I decided to use data for all ws_ids that are within the lat-long of the manta ray observations
  group_by(quarter) %>%
  summarize(total_arrivals = sum(trips, na.rm = TRUE))

manta_plot <- ggplot() +
  geom_col(data = manta_arrivals, aes(x = quarter, y = total_arrivals)) +
  geom_line(data = manta_observations, aes(x = quarter, y = n * 100), col = "blue") +
  scale_y_continuous(
    name = "Tourist Arrivals (k)",
    sec.axis = sec_axis(transform = ~. / 100, name = "Observations")
  ) +
  theme(axis.title.y.right = element_text(colour = "blue")) +
  labs(title = "Manta Ray (National)",
       x = "Quarter")

glowworm_plot / finch_plot / orchid_plot / manta_plot + plot_layout(axes = "collect") + plot_annotation(title = "Species and Tourist Arrivals")
