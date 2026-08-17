if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-08-11')

palomar_emission_lines <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-11/palomar_emission_lines.csv') %>%
  as_tibble()
palomar_survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-11/palomar_survey.csv') %>%
  as_tibble()

#Nuclear Activity and Morphology

morphology_activity <- palomar_survey %>%
  filter(hubble_type != "S0") %>% #Filters out lenticular galaxies
  mutate(hubble_type = str_sub(hubble_type, start = 1, end = 1)) %>%
  filter(hubble_type == "S" | hubble_type == "E") %>% #Filters for spiral and elliptical galaxies 
  select(hubble_type, activity_type) %>%
  mutate(hubble_type = as.factor(hubble_type),
         activity_type = as.factor(activity_type)) %>%
  mutate(hubble_type = if_else(hubble_type == "S", "Spiral", "Elliptical"))

morphology_activity_plot <- morphology_activity %>%
  ggplot(aes(x = activity_type)) +
  geom_bar() +
  facet_wrap(~hubble_type) +
  labs(x = "Activity Type",
       y = "Frequency",
       title = "Nuclear Activity and Galaxy Morphology")

morphology_activity_plot

#Velocity Dispersion and Nuclear Activity

velocity_activity <- palomar_survey %>%
  select(activity_type, velocity_dispersion_km_s) %>%
  mutate(activity_type = as.factor(activity_type))

velocity_activity_plot <- velocity_activity %>%
  ggplot(aes(x = activity_type, y = velocity_dispersion_km_s)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Activity Type",
       y = "Velocity Dispersion (km/s)",
       title = "Velocity Dispersion and Nuclear Activity")

velocity_activity_plot  

#BPT Diagnostic Diagram
joined_palomar <- inner_join(palomar_survey, palomar_emission_lines, by = "galaxy_name") %>%
  filter(activity_type == "H II" | activity_type == "Seyfert" | activity_type == "LINER")

bpt_diagram <- joined_palomar %>%
  ggplot(aes(x = nii_6583, y = oiii_5007 / h_beta, shape = factor(activity_type))) +
  geom_point() +
  scale_shape_manual(values = c(1, 17, 16)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "[N II] 6583 / H-alpha 6563",
       y = "[O III] 5007 / H-beta 4861",
       title = "BPT Diagnostic Diagram",
       shape = "Activity Type")

bpt_diagram
