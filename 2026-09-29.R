if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-09-29') 

#Per Capita Number of Hosptials and Pharmacies
per_capita <- tuesdata$health %>%
  select(GC_UCN_MAI_2025,HL_FPC_HOS_2025, HL_FPC_PHA_2025) %>%
  rename(name = GC_UCN_MAI_2025,
         hospital_pc = HL_FPC_HOS_2025,
         pharmacy_pc = HL_FPC_PHA_2025)

per_capita_hospitals <- per_capita %>%
  slice_max(order_by = hospital_pc, n = 10) %>%
  mutate(name = fct_reorder(name, hospital_pc, .desc = TRUE)) %>%
  ggplot(aes(x = name, y = hospital_pc)) +
  geom_col() +
  theme(axis.text.y = element_blank()) +
  labs(x = "Name",
       y = "Number of Hospitals Per Capita")

per_capita_pharmacies <- per_capita %>%
  slice_max(order_by = pharmacy_pc, n = 10) %>%
  mutate(name = fct_reorder(name, pharmacy_pc, .desc = TRUE)) %>%
  ggplot(aes(x = name, y = pharmacy_pc)) +
  geom_col() +
  theme(axis.text.y = element_blank()) +
  labs(x = "Name",
       y = "Number of Pharmacies Per Capita")

per_capita_hospitals / per_capita_pharmacies + plot_layout(axes = "collect") + plot_annotation(title = "Number of Hospitals and Pharmacies Per Capita")

#Income and Proximity to Hospitals and Pharmacies
income_proximity <- tuesdata$health %>%
  select(GC_DEV_WIG_2025, HL_SHP_HOS_2025, HL_SHP_PHA_2025) %>%
  rename(income = GC_DEV_WIG_2025,
         hospital_prox = HL_SHP_HOS_2025,
         pharmacy_prox = HL_SHP_PHA_2025) %>%
  drop_na() %>%
  mutate(income = fct_relevel(income, "High income", "Upper Middle", "Lower Middle", "Low income"))

income_proximity_plot <- income_proximity %>%
  ggplot(aes(x = hospital_prox, y = pharmacy_prox, colour = income)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.1) +
  labs(title = "Proximity to a Hospital and Pharmacy by Income Group",
       x = "Urban Centre Population Living Within 1 Km Buffer from a Hospital (%)",
       y = "Urban Centre Population Living Within 1 Km Buffer from a Pharmacy (%)")

income_proximity_plot

#Hospitals and Pharmacies
hospital_pharmacy <- tuesdata$health %>%
  select(GC_DEV_WIG_2025, HL_FCL_HOS_2024, HL_FCL_PHA_2024) %>%
  rename(income = GC_DEV_WIG_2025,
         hospital = HL_FCL_HOS_2024,
         pharmacy = HL_FCL_PHA_2024) %>%
  drop_na() %>%
  mutate(income = fct_relevel(income, "High income", "Upper Middle", "Lower Middle", "Low income"))

hospital_pharmacy_plot <- hospital_pharmacy %>%
  ggplot(aes(x = hospital, y = pharmacy, colour = income)) +
  geom_point() +
  geom_smooth(method = "lm")

hospital_pharmacy_plot
