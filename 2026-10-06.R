if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, RColorBrewer, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-10-06') 

#Identifying Pure and Adulterated Samples

#I decided to use palmitoleic acid to represent fatty acids and beta-sitosterol to represent sterols

avocado_oil <- tuesdata$avocado_oil_bottles %>%
  as_tibble()

avocado_oil_plot <- avocado_oil %>%
  ggplot(aes(x = c16_1_palmitoleic_pct, y = beta_sitosterol_pct, colour = purity_result)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Beta-sistosteral against Palmitoleic Acid",
       x = "Palmitoleic Acid as % of Fatty Acids",
       y = "Beta-sitosteral as % of Total Sterols",
       colour = "Purity Result") 

avocado_oil_plot

#Price and Authenticity for Bottled Oils

avocado_price_authencity_plot <- avocado_oil %>%
  mutate(purity_result = str_to_sentence(purity_result)) %>%
  ggplot(aes(x = cost_per_fl_oz, y = purity_result, fill = purity_result)) +
  geom_boxplot() +
  labs(title = "Authenticity and Cost",
       x = "Cost per Fluid Ounce ($US)",
       y = "Purity Result",
       fill = "Purity Result")

avocado_price_authencity_plot

#Authenticity of Olive v. Avocado Oil
avocado_olive <- tuesdata$avocado_oil_processed_foods %>%
  as_tibble() %>%
  select(oil_type, authentic) %>%
  group_by(oil_type) %>%
  count(authentic)
  
avocado_olive_plot <- avocado_olive_plot <- avocado_olive %>%
  ggplot(aes(x = oil_type, y = n, fill = authentic)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "% of Authentic Products by Oil Type",
       x = "Oil Type",
       y = "% of Authentic Products",
       fill = "Authenticity")

avocado_olive_plot
