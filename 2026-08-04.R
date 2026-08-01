if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-08-04')

basotho_wool <- tuesdata$basotho_wool

#Mean Basotho Wool Imports by Country

country_mean <- basotho_wool %>%
  mutate(Country = reporter_desc) %>%
  group_by(Country) %>%
  summarise(Quantity = mean(net_wgt)) %>%
  mutate(Country = fct_reorder(Country, Quantity, .desc = TRUE))
  
country_mean_plot <- country_mean %>%
  ggplot(aes(x = Country, y = Quantity)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Mean Basotho Wool Imports by Country",
       y = "Quantity (kg)")

country_mean_plot

#Change in Each Country's Imports of Basotho Wool Over Time

country_time <- basotho_wool %>%
  group_by(reporter_desc, ref_year) %>%
  summarise(quantity = sum(net_wgt))

country_time_plot <- country_time %>%
  ggplot(aes(x = ref_year, y = quantity, colour = reporter_desc)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Change in Basotho Wool Imports Over Time",
       x = "Year",
       y = "Quantity (kg)",
       colour = "Country")

country_time_plot

#Monthly Quantity and Revenue in 2024

quantity_revenue_monthly_2024 <- basotho_wool %>%
  mutate(cifvalue = replace_na(cifvalue, 0),
         fobvalue = replace_na(fobvalue, 0),
         total_monetary_value = cifvalue + fobvalue) %>%
  filter(ref_year == 2024) %>%
  group_by(ref_month) %>%
  summarise(quantity = sum(net_wgt),
            revenue = sum(total_monetary_value))

quantity_revenue_plot <- quantity_revenue_monthly_2024 %>% 
  ggplot(aes(x = ref_month)) +
  geom_col(aes(y = quantity)) +
  geom_line(aes(y = revenue / 10), col = "red") +
  scale_y_continuous(
    name = "Quantity (kg)",
    sec.axis = sec_axis(trans = ~. * 10, name = "Revenue (US$)", labels = scales::label_comma()),
    labels = scales::label_comma()
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme(axis.title.y.right = element_text(colour = "red")) +
  labs(title = "Monthly Quantity and Revenue in 2024",
       x = "Month")
  

quantity_revenue_plot
