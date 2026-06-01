if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, lubridate, ggrepel, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-06-02')

eplp <- tuesdata$eplp %>%
  as_tibble() %>%
  mutate(mat_m_ld_tt = mat_m_ld_bb + mat_m_ld_ab)

#Changes in maternity leave
maternity_leave_change <- eplp %>%
  select(country:year, mat_m_ld_bb:mat_m_ld_ab, mat_m_ld_tt) %>%
  group_by(country) %>%
  summarize(bb_change = last(mat_m_ld_bb) - first(mat_m_ld_bb),
            ab_change = last(mat_m_ld_ab) - first(mat_m_ld_ab),
            tt_change = last(mat_m_ld_tt) - first(mat_m_ld_tt))

maternity_leave_change_plot <- maternity_leave_change %>%
  ggplot(aes(x = country, y = tt_change)) +
  geom_point(color = ifelse(maternity_leave_change$tt_change > 0, "green", ifelse(maternity_leave_change$tt_change < 0, "red", "black"))) +
  labs(title = "Change in maternity leave between 1970 and 2024",
       y = "Change") +
  theme(axis.title.x = element_blank(), legend.position = "none")

maternity_leave_change_plot

#Year of introduction of co-parent leave
co_leave <- eplp %>%
  select(country:year, co_ld) %>%
  filter(co_ld != 0) %>%
  group_by(country) %>%
  summarize(co_introduction = first(year)) %>%
  mutate(co_introduction = ymd(co_introduction, truncated = 2L))

co_leave_plot <- co_leave %>%
  ggplot(aes(x = co_introduction, y = "Year of Introduction", label = country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(direction = "y",
                  point.padding = 0.5,
                  hjust = 0,
                  box.padding = 1,
                  size = 3,
                  seed = 2,
                  max.overlaps = Inf) +
  scale_x_date(name = "", date_breaks = "10 years") +
  scale_y_discrete(name = "") +
  theme_minimal() #Taken from https://www.r4photobiology.info/galleries/plot-timeline.html

co_leave_plot
  
