if(!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, tidyverse, magrittr, patchwork, GGally, ggplotify, tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2026-07-14')

penguin_data <- tuesdata$many_penguins %>%
  group_by(species, genus, shortname) %>%
  mutate(across(where(is.numeric),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>% #Replaces all NA values with the mean value of that column for the species
  ungroup()
  
#Correlation between all variables for each genus
penguin_correlations <- penguin_data %>%
  group_split(genus) %>% #Splits the dataframe into a list with each genus being its own dataframe
  lapply(function(df) {
    df %>% 
      select(beak.length_culmen:tail.length) %>%
      ggpairs(title = unique(df$genus)) #Uses ggpairs to generate correlation plots between each variable
  })

penguin_correlations

#3D visualisation of the relationship between culmen length, beak width and beak depth
pacman::p_load(rgl)

penguin_beak_data <- penguin_data %>%
  select(shortname, beak.length_culmen, beak.depth, beak.width) %>%
  mutate(Culmen_Length = beak.length_culmen,
         Beak_Depth = beak.depth,
         Beak_Width = beak.width)

colours <- colours() %>% #Uses random colours for each species
  sample(18) %>%
  as_tibble() 

penguins_colours <- penguin_beak_data %>%
  distinct(shortname) %>%
  cbind(colours)

#Generates the 3D plot
open3d(windowRect = c(100, 100, 1600, 1000)) #Expands the window size to increase white space and reduce the size of the legend

with(penguin_beak_data, plot3d(Culmen_Length, Beak_Depth, Beak_Width,
       type = "s", radius = 0.5, col = penguins_colours$value))

legend3d("bottomleft",
         legend = penguins_colours$shortname,
         col = penguins_colours$value, 
         pch = 16,
         title = "Species")
