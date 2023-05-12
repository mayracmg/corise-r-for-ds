file_path_names <- here::here('data/names.csv.gz')
tbl_names <- readr::read_csv(file_path_names, show_col_types = FALSE)
tbl_names

library(tidyverse)

tbl_names_top_100_female <- tbl_names |> 
  # Keep ONLY Female names
  filter(sex == "F") |> 
  # Group by name
  group_by(name) |> 
  # Summarize the total number of births by group
  summarize(nb_births = sum(nb_births), .groups = "drop") |> 
  # Slice the top 100 names by number of births
  slice_max(nb_births, n = 100) 

tbl_names_top_100_female

tbl_names_top_100_female_trends <- tbl_names |> 
  # Keep ONLY female names in the top 100
  filter(name %in% tbl_names_top_100_female$name) |> 
  # Group by year
  group_by(year) |> 
  # Add dense rank for number of births in descending order
  mutate(rank = dense_rank(desc(nb_births))) |> 
  # Keep ONLY rows that are in the top 20 ranks
  filter(rank <= 20)

tbl_names_top_100_female_trends

library(gganimate)
library(ggplot2)


anim <- tbl_names_top_100_female_trends |> 
  # Filter for rows after the year 2000
  filter(year > 2000) |> 
  # Create a column plot of rank vs. nb_births
  ggplot(aes(x = nb_births, y = fct_rev(factor(rank)))) +
  geom_col(aes(fill = name), show.legend = FALSE) +
  geom_text(
    aes(label = name),
    x = 0,
    hjust = 0,
    size = 5
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_null() +
  aes(group = name) +
  labs(
    title = 'Animated Barchart of Top Female Babynames',
    x = '# Births',
    y = NULL
  ) +
  theme_gray(base_size = 16) +
  theme(
    plot.title.position = 'plot',
    axis.ticks = element_blank(),
    axis.text.y = element_blank()
  ) +
  transition_time(year) +
  ease_aes('cubic-in-out') +
  labs(subtitle = "Year: {round(frame_time)}")

gganimate::animate(anim, fps = 5, width = 600, height = 800)