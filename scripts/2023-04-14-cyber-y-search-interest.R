# Data downloaded from: https://trends.google.com/trends/explore?date=2019-01-01%202023-03-31&geo=US&q=cybertruck,model%20y&hl=en
library(here)
library(tidyverse)
library(glue)
library(ggtext)

here("data", "2023-04-14-cyber-y-search-interest.csv") |> 
  read_csv(skip = 2, show_col_types = FALSE) |> 
  rename(week = 1, cybertruck = 2, model_y = 3) |> 
  mutate(
    week = mdy(week),
    cybertruck = ifelse(cybertruck == "<1", 0, cybertruck) |> as.numeric()
  ) ->
  search_interest

search_interest |> 
  pivot_longer(
    cols = cybertruck:model_y,
    names_to = "keyword",
    values_to = "hits"
  ) ->
  search_interest

search_interest |> 
  ggplot(aes(week, hits, color = keyword)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  expand_limits(y = c(0, 100)) +
  scale_color_manual(values = c("#6A6869", "#cc0000")) +
  labs(
    title =
      "Search interest for the <span style='color:#cc0000;'>Model Y</span> 
    and <span style='color:#6A6869;'>Cybertruck</span>",
    caption = "**Data**: Google Trends.<br/>**Script**: github.com/jeffboichuk/google-trends-analysis/scripts/2024-04-14-cyber-y-search-interest.R."
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title          = element_markdown(face = "bold", size = 9),
    axis.title          = element_blank(),
    axis.text           = element_text(size = 8), 
    legend.position     = "none",
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    plot.caption        = element_markdown(
      size       = 5, 
      lineheight = 1.25, 
      color      = "gray"
    )
  )

ggsave(
  filename = here("outputs", glue("{today()}-cyber-y-search-interest.jpeg")), 
  width    = 2048, 
  height   = 1024, 
  units    = "px", 
  dpi      = 300
)

search_interest |> 
  filter(week >= as_date("2021-01-01")) |> 
  group_by(keyword) |> 
  summarize(hits = mean(hits)) |> 
  mutate(cyber_to_y_ratio = hits / lead(hits))
