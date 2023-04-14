# Data downloaded from: https://trends.google.com/trends/explore?date=2019-01-01%202023-03-31&geo=US&q=cybertruck,model%20y&hl=en
library(here)
library(tidyverse)
library(glue)
library(ggtext)
library(scales)

here("data", "2023-04-14-cyber-y-search-interest-by-state.csv") |> 
  read_csv(skip = 2, show_col_types = FALSE) |> 
  rename(region = 1, cybertruck = 2, model_y = 3) |> 
  mutate(
    cybertruck  = str_remove(cybertruck, "%") |> as.numeric() / 100,
    model_y     = str_remove(model_y, "%")    |> as.numeric() / 100,
  ) ->
  search_interest

search_interest |> 
  pivot_longer(
    cols = cybertruck:model_y,
    names_to = "keyword",
    values_to = "share"
  ) ->
  search_interest

search_interest |> 
  filter(keyword == "cybertruck") |> 
  arrange(share) |> 
  pull(region) ->
  region_levels

search_interest |> 
  # filter(region %in% region_levels) |> 
  mutate(region  = factor(region, levels = region_levels)) |> 
  ggplot(aes(x = region, y = share, fill = keyword)) +
  geom_col() +
  # facet_wrap(vars(state_type), scales = "free_y", ncol = 1) +
  coord_flip() +
  scale_fill_manual(values = c("#6A6869", "#cc0000")) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = label_percent()) +
  labs(
    title =
    "Search interest by state for the <span style='color:#cc0000;'>Model Y</span> 
    and <span style='color:#6A6869;'>Cybertruck</span>",
    caption = "**Data**: Google Trends.<br/>**Script**: github.com/jeffboichuk/google-trends-analysis/scripts/2024-04-14-cyber-y-search-interest-by-state.R."
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
  filename = here(
    "outputs", 
    glue("{today()}-cyber-y-search-interest-by-state.jpeg")
  ), 
  width    = 1024, 
  height   = 2048, 
  units    = "px", 
  dpi      = 300
)
