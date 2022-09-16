library(tidyverse)
library(MetBrewer)
library(glue)
library(ggtext)

all_data <- read_rds("data/all_data.rda")

colors <- met.brewer("Demuth")


all_data$race_totals |> 
  filter(year == 2021) |> 
  ggplot(aes(x = reorder(label, diff), y = diff)) +
  geom_col(fill = colors[9], width = .5) +
  scale_x_discrete(labels = label_wrap_gen()) +
  scale_y_continuous(labels = label_comma(), limits = c(-70000, 70000),
                     breaks = seq(from = -60000, to = 60000, by = 30000)) +
  scale_fill_met_c(name = "Demuth") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Kefa"),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        panel.background = element_rect(fill = NA, color = "grey30"),
        plot.title = element_textbox(size = 28,
                                     margin = margin(l = 10, t = 5)),
        plot.subtitle = element_textbox(color = "grey40", size = 14,
                                     margin = margin(t = 10, b = 20, 
                                                     l = 10),
                                     width = unit(8, "in"), lineheight = 1.25),
        plot.caption = element_textbox(color = "grey50", size = 10, hjust = 0,
                                    margin = margin(t = 20, l = 10, b = 5),
                                    lineheight = 1.25),
        plot.caption.position = "plot") +
  labs(title = "Milwaukee Net changes: Race/Ethnicity",
       subtitle = glue("Respondents identifying as two or more races increased the most, ",
                       "while respondents identifying as white alone decreased the most."),
       x = "",
       y = "Net change from 2019 to 2021",
       caption = glue("Data: American Community Survey 1-Year Estimates for Milwaukee, Wisconsin<br>",
                      "Analysis: Spencer Schien (@MrPecners)"))

ggsave("plots/race_subgroups.png", bg = "white", width = 9, height = 5)
