# 2020 data not available
age_data <- map_df(c(2011:2019, 2021), function(i) {
  d <- get_acs(geography = "place", year = i, survey = "acs1",
               variable = c(sprintf("B01001_0%02i", 3:25), sprintf("B01001_0%02i", 27:49)),
               state = "WI")
  
  d |>
    mutate(year = i)
})

ad <- map_df(unique(age_data$variable), function(i) {
  tmp <- age_data |> 
    filter(variable == i & NAME == "Milwaukee city, Wisconsin") |> 
    arrange(year)
  
  map_df(2:(nrow(tmp)), function(j) {
    diff <- tmp[[j, "estimate"]] - tmp[[j-1, "estimate"]]
    perc <- diff / tmp[[j-1, "estimate"]]
    tibble(
      name = tmp[[j, "NAME"]],
      diff = diff,
      perc = perc,
      year = tmp[[j, "year"]],
      variable = i
    )
  })
  
}) |> 
  left_join(vars, by = c("variable" = "name")) |> 
  mutate(sex = ifelse(str_detect(label, "!Male"), "Male", "Female"),
         label = str_remove(label, ".*!!"),
         label = str_remove(label, ":"),
         label = str_remove(label, "alone"),
         label = str_trim(label))

age_levels <- c(
  "Under 5 years",
  "5 to 9 years",
  "10 to 14 years",
  "15 to 17 years",
  "18 and 19 years",
  "20 years",
  "21 years",
  "22 to 24 years",
  "25 to 29 years",
  "30 to 34 years",
  "35 to 39 years",
  "40 to 44 years",
  "45 to 49 years",
  "50 to 54 years",
  "55 to 59 years",
  "60 and 61 years",
  "62 to 64 years",
  "65 and 66 years",
  "67 to 69 years",
  "70 to 74 years",
  "75 to 79 years",
  "80 to 84 years",
  "85 years and over"
)

colors <- met.brewer("Demuth")


labelled <- ad |> 
  filter(year == "2021") |> 
  mutate(label = factor(label, levels = age_levels),
         decade = case_when(label %in% c("Under 5 years", "5 to 9 years") ~ "0-9",
                            str_detect(label, "^1") ~ "10-19",
                            str_detect(label, "^2") ~ "20-29",
                            str_detect(label, "^3") ~ "30-39",
                            str_detect(label, "^4") ~ "40-49",
                            str_detect(label, "^5") ~ "50-59",
                            str_detect(label, "^6") ~ "60-69",
                            str_detect(label, "^7") ~ "70-79",
                            str_detect(label, "^8") ~ "80+ years old"))

labelled |> 
  group_by(decade) |> 
  summarise(total_diff = sum(diff)) |> 
  ggplot(aes(decade, total_diff)) +
  geom_col(fill = colors[9], width = .5) +
  scale_y_continuous(labels = label_comma(),
                     lim = c(-14000, 14000),
                     breaks = seq(from = -14000, to = 14000, by = 3500)) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Kefa"),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = NA, color = "grey30"),
        strip.text = element_text(size = 14),
        plot.title.position = "plot",
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
  labs(title = "Milwaukee Net Changes: Age",
       subtitle = glue("The 20-29 year old age band saw the largest decline for both males and females. ",
                       "Children 0-9 years old saw the second largest decline."),
       x = "",
       y = "Net change from 2019 to 2021",
       caption = glue("Data: American Community Survey 1-Year Estimates for Milwaukee, Wisconsin<br>",
                      "Analysis: Spencer Schien (@MrPecners)"))


ggsave("plots/age_subgroups.png", bg = "white", width = 9, height = 7)

