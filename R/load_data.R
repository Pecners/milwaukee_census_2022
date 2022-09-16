library(tidycensus)
library(tidyverse)

data <- get_acs(geography = "place", year = 2021, survey = "acs1", variable = "B01001_001", state = "WI")
vars <- load_variables(year = 2021, dataset = "acs1")

# 2020 data not available
tract_data <- map_df(c(2011:2019, 2021), function(i) {
  d <- get_acs(geography = "place", year = i, survey = "acs1",
               variable = c(sprintf("B02001_0%02i", 2:8), "B03003_003", "B02001_001"),
               state = "WI")
  
  d |>
    mutate(year = i)
})

td <- map_df(unique(tract_data$variable), function(i) {
  tmp <- tract_data |> 
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
  mutate(label = str_remove(label, ".*!!"),
         label = str_remove(label, ":"),
         label = str_remove(label, "alone"),
         label = str_trim(label))



total_diff <- td |> 
  filter(name == "Milwaukee city, Wisconsin" & variable == "B02001_001")

race_totals <- td |> 
  filter(name == "Milwaukee city, Wisconsin" & variable != "B02001_001")

all_data <- list(
  place_data = tract_data,
  total_diff = total_diff,
  race_totals = race_totals
)

saveRDS(all_data, "data/all_data.rda")
