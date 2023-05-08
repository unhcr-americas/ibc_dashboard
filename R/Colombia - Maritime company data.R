library(tidyverse)
library(httr)

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRP7KhswbG3dgklLdNFB9OPcadfD5-aQ-86Lx80NdEbN1oLBX8A6bUQfUXDNBDSJeiAZi3yP4DAb2Na/pubchart?oid=1202003462&format=interactive"

r <- GET(url) |> content(as = "text")

chart_json <- str_replace_all(str_match(r, "'chartJson': '(.+?)'")[,2],
                        c("\\\\x7b" = "{",
                          "\\\\x22" = "\"",
                          "\\\\x5b" = "[",
                          "\\\\x5d" = "]",
                          "\\\\x7d" = "}")) |> jsonlite::fromJSON()

data <- 
  map_dfr(chart_json$dataTable$rows$c, 
          ~tibble(pop = chart_json$dataTable$cols$label, 
                  n = .$f)) |> 
  mutate(date = if_else(pop == "", n, NA)) |> 
  fill(date) |> 
  filter(pop != "") |> 
  transmute(date = lubridate::dmy(date), 
            pop, 
            n = as.numeric(n))
