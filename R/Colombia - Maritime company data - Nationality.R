library(tidyverse)
library(httr)
library(activityinfo)


# data source -------------------------------------------------------------


url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRP7KhswbG3dgklLdNFB9OPcadfD5-aQ-86Lx80NdEbN1oLBX8A6bUQfUXDNBDSJeiAZi3yP4DAb2Na/pubchart?oid=1202003462&format=interactive"
activityinfo_form <- "cxkingzlhgowkuf6"

# extract data ------------------------------------------------------------


r <- GET(url) |> content(as = "text")

chart_json <- str_replace_all(str_match(r, "'chartJson': '(.+?)'")[,2],
                        c("\\\\x7b" = "{",
                          "\\\\x22" = "\"",
                          "\\\\x5b" = "[",
                          "\\\\x5d" = "]",
                          "\\\\x7d" = "}")) |> jsonlite::fromJSON()


# arrange data ------------------------------------------------------------


data <- 
  map_dfr(chart_json$dataTable$rows$c, 
          ~tibble(pop = chart_json$dataTable$cols$label, 
                  n = .$f)) |> 
  mutate(date = if_else(pop == "", n, NA)) |> 
  fill(date) |> 
  filter(pop != "") |> 
  transmute(date = lubridate::dmy(date), 
            nationality = pop, 
            people = as.numeric(n),
            source = "GIFMM - Maritime company data")



# check for duplicates ----------------------------------------------------

online_df <- getRecords(activityinfo_form) |> 
  select(date, nationality, people, source) |> 
  mutate(date = lubridate::ymd(date)) |> 
  as_tibble() 

data <- data |> 
  anti_join(online_df)


# send data to activityinfo -----------------------------------------------

if (nrow(data) > 0) importRecords(formId = activityinfo_form, data, stageDirect = TRUE)


# ACTIVITYINFO_IBC = "2a36531bbd3ce6daff18d003780a280c"