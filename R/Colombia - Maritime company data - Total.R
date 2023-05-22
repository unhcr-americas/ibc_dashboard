library(tidyverse)
library(httr)
library(activityinfo)

# data source -------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRP7KhswbG3dgklLdNFB9OPcadfD5-aQ-86Lx80NdEbN1oLBX8A6bUQfUXDNBDSJeiAZi3yP4DAb2Na/pubchart?oid=1174121648&format=interactive"
activityinfo_form <- "c3z54hulhgoreh32"
  
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
          ~tibble(col_type = chart_json$dataTable$cols$type, 
                  n = .$f)) |> 
    mutate(date = n) |> 
    mutate(date = if_else(col_type  == "date", date, NA)) |> 
    mutate(n = if_else(col_type  == "number", n, NA)) |> 
    fill(date) |> 
    filter(!is.na(n)) |> 
    transmute(date = lubridate::dmy(date), 
            people = as.numeric(n),
            source = "GIFMM - Maritime company data")


# check for duplicates ----------------------------------------------------

online_df <- getRecords(activityinfo_form) |> 
  select(date, people, source) |> 
  mutate(date = lubridate::ymd(date)) |> 
  as_tibble() 

data <- data |> 
  anti_join(online_df)


# send data to activityinfo -----------------------------------------------

if (nrow(data) > 0) importRecords(formId = activityinfo_form, data, stageDirect = TRUE)





