library(tidyverse)
library(httr)
library(activityinfo)


# function ----------------------------------------------------------------

source(file = "R/fun.R")

# data source -------------------------------------------------------------


url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRP7KhswbG3dgklLdNFB9OPcadfD5-aQ-86Lx80NdEbN1oLBX8A6bUQfUXDNBDSJeiAZi3yP4DAb2Na/pubchart?oid=1202003462&format=interactive"
activityinfo_form_daily <- "cxkingzlhgowkuf6"
activityinfo_form_monthly <- "cr55i4zlibuu4uxh"

# extract data ------------------------------------------------------------


r <- GET(url) |> content(as = "text")

chart_json <- str_replace_all(str_match(r, "'chartJson': '(.+?)'")[,2],
                        c("\\\\x7b" = "{",
                          "\\\\x22" = "\"",
                          "\\\\x5b" = "[",
                          "\\\\x5d" = "]",
                          "\\\\x7d" = "}")) |> jsonlite::fromJSON()


# arrange daily data ------------------------------------------------------

data_daily <- 
  map_dfr(chart_json$dataTable$rows$c, 
          ~tibble(pop = chart_json$dataTable$cols$label, 
                  n = .$f)) |> 
  mutate(date = if_else(pop == "", n, NA)) |> 
  fill(date) |> 
  filter(pop != "") |> 
  transmute(date = lubridate::dmy(date), 
            nationality = pop, 
            iso3c = isoccode(nationality, origin = "spanish"),
            people = as.numeric(n),
            source = "GIFMM - Maritime company data")



# arrange monthly data ----------------------------------------------------

data_monthly <- 
  map_dfr(chart_json$dataTable$rows$c, 
          ~tibble(pop = chart_json$dataTable$cols$label, 
                  n = .$f)) |> 
  mutate(date = if_else(pop == "", n, NA)) |> 
  fill(date) |> 
  filter(pop != "") |> 
  transmute(date = lubridate::dmy(date), 
            nationality = pop, 
            iso3c = isoccode(nationality, origin = "spanish"),
            people = as.numeric(n),
            source = "GIFMM - Maritime company data") |> 
  group_by(date = lubridate::floor_date(date, "month"), nationality, iso3c, source) |> 
  summarise(people = sum(people, na.rm = TRUE)) |> 
  select(date,
         nationality,
         iso3c,
         people,
         source)
  


# check for duplicates ----------------------------------------------------

online_df_daily <- getRecords(activityinfo_form_daily) |> 
  select(date, people, source) |> 
  mutate(date = lubridate::ymd(date)) |> 
  as_tibble() 

data_daily <- data_daily |> 
  anti_join(online_df_daily)



online_df_monthly <- getRecords(activityinfo_form_monthly) |> 
  select(date, people, source) |> 
  mutate(date = lubridate::ymd(date)) |> 
  as_tibble() 

data_monthly <- data_monthly |> 
  anti_join(online_df_monthly)

# send data to activityinfo -----------------------------------------------

if (nrow(data_daily) > 0) importRecords(formId = activityinfo_form_daily, data_daily, stageDirect = TRUE)

if (nrow(data_monthly) > 0) importRecords(formId = activityinfo_form_monthly, data_monthly, stageDirect = TRUE)




