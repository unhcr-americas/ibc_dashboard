library(tidyverse)
library(rvest)
library(activityinfo)

# function ----------------------------------------------------------------

source(file = "R/fun.R")

# activityinfo_dataset ----------------------------------------------------
activityinfo_form <- "c4ff2molhzbq41p2"

online_df <- getRecords(activityinfo_form) |> 
  select(id =`_id`,date, `fiscal year`, year , month , component, `land border region`, `area of responsability`, `area of responsability (abbreviation)`, state,
         demographic, nationality, iso3c,`title of authority`, `encounter type`, `encounter count`, source) |> 
  as_tibble() 
  
# retrieve data from cbp website ------------------------------------------

cbp_url <-  "https://www.cbp.gov/document/stats/nationwide-encounters"
session_cbp <- session(cbp_url)

list_element_a <- html_elements(session_cbp, "a")
list_href <- html_attr(list_element_a, "href")
list_path_csv <- keep(list_href, \(x) str_detect(x, "csv"))
recent_csv_nationwide <- first(list_path_csv)

url_csv <- httr::modify_url(cbp_url, path=recent_csv_nationwide)
data_raw <- read_csv(url_csv)


# data wrangle ------------------------------------------------------------


datanew <- data_raw %>%
  mutate(`Fiscal Year` = parse_number(`Fiscal Year`)) |> 
  mutate(calendar_year = if_else(
    `Month (abbv)` %in%
      c("OCT", "NOV", "DEC"),
    as.numeric(`Fiscal Year`) - 1,
    as.numeric(`Fiscal Year`)),
    month_name = month(parse_date(paste0("01 ", `Month (abbv)`,  " 2022"),"%d %b %Y"), label = TRUE, abbr = FALSE)
    ) |> 
transmute(
  date = as.character(as_date(paste0("01-",month_name,"-",calendar_year), format = "%d-%B-%Y")),
  `fiscal year` = `Fiscal Year`,
  year = calendar_year,
  month = month_name,
  component = Component,
  `land border region` = `Land Border Region`,
  `area of responsability` = `Area of Responsibility`,
  `area of responsability (abbreviation)` = `AOR (Abbv)`,
  state = State,
  demographic = Demographic,
  nationality = Citizenship,
  iso3c = isoccode(nationality, src = "cbp", origin = "english"),
  `title of authority` = `Title of Authority`,
  `encounter type` = `Encounter Type`,
  `encounter count` = `Encounter Count`,
  source = "U.S. Customs and border protection"
)


# check if past data was update -------------------------------------------

ids_list <- online_df |> 
  column_to_rownames(var = 'id') |> 
  anti_join(datanew) |> 
  rownames_to_column(var = "rowname") |> 
  pull(rowname)

walk(ids_list, possibly(function(x) deleteRecord(formId = activityinfo_form, recordId = x), otherwise = "Error"))


# check for duplicates ----------------------------------------------------

datanew <- datanew |> 
  anti_join(online_df) |> 
  distinct()



# send data to activityinfo -----------------------------------------------

if (nrow(datanew) > 0) importRecords(formId = activityinfo_form, datanew, stageDirect = TRUE)

