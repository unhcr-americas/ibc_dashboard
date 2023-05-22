library(tidyverse)
library(rvest)
library(activityinfo)

# activityinfo_dataset ----------------------------------------------------
activityinfo_form <- "c4ff2molhzbq41p2"

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
  `title of authority` = `Title of Authority`,
  `encounter type` = `Encounter Type` ,
  `encounter count` = `Encounter Count`,
  source = "U.S. Customs and border protection"
)


  # check for duplicates ----------------------------------------------------

online_df <- getRecords(activityinfo_form) |> 
  select(`fiscal year`, year , month , component, `land border region`, `area of responsability`, `area of responsability (abbreviation)`, state,
         demographic, nationality, `title of authority`, `encounter type`, `encounter count`, source) |> 
  as_tibble() 

datanew <- datanew |> 
  anti_join(online_df) |> 
  distinct()


# send data to activityinfo -----------------------------------------------

if (nrow(datanew) > 0) importRecords(formId = activityinfo_form, datanew, stageDirect = TRUE)

