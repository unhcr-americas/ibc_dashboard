library(rvest)
library(tidyverse)
library(pdftools)
library(hablar)
library(janitor)


panama_stat <- read_html("https://www.migracion.gob.pa/inicio/estadisticas", encoding = "UTF-8")



url_panama_stat <- panama_stat |> 
  html_element("a[href*='DARIÉN']") |> 
  html_attr("href")

pages <- pdf_text(paste0("https://www.migracion.gob.pa/", url_panama_stat))


# url_panama_stat <- panama_stat |> 
#   xml_find_all(".//a[attribute::*[contains(.,'IRREGULARES')] and .//span//span ]") |> 
#   xml_attr("href")

# pages <- pdf_text(paste0("https://www.migracion.gob.pa/", grep('2023\\.', url_panama_stat, value=TRUE)))


cy <- str_match(pages[1], "(\\d{4})")[,2] |> as.numeric()


datasets <-
  tribble(~year, ~dataset,   ~ncol,  ~startpg, ~endpg, ~cuadro, ~position,
          cy, "TRÁNSITO IRREGULAR DE EXTRANJEROS POR LA FRONTERA CON COLOMBIA POR REGIÓN SEGÚN ORDEN DE IMPORTANCIA",  14, 1, 1, 1, 1,
          cy, "TRÁNSITO IRREGULAR DE EXTRANJEROS POR LA FRONTERA CON COLOMBIA SEGÚN CONDICIÓN",                        14, 1, 1, 2, 2,
          cy, "TRÁNSITO IRREGULAR DE EXTRANJEROS POR LA FRONTERA CON COLOMBIA POR PAÍS SEGÚN ORDEN DE IMPORTANCIA:",   14, 2, 2, 3, 1,
          cy, "TRÁNSITO IRREGULAR DE EXTRANJEROS POR LA FRONTERA CON COLOMBIA SEGÚN CONDICIÓN",                        14, 2, 2, 4, 2)



# extract tables from pdf -------------------------------------------------

data_2023 <-
  datasets |>
  mutate(data =
           pmap(list(ncol = ncol, startpg = startpg, endpg = endpg, cuadro = cuadro, position = position),
                function(ncol, startpg, endpg, cuadro, position) {
                  map_dfr(pages[startpg:endpg],
                          function(page) {
                            read_lines(page) |>
                              trimws() |> 
                              str_split("\\s{2,}", simplify = TRUE) |>
                              as_tibble(.name_repair = "universal") |>
                              slice({
                                i1 <- which(str_detect(...1, paste0("Cuadro No. 00", cuadro)))
                                c(i1, tail(i1, 1) + 1)
                              }[position]:{
                                i2 <- which(str_detect(...1, "SENAFRONT-DARIÉN|\\(1\\) Hijos|Cifras|Gráfico"))
                                c(i2, tail(i2, 1) + 1)
                              }[position]) |>
                              filter(!if_any(everything(), ~.==""),
                                     if_any(-1, ~!str_detect(., "-"))
                              ) 
                          })
                  
                }))



# clean data --------------------------------------------------------------

df_region_2023 <- data_2023$data[[1]] |> 
  row_to_names(row_number = 1) |> 
  map_df(str_replace, pattern = ",", replacement = "") |>
  map_df(str_replace, pattern = "-", replacement = "") |> 
  retype() |> 
  arrange(desc(Total)) |> 
  slice(-1) |>
  select(-c(Total)) |> 
  gather(month, value, -c(`Región`)) |>
  mutate(month_eng = case_when(month == "Ene." ~ "January", 
                               month == "Feb." ~ "February",
                               month == "Mar." ~ "March", 
                               month == "Abr." ~ "April", 
                               month == "May." ~ "May", 
                               month == "Jun." ~ "June",
                               month == "Jul." ~ "July",
                               month == "Ago." ~ "August",
                               month == "Sep." ~ "September", 
                               month == "Oct." ~ "October", 
                               month == "Nov." ~ "November",
                               month == "Dic." ~ "December",
                               TRUE ~ NA_character_
  ),
  year = cy) |> 
  mutate(region_eng = case_when(tolower(`Región`) == "américa del sur" ~ "South America",
                                tolower(`Región`) == "antillas" ~ "Antillas",
                                tolower(`Región`) == "áfrica" ~ "Africa",
                                tolower(`Región`) == "asia" ~ "Asia",
                                tolower(`Región`) == "europa" ~ "Europa",
                                tolower(`Región`) == "eurasia" ~ "Eurasia",
                                tolower(`Región`) == "oceanía" ~ "Oceania",
                                tolower(`Región`) == "américa central" ~ "Central America",
                                tolower(`Región`) == "américa del norte" ~ "North America",
                                TRUE ~ NA_character_)
  ) |> 
  select(-c(`Región`, month)) |> 
  filter(!is.na(value)) |> 
  select(year, month_eng, region_eng, value)

write_csv(df_region_2023, 'data-wrangle/df_region_2023.csv')



df_gender_2023 <- data_2023$data[[2]] |> 
  row_to_names(row_number = 1) |> 
  map_df(str_replace, pattern = ",", replacement = "") |> 
  map_df(str_replace, pattern = "-", replacement = "") |> 
  retype() |> 
  arrange(desc(Total)) |> 
  rename(gender = `Condición`) |> 
  select(-c(Total)) |>
  group_by(gender) |> 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |> 
  gather(month, value, -c(gender)) |>
  mutate(month_eng = case_when(month == "Ene." ~ "January", 
                               month == "Feb." ~ "February",
                               month == "Mar." ~ "March", 
                               month == "Abr." ~ "April", 
                               month == "May." ~ "May", 
                               month == "Jun." ~ "June",
                               month == "Jul." ~ "July",
                               month == "Ago." ~ "August",
                               month == "Sep." ~ "September", 
                               month == "Oct." ~ "October", 
                               month == "Nov." ~ "November",
                               month == "Dic." ~ "December",
                               TRUE ~ NA_character_
  ),
  year = cy) |> 
  mutate(gender_eng = case_when(tolower(gender) == "hombres" ~ "men",
                                tolower(gender) == "mujeres" ~ "women",
                                tolower(gender) == "total" ~ "total",
                                TRUE ~ NA_character_)) |> 
  filter(!is.na(value)) |> 
  select(-c(gender, month)) |> 
  pivot_wider(names_from = gender_eng, values_from = value) |> 
  select(year, month_eng, men, women, total)


write_csv(df_gender_2023, 'data-wrangle/df_gender_2023.csv')




df_country_2023 <- data_2023$data[[3]] |> 
  row_to_names(row_number = 1) |> 
  map_df(str_replace, pattern = ",", replacement = "") |>
  map_df(str_replace, pattern = "-", replacement = "") |> 
  retype() |> 
  arrange(desc(Total)) |> 
  slice(-1) |> 
  group_by(`País`) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |> 
  select(-c(Total)) |> 
  gather(month, value, -c(`País`)) |>
  mutate(month_eng = case_when(month == "Ene." ~ "January", 
                               month == "Feb." ~ "February",
                               month == "Mar." ~ "March", 
                               month == "Abr." ~ "April", 
                               month == "May." ~ "May", 
                               month == "Jun." ~ "June",
                               month == "Jul." ~ "July",
                               month == "Ago." ~ "August",
                               month == "Sep." ~ "September", 
                               month == "Oct." ~ "October", 
                               month == "Nov." ~ "November",
                               month == "Dic." ~ "December",
                               TRUE ~ NA_character_
  ),
  year = cy) |> 
  mutate(`País` = trimws(gsub("[[:punct:]]|[[:digit:]]", "", `País`))) |> 
  select(-c(month)) |> 
  select(year, 
         month_eng, 
         country = `País`,
         value)

write_csv(df_country_2023, 'data-wrangle/df_country_2023.csv')



df_age_2023 <- data_2023$data[[4]] |> 
  row_to_names(row_number = 1) |> 
  map_df(str_replace, pattern = ",", replacement = "") |> 
  retype() |> 
  arrange(desc(Total)) |> 
  rename(age = `Condición`) |> 
  select(-c(Total)) |> 
  group_by(age) |> 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |> 
  gather(month, value, -c(age)) |>
  mutate(month_eng = case_when(month == "Ene." ~ "January", 
                               month == "Feb." ~ "February",
                               month == "Mar." ~ "March", 
                               month == "Abr." ~ "April", 
                               month == "May." ~ "May", 
                               month == "Jun." ~ "June",
                               month == "Jul." ~ "July",
                               month == "Ago." ~ "August",
                               month == "Sep." ~ "September", 
                               month == "Oct." ~ "October", 
                               month == "Nov." ~ "November",
                               month == "Dic." ~ "December",
                               TRUE ~ NA_character_
  ),
  year = cy) |> 
  mutate(age_eng = case_when(tolower(age) == "adultos" ~ "adults",
                             tolower(age) == "menores" ~ "minors",
                             tolower(age) == "total" ~ "total",
                             TRUE ~ NA_character_)) |> 
  select(-c(age, month)) |> 
  pivot_wider(names_from = age_eng, values_from = value) |> 
  select(year, month_eng, adults, minors, total)

write_csv(df_age_2023, 'data-wrangle/df_age_2023.csv')



























