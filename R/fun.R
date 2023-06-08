

# function translate country name to iso3c --------------------------------

custom_dict <- data.frame(spanish = iconv(stringr::str_to_lower(countrycode::codelist$cldr.name.es) , from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                          english = iconv(stringr::str_to_lower(countrycode::codelist$country.name.en) , from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                          iso3c = countrycode::codelist$iso3c,
                          stringsAsFactors = FALSE) |> 
  dplyr::filter(!is.na(iso3c))


isoccode <- function(x, src = "", origin) {
  dict <-
    tibble::tibble(coo = unique(iconv(stringr::str_to_lower(x), from = 'UTF-8', to = 'ASCII//TRANSLIT')),
                   iso3c = countrycode::countrycode(coo, origin = origin, destination = "iso3c", custom_dict = custom_dict)) |>
    # manually fix some entries that were missed by the automatic mapping
    mutate(iso3c = dplyr::case_when(
      src == "cbp" & coo == "REUNIOUN" ~ "REU",
      src == "cbp" & coo == "UZEBEKISTAN" ~ "UZB",
      src == "cbp" & coo == "china, peoples republic of" ~ "CHN",
      src == "cbp" &  stringr::str_detect(coo, "(STATELESS|COUNTRY|NO NATIONALITY)") ~ "STA",
      src == "cbp" &  stringr::str_detect(coo, "CHRISTMAS ISLANDS") ~ "AUL",
      src == "cbp" &  stringr::str_detect(coo, "COCOS ISLAND") ~ "AUL",
      src == "cbp" &  stringr::str_detect(coo, "CZECHOSLOVAKIA") ~ "CZE",
      src == "cbp" &  stringr::str_detect(coo, "HEARD AND MCDONALD ISLANDS") ~ "AUL",
      src == "cbp" &  stringr::str_detect(coo, "KOSOVO") ~ "SRB",
      src == "cbp" &  stringr::str_detect(coo, "NETHERLANDS ANTILLES") ~ "NET",
      src == "cbp" &  stringr::str_detect(coo, "SERBIA MONTENEGRO") ~ "SRB",
      src == "cbp" &  stringr::str_detect(coo, "TAIWAN") ~ "CHI",
      src == "cbp" & coo == "COTE D' IVORE" ~ "ICO",
      src == "cbp" & coo == "STATELESS" ~ "STA",
      src == "cbp" & coo == "Yemen (Sanaa)" ~ "YEM",
      src == "panama" & coo == "bangladesh" ~ "BGD",
      src == "panama" & coo == "guyana francesa" ~ "GUF",
      src == "panama" & coo == "rep dominicana" ~ "DOM",
      is.na(iso3c) ~ "UNK",
      TRUE ~ iso3c))
  
  set_names(dict$iso3c, dict$coo)[iconv(stringr::str_to_lower(x), from = 'UTF-8', to = 'ASCII//TRANSLIT')]
}

# iconv(stringr::str_to_lower("CHINA, PEOPLES REPUBLIC OF"), from = 'UTF-8', to = 'ASCII//TRANSLIT')
