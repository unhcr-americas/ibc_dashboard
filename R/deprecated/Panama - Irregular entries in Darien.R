library(tidyverse)
library(pdftools)

doc <- pdf_text("https://www.migracion.gob.pa/images/img2023/pdf/IRREGULARES_POR_%20DARI%C3%89N_2023.pdf")

extract_table <- function(doc, page, lines, label) {
  str_split(doc, "\\n")[[page]][lines] |> 
    str_match("([^\\d]+)\\s+(.+)") |> 
    as_tibble(.name_repair = "universal") |> 
    transmute({{label}} := str_trim(...2),
              counts = str_trim(...3)) |> 
    separate_wider_delim(counts, stringr::regex("\\s+"), 
                         names = c("total", as.character(1:12)), 
                         too_few = "align_start") |> 
    select(-total) |> 
    pivot_longer(-1, names_to = "month", values_to = "n") |> 
    mutate(date = lubridate::make_date(2023, month, 1),
           n = parse_number(n)) |> 
    filter(!is.na(n))
}

sex <- extract_table(doc, 1, 28:29, label = "sex")
age <- extract_table(doc, 2, 52:53, label = "age")
coo <- extract_table(doc, 2, 07:42, label = "coo") |> mutate(coo = str_replace(coo, " \\(1\\)", ""))

list(age = age, sex = sex, coo = coo) |> writexl::write_xlsx("darien.xlsx", format_headers = FALSE)
