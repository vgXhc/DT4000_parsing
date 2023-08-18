library(pdftools)
library(tidyverse)

extract_fields <- function(pdf_path) {
pdf_text <- pdftools::pdf_text(pdf_path)
pdf_text_clean <- tibble(text = pdf_text |>
  str_split(pattern = "\n") |> #split by line breaks
  unlist() |>
  str_squish()) |> #remove multiple blank spaces
  #remove empty rows and rows that refer to the unit number
  filter(!text %in% c("", "01", "02", "03", "04"))

vin_make_year_model <- pdf_text_clean$text[which(str_detect(pdf_text_clean$text, "^Vehicle\\sIdentification*"))+1]
vehicle_type <- pdf_text_clean$text[which(str_detect(pdf_text_clean$text, "^Vehicle\\sType*"))+1]
dob_race <- pdf_text_clean$text[which(str_detect(pdf_text_clean$text, "^Date\\sof\\sBirth*"))+1]

list(vin_make_year_model, vehicle_type, dob_race)
}



pdfs <- tibble(paths = list.files("sample_pdf/",full.names = TRUE))


y <- map(pdfs$paths, extract_fields)
x <- pdfs |>
  rowwise() |>
  mutate(make_model = list(extract_fields(paths)[[1]]),
         vehicle_type = list(extract_fields(paths)[[2]]),
         race = list(extract_fields(paths)[[3]]))

x |>
  unnest_wider(c(make_model, vehicle_type, race), names_sep = "_") |>
  mutate(across(starts_with("make_model"),
                ~case_when(. %in% c("UNK UNKOWN",
                                    "Color Body Style Bus Use",
                                    "UNKNOWN UNKNOWN") ~ "Unknown",
                           TRUE ~ .))) |> View()

