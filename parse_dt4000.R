library(pdftools)
library(tidyverse)

extract_text <- function(pdf_path) {
  pdf_text <- pdftools::pdf_text(pdf_path)
  text_df <- tibble(text = pdf_text |>
    str_split(pattern = "\n") |> # split by line breaks
    unlist() |>
    str_squish(), # remove multiple blank spaces
    report_path = pdf_path,
    report_id = str_remove_all(pdf_path, "\\.pdf|sample_pdf/")) |>
    # remove empty rows and rows that refer to the unit number
    filter(!text %in% c("", "01", "02", "03", "04"))
  vin_make_year_model <- text_df$text[which(str_detect(text_df$text, "^Vehicle\\sIdentification*")) + 1]
  vehicle_type <- text_df$text[which(str_detect(text_df$text, "^Vehicle\\sType*")) + 1]
  dob_race <- text_df$text[which(str_detect(text_df$text, "^Date\\sof\\sBirth*")) + 1]

  tibble(report_id = str_remove_all(pdf_path, "\\.pdf|sample_pdf/"),
  vin_make_year_model = list(vin_make_year_model),
  vehicle_type = list(vehicle_type),
  dob_race = list(dob_race))

}

extract_fields <- function(text_df) {

  list(vin_make_year_model, vehicle_type, dob_race)
  text_df |>
  rowwise() |>
    mutate(
      vin_make_model = vin_make_year_model,
      vehicle_type = list(extract_fields(paths)[[2]]),
      race = list(extract_fields(paths)[[3]])
    )
}

extract_text(pdfs$paths[[5]])

map_dfr(pdfs$paths[1:5], extract_text)

pdfs <- tibble(paths = list.files("sample_pdf/", full.names = TRUE))

dt_4000_text <- map_dfr(pdfs$paths, extract_text)


extract_fields(dt_4000_text)

x <- pdfs |>
  rowwise() |>
  mutate(
    make_model = list(extract_fields(paths)[[1]]),
    vehicle_type = list(extract_fields(paths)[[2]]),
    race = list(extract_fields(paths)[[3]])
  )

dt_4000_text |>
  unnest_wider(c(vin_make_year_model, vehicle_type, dob_race), names_sep = "_") |>
  mutate(across(
    starts_with("vin_make_year_model"),
    ~ case_when(
      . %in% c(
        "UNK UNKOWN",
        "Color Body Style Bus Use",
        "UNKNOWN UNKNOWN"
      ) ~ "Unknown",
      TRUE ~ .
    )
  )) |>
  separate_wider_delim(cols = vin_make_year_model_1, delim = " ",
                       names = c("vin, make, year, model"),
                       too_many = "merge", too_few = "align_end") |>
  View()
