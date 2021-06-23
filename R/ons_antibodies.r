library("readxl")
library("rvest")
library("purrr")
library("tidyr")
library("dplyr")
library("janitor")
library("socialmixr")
library("epimixr")
library("lubridate")
library("ggplot2")
library("readr")

suppressWarnings(dir.create(here::here("data")))
suppressWarnings(dir.create(here::here("output")))

url <- paste0("https://www.ons.gov.uk/peoplepopulationandcommunity/",
              "healthandsocialcare/conditionsanddiseases/datasets/",
              "coronaviruscovid19antibodydatafortheuk")
session <- session(url)

file_url <- session %>%
  html_nodes(xpath = paste0(
               "//a[contains(concat(' ', ",
               "normalize-space(@class),' '),' btn--thick ')]"
             )) %>%
  html_attr("href") %>%
  pluck(1)

file_name <- sub("^.*/([^/]+)$", "\\1", file_url)

data_dir <- here::here("data")

if (!file.exists(file.path(data_dir, file_name))) {
  download.file(paste0("https://www.ons.gov.uk", file_url),
                file.path(data_dir, file_name))
}

antibody_data_raw <- read_excel(file.path(data_dir, file_name),
                                sheet = "1d",
                                skip = 4
                                ) %>%
  clean_names()

ages <- antibody_data_raw %>%
  colnames() %>%
  tibble(age = .) %>%
  mutate(
    age =
      if_else(grepl("^age_", age), as.integer(parse_number(age)),
              NA_integer_)
  ) %>%
  fill(age) %>%
  mutate(age = if_else(is.na(age), "", paste0("_", age))) %>%
  .$age

colnames(antibody_data_raw) <-
  antibody_data_raw %>%
  slice(1) %>%
  unlist() %>%
  if_else(grepl("^Modelled", .), "estimate", .) %>%
  sub("95% (.*) credible interval$", "\\1", .) %>%
  tolower() %>%
  paste0(., ages)

ab <- antibody_data_raw %>%
  clean_names() %>%
  slice(-1) %>%
  mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) %>%
  filter(!is.na(date)) %>%
  pivot_longer(-date) %>%
  separate(name, c("name", "age"), sep = "_") %>%
  mutate(age = as.integer(age),
         value = as.numeric(value) / 100) %>%
  pivot_wider()

saveRDS(ab, here::here("output", "ab.rds"))
