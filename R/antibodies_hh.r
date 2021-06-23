library("readxl")
library("janitor")
library("dplyr")
library("tidyr")
library("ggplot2")

file_url <-
  paste0("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
         "birthsdeathsandmarriages/families/adhocs/",
         "13321agedistributionofpeoplelivinginhouseholdsandfamilies",
         "containingchildreninprimarysecondaryeducationandyears12and13",
         "englandoctobertodecember2020/",
         "agedistributionofthoselivingwithschoolagechildren.xlsx")

file_name <- sub("^.*/([^/]+)$", "\\1", file_url)
data_dir <- here::here("data")
file_path <- file.path(data_dir, file_name)

if (!file.exists(file_path)) {
  download.file(file_url, file_path)
}

type <- c(primary = "Table 2a", secondary = "Table 2b")

total <- read_excel(file_path, sheet = "Table 1", skip = 3) %>%
  clean_names() %>%
  mutate_all(as.integer) %>%
  filter(!is.na(age)) %>%
  select(age, total)

pops <- lapply(names(type), function(x) {
  read_excel(file_path, sheet = type[[x]], skip = 3) %>%
    clean_names() %>%
    mutate_all(as.integer) %>%
    filter(!is.na(age))
})
names(pops) <- names(type)
pops <- bind_rows(pops, .id = "type") %>%
  select(type, age, total) %>%
  filter(!is.na(total)) %>%
  filter(age < 5 | age > 16)

ab <- readRDS(here::here("output", "ab.rds")) %>%
  complete(date = unique(date), age = 0:max(age)) %>%
  group_by(date) %>%
  fill(estimate, upper, lower, .direction = "up") %>%
  inner_join(pops, by = "age") %>%
  pivot_longer(c(estimate, upper, lower)) %>%
  group_by(date, type, name) %>%
  summarise(value = sum(value * total) / sum(total), .groups = "drop") %>%
  pivot_wider() %>%
  mutate(data = "Antibodies")

age_vacc <- readRDS(here::here("data", "processed", "age_vaccinations.rds")) %>%
  group_by(age, dose_number, product_display_type) %>%
  mutate(n = cumsum(n)) %>%
  ungroup() %>%
  arrange(product_display_type, dose_number, age, vaccination_date) %>%
  complete(age = unique(age),
           dose_number = unique(dose_number),
           product_display_type = unique(product_display_type),
           vaccination_date = unique(vaccination_date)) %>%
  group_by(age, dose_number, product_display_type) %>%
  fill(n, .direction = "down") %>%
  ungroup() %>%
  replace_na(list(n = 0)) %>%
  filter(vaccination_date >= "2021-03-01") %>%
  group_by(vaccination_date, age, dose_number) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  inner_join(total, by = "age") %>%
  mutate(estimate = n / total) %>%
  select(-n, -total) %>%
  inner_join(pops, by = "age") %>%
  group_by(date = vaccination_date, type, dose_number) %>%
  summarise(estimate = sum(estimate * total) / sum(total), .groups = "drop") %>%
  mutate(data = paste(dose_number, "dose")) %>%
  select(-dose_number)

all <- bind_rows(ab, age_vacc)

p <- ggplot(all, aes(x = date, y = estimate, ymin = lower, ymax = upper,
               colour = type, fill = type)) +
  expand_limits(y = c(0, 1)) +
  theme_minimal() +
  scale_colour_brewer("School", palette = "Set1") +
  scale_fill_brewer("School", palette = "Set1") +
  geom_line() +
  geom_ribbon(alpha = 0.35) +
  xlab("") +
  scale_y_continuous("Modelled proportion with antibodies",
                     labels = scales::percent_format(accuracy = 1L)) +
  facet_grid(data ~ .) +
  theme(legend.position = "top")

ggsave(here::here("figure", "schools_hh_antibodies.pdf"),
       width = 5, height = 6)
