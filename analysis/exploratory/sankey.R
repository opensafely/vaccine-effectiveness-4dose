# load libraries
library(tidyverse)

# define functions
roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

# import extracted data
extract <- arrow::read_feather(here::here("output", "exploratory", "extract", "input_exploratory.feather")) %>%
  mutate(across(ends_with("_date"), as.Date))

# extract and process target_disease_data
target_disease_data <- extract %>%
  select(patient_id, starts_with("covid_vax_disease")) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c(NA, "index"),
    names_pattern = "^(.*)_(\\d+)_date",
    names_transform = list(index = as.integer),
    values_to = "date",
    values_drop_na = TRUE
  ) 

# extract and process target_product_data
cat("Derive long target_product_data\n")
target_product_data <- extract %>%
  select(-starts_with("covid_vax_disease"), -age20210701) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c("descr", NA),
    names_pattern = "^(.*)_(\\d+)_date",
    # names_transform = list(index = as.integer),
    values_to = "date",
    values_drop_na = TRUE
  ) 

cat("Derive `index` column for target_product_data\n")
target_product_data <- target_product_data %>%
  group_by(patient_id) %>%
  arrange(date, .by_group = TRUE) %>%
  # use dense rank rather than min_rank because >1 vaccines on the same day is
  # almost certainly an error, so don't want to shift all subsequent indices
  mutate(
    index = dense_rank(date)#,
    # dummy_index = row_number()
    ) %>%
  ungroup() %>%
  distinct()

# any products with zero matches (this could flag a typo in the product name)
vaccine_product_names <- readr::read_delim(
  here::here("lib", "vaccine-product-names"), 
  delim = ";", 
  escape_double = FALSE,
  trim_ws = TRUE
  ) %>%
  select(name, product_name)

cat("Product names with zero matches in data:\n")
target_product_data %>%
  group_by(descr) %>%
  count() %>%
  ungroup() %>%
  full_join(
    vaccine_product_names, by = c("descr" = "name")
  ) %>%
  filter(is.na(n)) %>%
  select(-n) %>%
  print(n=Inf)


# how often does the target_disease data not match the target_product data?
check_match <- full_join(
  target_disease_data %>% select(patient_id, date, index),
  target_product_data %>% select(patient_id, date, descr),
  by = c("patient_id", "date")
) %>%
  filter(is.na(descr))

cat("target_product info missing for the following doses identified by target disease:")
cat(" \n")
check_match %>%
  group_by(index) %>%
  count() 

# prepare data for sankey diagram (using target_product for first six doses)

# define some dummy data with sensible vaccine sequences to visualise sankey diagram
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  
  dummy_data <- extract %>%
    distinct(patient_id) %>%
    mutate(
      n_doses = sample(
        x = 1:6, 
        size = n(), 
        replace = TRUE,
        prob = c(0.05, 0.5, 0.2, 0.2, 0.025, 0.025)
        ),
      covid_vax_disease_1_type = sample(
        x = c("pfizer_standard", "az_standard", "moderna_standard"),
        size = n(),
        replace = TRUE
      ),
      covid_vax_disease_2_type = if_else(
        n_doses >= 2,
        covid_vax_disease_1_type, 
        NA_character_
      ),
      covid_vax_disease_3_type = if_else(
        n_doses >= 3,
        sample(
          x = c("pfizer_standard", "moderna_standard"),
          size = n(), 
          replace = TRUE
        ), 
        NA_character_
      )
      
        ) %>%
    select(-n_doses) 
  
  target_product_data <- dummy_data %>%
    pivot_longer(
      cols = -patient_id,
      names_pattern = "covid_vax_disease_(.)_type",
      names_to = "index",
      names_transform = list(index = as.integer),
      values_to = "descr",
      values_drop_na = TRUE
    )
  
}

# count occurrences of all vaccine schedules in the data (up to 6 doses)
schedule_data <- target_product_data %>%
  filter(index <=6) %>%
  distinct(patient_id, descr, index) %>%
  transmute(
    patient_id,
    descr = paste0(descr, "_index_", index),
    value = TRUE
    ) %>%
  pivot_wider(
    names_from = descr,
    values_from = value
  ) %>%
  select(-patient_id) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  mutate(schedule_id = row_number()) 

# A connection data frame is a list of flows with intensity for each flow
links <- schedule_data %>%
  pivot_longer(
    cols = -c(n, schedule_id),
    names_to = "source",
    # names_pattern = "(.*)_index_(.)",
    # names_to = c("source", "index"),
    # names_transform = list(index = as.integer),
    values_drop_na = TRUE
  ) %>%
  mutate(index = as.integer(str_extract(source, "\\d$"))) %>%
  group_by(schedule_id) %>%
  arrange(index, .by_group = TRUE) %>%
  mutate(target = lead(source)) %>%
  ungroup() %>%
  filter(!is.na(target)) %>%
  group_by(source, target) %>%
  summarise(value = roundmid_any(sum(n)), .groups = "keep") %>%
  ungroup() %>%
  as.data.frame()

# save links data for release
write_csv(
  links,
  here::here("output", "exploratory", "links.csv")
)

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("")){
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, 
  # not using real name like in the links dataframe.. 
  # So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  p <- networkD3::sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "IDsource", Target = "IDtarget",
    Value = "value", NodeID = "name",
    sinksRight=FALSE
    )
  p
  
} 
