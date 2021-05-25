## LOAD PSES 2020 DATA

## This script downloads the six different 2020 Public Service Employee Survey
## data subsets and then combines them into a single dataframe. For convenience,
## we save this dataframe into an RDS file for later retrieval when calling
## this script.

library(tidyverse)
library(janitor)

# Set directory names
data_dir <- file.path(getwd(), "data")
plot_dir <- file.path(getwd(), "plots")
output_dir <- file.path(getwd(), "output")

# Create directories if they don't yet exist
ifelse(!dir.exists(file.path(data_dir)), dir.create(file.path(data_dir)), FALSE)
ifelse(!dir.exists(file.path(plot_dir)), dir.create(file.path(plot_dir)), FALSE)
ifelse(!dir.exists(file.path(output_dir)), dir.create(file.path(output_dir)), FALSE)

# This function checks if a file exist and, if not, downloads it.
check_file <- function(file_path, file_url) {
  if (!file.exists(file_path)) {
    safely(download.file(file_url, file_path))
  }
}

# Build a dataframe with the subset names (ss1, ss2, etc.), URLs, file names and file paths.
pses_2020_files <- 
  tibble(
    name = paste0("ss", seq(1:6), "_2020"),
    url = c(
      "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/83b8d566-8199-40a3-9d87-6b99e9600c5b/download/subset-1-sous-ensemble-1.csv",
      "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/12d848dd-568d-41b6-8a78-09d898541f07/download/subset-2-sous-ensemble-2.csv",
      "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/63f972a8-8bf2-4584-a735-e5d0c07a9eb6/download/subset-3-sous-ensemble-3.csv",
      "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/39f42f93-8ea4-4de2-b4d2-174b6b31e412/download/subset-4-sous-ensemble-4.csv", # This link seems dead
      "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/56d0ebe0-4353-493f-889a-b9ab36259751/download/subset-5-sous-ensemble-5.csv",
      "https://open.canada.ca/data/dataset/4301f4bb-1daa-4b50-afab-d1193b5d2284/resource/b9363aab-8906-45b3-91b5-c91cae5a9327/download/subset-6-sous-ensemble-6.csv"
    ) 
  ) %>% 
  mutate(
    file = basename(url),
    path = file.path(data_dir, file)
  )

# If we don't have an RDS file to read pses_2020 from, let's build it.
if(!file.exists("pses_2020.rds")) {
  
  # Check if each file exists. If not, download it.
  if(!file.exists(pses_2020_files$path[1])) {download.file(pses_2020_files$url[1], pses_2020_files$path[1])}
  if(!file.exists(pses_2020_files$path[2])) {download.file(pses_2020_files$url[2], pses_2020_files$path[2])}
  if(!file.exists(pses_2020_files$path[3])) {download.file(pses_2020_files$url[3], pses_2020_files$path[3])}
  if(!file.exists(pses_2020_files$path[4])) {download.file(pses_2020_files$url[4], pses_2020_files$path[4])} # Not working for the moment
  if(!file.exists(pses_2020_files$path[5])) {download.file(pses_2020_files$url[5], pses_2020_files$path[5])}
  if(!file.exists(pses_2020_files$path[6])) {download.file(pses_2020_files$url[6], pses_2020_files$path[6])}
  
  # Check if the pses_2002 dataframe exists. If not, read each subset and construct it. 
  if (!exists("pses_2020")) {
    
    ss1_2020 <- read.csv(pses_2020_files$path[1], na.strings = "9999", stringsAsFactors = FALSE)
    ss2_2020 <- read.csv(pses_2020_files$path[2], na.strings = "9999", stringsAsFactors = FALSE)
    ss3_2020 <- read.csv(pses_2020_files$path[3], na.strings = "9999", stringsAsFactors = FALSE)
    ss4_2020 <- read.csv(pses_2020_files$path[4], na.strings = "9999", stringsAsFactors = FALSE) # Not working for the moment
    ss5_2020 <- read.csv(pses_2020_files$path[5], na.strings = "9999", stringsAsFactors = FALSE)
    ss6_2020 <- read.csv(pses_2020_files$path[6], na.strings = "9999", stringsAsFactors = FALSE)
    
    pses_2020 <- 
      # Bind all subsets together
      bind_rows(
        ss1_2020,
        ss2_2020,
        ss3_2020,
        ss4_2020, # Not working for the moment
        ss5_2020,
        ss6_2020
      ) %>% 
      # Convert column names to snake_case
      clean_names() %>%
      # Standardize language-specific column names with _e and _f, 
      # as opposed to ENG and FRA
      set_names(~sub("eng$","_e",.x)) %>%
      set_names(~sub("fra$","_f",.x)) %>% 
      # Create a "dem_question" column to group the "bycond" column by 
      # demographic question. Use "none" when NA and use "org" when
      # the "bycond" value refers to an organizational level. 
      # Otherwise, use the string preceding the "=" sign.
      mutate(dem_question = case_when(
        is.na(bycond) ~ "none",
        startsWith(bycond, "LEVEL") ~ "org",
        TRUE ~ word(bycond, 1, sep = " =")
      )) %>% 
      # Rename the very wordy "most... and least..." columns 
      rename(
        positive = most_positive_or_least_negative,
        neutral = neutral_or_middle_category,
        negative = most_negative_or_least_positive
      )
    
  }
  
  # Save as an RDS file (so we don't need to do this again)
  saveRDS(pses_2020, file = "pses_2020.rds")
  
} else {
  
  # If we already have and RDS file, use that.
  pses_2020 <- readRDS(file = "pses_2020.rds")
  
}
