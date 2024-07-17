#-------------------------------------------------------------------------------
# Install and load libraries
#-------------------------------------------------------------------------------
load_packages <- function(packages, update = FALSE) {
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(!installed_packages)) {
    install.packages(packages[!installed_packages], repos = "http://cran.us.r-project.org")}

  # Optionally update installed packages
  if (update) {
    update.packages(ask = FALSE, checkBuilt = TRUE) }
  # Load packages without showing messages or warnings
  suppressWarnings(suppressMessages(invisible(lapply(packages, library, character.only = TRUE))))
  print("All packages installed and loaded. You are ready to go!")}

# List of packages to load
packages <- c('readxl', 'tidyverse', 'stringi')
# Call the function to install, optionally update, and load packages
load_packages(packages = packages, update = FALSE)  # Set update = TRUE to update packages

#-------------------------------------------------------------------------------
# Define some variables and vectors
#-------------------------------------------------------------------------------

logger <- c("LOG_NR", "Log_NR", "Log_Nr", "log_nr", "Nummer_2020", "Nummer_2019")
location_columns <- c("STANDORT_NEU", "Standort", "name", "Name", "NAME", "STANDORT")
lat_columns <- c("Lat", "Latitude", "NORD_CHTOPO", "NORD_CH_TOPO", "NORD_CHTOP")
lon_columns <- c("Lon", "Longitude", "OST_CHTOPO", "OST_CH_TOPO", 'OST_CHTOP')

#-------------------------------------------------------------------------------
# Read the official metadata between 2018-2021
#-------------------------------------------------------------------------------

# Read Metadata from 2018-2020 (drop 2018 because it was the first year and many things have changed)
metadata_until_2020 <- read_xlsx('Metadata/Bern/Standorte_2020_def.xlsx')|>
  # drop 2018 because it was the first year and many things have changed
  select(-NUMMER_2018)|>
  # Make sure each location correspond to the same Log_Nr
  filter(Nummer_2019 == Nummer_2020)|>
  # Yes, it does. So we drop 2019
  select(-Nummer_2019)|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  # Check if a colum called Code_grafana exists already. If yes, do nothing else create it and fill it with NA
  mutate(Code_grafana = ifelse(!("Code_grafana" %in% colnames(.data)), NA, Code_grafana))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Start = NA,
         End = NA)|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

# Read Metadata from 2021
metadata_2021 <- read_xlsx('Metadata/Bern/Standorte_2021_Hauptnetz.xlsx')|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  # Check if a colum called Code_grafana exists already. If yes, do nothing else create it and fill it with NA
  mutate(Code_grafana = ifelse(!("Code_grafana" %in% colnames(.data)), NA, Code_grafana))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Start = NA,
         End = NA)|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

#-------------------------------------------------------------------------------
# Read older metadata
#-------------------------------------------------------------------------------

# Read metadata between 2019 and 2022 (here, I am not sure if it complete)
metadata_2019_2022 <- read_csv('data/Metadata_19-22.csv') |>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  # Check if a colum called Code_grafana exists already. If yes, do nothing else create it and fill it with NA
  mutate(Code_grafana = ifelse(!("Code_grafana" %in% colnames(.data)), NA, Code_grafana))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         Start = NA,
         End = NA)|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

# Read metadata for the Bernometer
metadata_Bernometer <- read_csv('data/metadata_gen_2.csv')|>
  select(-STANDORT)|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5))|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

# Not all files has the same delimiter --> use function to dedect the delimiter
determine_delimiter <- function(file) {
  # Read the first line of the file
  first_line <- readLines(file, n = 1)
  # Check if the first line contains ';'
  if (grepl(";", first_line)) {
    return(";")
  } else {
    return(",")
  }
}

# Do it again but use a different folder_path
folder_path <- 'data/Metadata_old/'
# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Loop through each file and read the CSV
for (file in csv_files) {
  # Determine the delimiter
  delimiter <- determine_delimiter(file)
  # Read the CSV file with the appropriate delimiter
  df <- read.csv(file, sep = delimiter)
  # Extract the file name without extension to use as the variable name
  file_name <- tools::file_path_sans_ext(basename(file))
  # Create a variable with the name of the file (without extension) and assign the dataframe to it
  assign(file_name, df)
}

meta_complete <- meta_complete|>
  select(-STANDORT)|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         Start = dmy(Start),
         Start = format(Start, "%m-%d-%Y"),
         End = dmy(End),
         End = format(End, "%m-%d-%Y"))|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

meta_complete_2 <- meta_complete_2|>
  select(-STANDORT)|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         Start = ymd(Start),
         Start = format(Start, "%m-%d-%Y"),
         End = ymd(End),
         End = format(End, "%m-%d-%Y"))|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

metadata_network_2023 <- metadata_network_2023|>
  select(-STANDORT)|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5))|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

metadata_network_2024 <- metadata_network_2024|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         End = NA)|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

metadata_network_2024_2 <- metadata_network_2024_2|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         Start = NA,
         End = NA)|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

metadata_network_old <- metadata_network_old|>
  select(-STANDORT)|>
  # rename some columns
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         Start = dmy(Start),
         Start = format(Start, "%m-%d-%Y"),
         End = dmy(End),
         End = format(End, "%m-%d-%Y"))|>
  # Select columns
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End))

#-------------------------------------------------------------------------------
# Read the newest metadata sheet (here we know it is true)
#-------------------------------------------------------------------------------

metadata_static <- read_xlsx('Metadata/Bern/Meta_Bern.xlsx')|>
  select(-STANDORT)|>
  rename(Log_NR = any_of(logger),
         Latitude = any_of(lat_columns),
         Longitude = any_of(lon_columns),
         Location = any_of(location_columns))|>
  mutate(Log_NR = as.numeric(Log_NR),
         Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         ID = "live")

#-------------------------------------------------------------------------------
# Combine the data sets
#-------------------------------------------------------------------------------

vec <- list(metadata_until_2020|>mutate(ID = 'OLD'), metadata_2021|>mutate(ID = 'OLD'),
            metadata_2019_2022|>mutate(ID = 'OLD'), meta_complete|>mutate(ID = 'OLD'),
            meta_complete_2|>mutate(ID = 'OLD'), metadata_Bernometer|>mutate(ID = 'OLD'),
            metadata_network_2023|>mutate(ID = 'OLD'), metadata_network_2024|>mutate(ID = 'OLD'),
            metadata_network_2024_2|>mutate(ID = 'OLD'), metadata_network_old|>mutate(ID = 'OLD'),
            metadata_static)

metadata_dynamic <- bind_rows(vec)|>
  mutate(Latitude = round(Latitude,5),
         Longitude = round(Longitude,5),
         Start = dmy(Start),
         Start = format(Start, "%m-%d-%Y"),
         End = dmy(End),
         End = format(End, "%m-%d-%Y"))|>
  arrange(Log_NR)|>
  #drop identical rows
  distinct()|>
  filter(!is.na(Code_grafana))|>
  filter(Latitude < 100)|>
  filter(!is.na(Start))|>
  select(c(Log_NR, Location, Latitude, Longitude, Code_grafana, Start, End, ID))

#-------------------------------------------------------------------------------
# substitude ä, ö, ü with a, o, u
#-------------------------------------------------------------------------------

replace_umlauts <- function(df) {
  df_updated <- df |>
    mutate(across(everything(), ~stri_replace_all_fixed(.,
                                                        c("ä", "ö", "ü", "Ä", "Ö", "Ü"),
                                                        c("a", "o", "u", "A", "O", "U"),
                                                        vectorize_all = FALSE)))
  return(df_updated)
}

# Example usage
metadata_dynamic <- replace_umlauts(metadata_dynamic)

#-------------------------------------------------------------------------------
# Write and safe csv file (additionally, there is kind of a version control system)
#-------------------------------------------------------------------------------

guardian <- TRUE

# We need a version control system. So if we generate a new file, then we
# append the local timestamp to the file name
if (guardian == FALSE) {
  # Generate a new file with the current system time in the name
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  new_file_name <- paste0('Metadata/Bern/metadata_dynamic_draft_', timestamp, '.csv')
  print(paste('Generating a new file:', new_file_name))
  write_csv(metadata_dynamic, new_file_name)
} else {
  print('There is already a file called metadata_dynamic_draft.csv')
}

