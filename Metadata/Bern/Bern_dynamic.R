library('tidyverse')
library('readxl')

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

# Set the directory containing the CSV files
folder_path <- '../../data'
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

# Set the directory containing the CSV files
folder_path <- '../../data/Metadata_old'
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

metadata_static <- read_xlsx('../Bern/Meta_Bern.xlsx')
metadata_static <- metadata_static|>
  select(c(Log_NR, STANDORT_NEU, Code_grafana, Latitude, Longitude, Start, End))|>
  rename(Standort = STANDORT_NEU)

metadata <- read_csv('../Bern/Metadata_19-22.csv')
metadata <- metadata|>
  mutate(Code_grafana = NA,
         Start = NA,
         End = NA)|>
  rename(Log_NR = Log_Nr,
         Standort = Name,
         Latitude = NORD_CHTOP,
         Longitude = OST_CHTOPO)|>
  select(c(Log_NR, Standort, Code_grafana, Latitude, Longitude, Start, End))

data <- rbind(metadata_static, metadata)|>
  mutate(Log_NR = as.numeric(Log_NR))|>
  arrange(Log_NR) |>
  group_by(Log_NR)|>
  filter(n() == 1 | !is.na(Code_grafana)) |>
  ungroup()

location_columns <- c("STANDORT_NEU", "Standort", "name", "NAME")
lat_columns <- c("Lat", "Latitude", "NORD_CHTOPO")
lon_columns <- c("Lon", "Longitude", "OST_CHTOPO")

data_combi <- function(data1, data2) {
  # Make sure the Log numbers are numeric
  data1 <- data1 |>
    dplyr::mutate(ID = 1)
  
  data2 <- data2|>
    dplyr::mutate(Log_NR = as.numeric(Log_NR),
                  ID = 2)|>
    select(c(Log_NR, any_of(location_columns), Code_grafana, any_of(lat_columns),
             any_of(lon_columns), Start, End, ID))|>
    rename(Standort = any_of(location_columns),
           Latitude = any_of(lat_columns),
           Longitude = any_of(lon_columns))

  df <- rbind(data1, data2)
  
  df <- df|>
    dplyr::filter(!is.na(Code_grafana)) |>
    arrange(Log_NR)
  
  # semi_join() return all rows from x with a match in y
  rows_to_remove <- df |>
    dplyr::filter(ID == 2) |>
    semi_join(df |> filter(ID == 1),
              by = c("Log_NR", "Code_grafana"))
  
  # anti_join() return all rows from x without a match in y.
  new_filtered <- df |>
    anti_join(rows_to_remove, by = c("Log_NR", "Code_grafana"))
  
  df <- rbind(data1, new_filtered)|>
    arrange(Log_NR)

  return(df)
}

df <- data_combi(data, metadata_gen_2)
df_1 <- data_combi(df, meta_complete)
df_2 <- data_combi(df_1, meta_complete_2)
df_3 <- data_combi(df_2, Logger_neu_grafana|>mutate(End = NA))
df_4 <- data_combi(df_3, metadata_network_2023|>mutate(End = NA))
df_5 <- data_combi(df_4, metadata_network_2024|>mutate(End = NA))
df_6 <- data_combi(df_5, metadata_network_2024_2|>mutate(Start = NA, End = NA))
df_7 <- data_combi(df_6, metadata_network_old)    

metadata_dynamic <- df_7|>
  select(-ID)|>
  filter(nchar(Code_grafana) == 12) |>
  filter(!is.na(Log_NR))|>
  distinct()|>
  arrange(Log_NR)

replace_umlauts <- function(df) {
  df_updated <- df %>%
    mutate_all(~chartr("äöüÄÖÜ", "aouAOU", .))
  return(df_updated)
}

# Applying the function to your data frame
metadata_dynamic <- replace_umlauts(metadata_dynamic)

guardian <- file.exists('../Bern/Bern_dynamic.csv')

# We need a version control system. So if we generate a new file, then we
# append the local timestamp to the file name
if (guardian == FALSE) {
  # Generate a new file with the current system time in the name
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  new_file_name <- paste0('metadata_dynamic_', timestamp, '.csv')
  print(paste('Generating a new file:', new_file_name))
  write_csv2(metadata_dynamic, new_file_name)
} else {
  print('There is already a file called metadata_static.csv')
}  
