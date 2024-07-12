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


metadata_static <- read_xlsx('../../Metadata/Bern/Meta_Bern.xlsx')

metadata <- read_csv('../../Metadata/Bern/Metadata_19-22.csv')
metadata <- metadata|>
  rename(Log_NR = Log_Nr,
         STANDORT_NEU = Name,
         Latitude = NORD_CHTOP,
         Longitude = OST_CHTOPO)|>
  mutate(STANDORT = NA,
         Code_grafana = NA,
         HuM = NA,
         Art = NA,
         ZUSTAENDIG = NA,
         BEFESTIGUNG = NA,
         Doppel_Messnetz_23 = NA,
         Start = NA,
         End = NA,
         Quali = NA,
         New = NA)


data <- rbind(metadata_static, metadata)|>
  mutate(Log_NR = as.numeric(Log_NR))|>
  arrange(Log_NR) |>
  group_by(Log_NR)|>
  filter(n() == 1 | !is.na(Code_grafana)) |>
  ungroup()


compare_data <- function(data1, data2) {
  # Make sure the Log numbers are numeric
  data1 <- data1|>
    dplyr::mutate(Log_NR = as.numeric(Log_NR))

  data2 <- data2|>
    dplyr::mutate(Log_NR = as.numeric(Log_NR))

  # Perform inner join based on Log_NR --> Add suffixes so that they are distinguishable
  merged_data <- inner_join(data1, data2, by = "Log_NR", suffix = c("_old", "_new"))

  # Filter for rows where Code_grafana matches
  matching_codes <- merged_data |>
    filter(Code_grafana_old == Code_grafana_new)

  filter_vec_1 <- matching_codes|>
    select(Log_NR)|>
    pull()

  # Filter for rows where Code_grafana not matches
  no_matching_codes <- merged_data |>
    filter(Code_grafana_old != Code_grafana_new)

  filter_vec_2 <- no_matching_codes|>
    select(Log_NR)|>
    pull()

  difference <- data2|>
    filter(Log_NR != c(filter_vec_1 | filter_vec_2))

  # Join both data sets but use only missing rows
  df <- anti_join(data2, difference, by = "Log_NR")

  lst <- list(matching_codes, no_matching_codes, df)
  return(lst)
}

df_1 <- compare_data(data, Logger_neu_grafana)

df_2 <- compare_data(data, meta_complete)

df_3 <- compare_data(data, meta_complete_2)

df_4 <- compare_data(data, metadata_gen_2)

df_5 <- compare_data(data, metadata_network_2023)

df_6 <- compare_data(data, metadata_network_2024)

df_7 <- compare_data(data, metadata_network_2024_2)

#df_8 <- compare_data(data, metadata_network_old)



replace_umlauts <- function(df) {
  df_updated <- df %>%
    mutate_all(~chartr("äöüÄÖÜ", "aouAOU", .))
  return(df_updated)
}

# Applying the function to your data frame
data_updated <- replace_umlauts(data)

data_updated|>


guardian <- file.exists('../Bern/metadata_static.csv')

# We need a version control system. So if we generate a new file, then we
# append the local timestamp to the file name
if (guardian == FALSE) {
  # Generate a new file with the current system time in the name
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  new_file_name <- paste0('metadata_static_', timestamp, '.csv')
  print(paste('Generating a new file:', new_file_name))
  write_csv2(data_updated, new_file_name)
} else {
  print('There is already a file called metadata_static.csv')
}

