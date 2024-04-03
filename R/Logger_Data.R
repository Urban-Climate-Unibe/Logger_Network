
###date_start = "2023-05-01",date_end = "2023-09-01"###
Logger_data <- function(date_start = "2023-04-01",date_end = as.character(Sys.Date()),write_csv = T,interpolate = 0,type = "temperature"){

#' Download the temperature measurement data from the grafana server and put it into a nice table.
#'
#' @param date_start A character string containing the first day of data to download. Default is yesterday.
#' @param date_end A character string containing the last day. Default is 50 days ago.
#' @param write_csv A Boolean indicating whether the data should be directly saved into a csv. Default is TRUE.
#' @param interpolate A Boolean indicating whether missing data should be interpolated. Default is TRUE.
#' @returns A dataframe containing the downloaded data in tidy format.
#' @examples
#' Logger_data(date_start = "2023-05-01", date_end = "2023-09-01", write_csv = T, interpolate = 0)
  # Time is in utc!

   #setting correct working-directory

  packages <- c("influxdbclient", "dplyr", "lubridate", "ggplot2", "tidyverse", "zoo")#requied packages
  suppressMessages(source("https://raw.github.com/Urban-Climate-Unibe/Logger_Network/main/R/load_packages.R"))#source loading function
  suppressMessages(load_packages(packages)) #load and install if required the packages
  source("https://raw.github.com/Urban-Climate-Unibe/Logger_Network/main/R/interpolate.R")#loading via github for external usage

  meta <- read_csv("https://raw.githubusercontent.com/Urban-Climate-Unibe/Logger_Network/main/data/metadata_gen_2.csv", show_col_types = FALSE)

  token = "tu3zUeCazQobS4TrIIRftQS3Tr4xoZQoZaRf0Ve0iCrU4LZSY1jTS3laCJ_OjwJxWJ6WsKuwXN_tVV10R73hyg==" #token for access of data

  client <- InfluxDBClient$new(url = "https://influx.smcs.abilium.io",
                               token = token,
                               org = "abilium")#Influxdb needs token



  # Create a sequence of dates from start to end in steps of 3 months for stability
  date_seq <- seq.Date(from = as.Date(date_start), to = as.Date(date_end), by = "3 months")

  data_current <- list()
  for (date in as.list(date_seq)) {
    print("Loading date range:")
    print(date)
    if(date+months(3)<as.Date(date_end)){date_end_local <- date+months(3)}else{date_end_local <- as.Date(date_end)}
    print(date_end_local)
    data_current[[length(data_current)+1]] <- client$query(paste0('from(bucket: "smcs") |> range(start: ', as.character(date), ', stop: ', date_end_local, ') |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")'))
  }

  # Get the data from grafana.

  tables <- bind_rows(data_current) |> #binding since better this way, tidy
    mutate(across(starts_with("_"), ~as.POSIXct(., format="%Y-%m-%dT%H:%M:%S%z")))|> #format time
    mutate(Code_grafana = name) #add code grafana for joining


  # meta <- read_csv("../data/meta_complete.csv")|> #reading complete metadata, now by github
  meta <- meta |>
    mutate(End = as.Date(End, format = "%d.%m.%Y"),
           Start = as.Date(Start, format = "%d.%m.%Y"))|>
    mutate(
           End = if_else(is.na(End),Sys.Date(),End))|>#end and quali formatting
    filter(Quali != 0) #removing bad quality data

  result <- inner_join(tables,meta, by = "Code_grafana",relationship = "many-to-many") |> #many to many since several code grafanas per entry sometimes
    filter(date(time) >= Start & date(time) <= End) |>ungroup()|> #now correct ones are assigned by date
    mutate(time = round_date(time, unit = "10 minutes")) |> # round to 10minutes interval
    group_by(time, Log_NR) |> #group now to mean since some may have several
    summarize(temperature = mean(decoded_payload_temperature, na.rm = TRUE),
              humidity = mean(decoded_payload_humidity, na.rm = TRUE), .groups = "drop") |> #now summarize
    ungroup() |>#important for order
    arrange(Log_NR,time) #now can be arranged


if(type == "temperature"){
  result <- result|> select(temperature,time,Log_NR)|>
    pivot_wider(
      names_from = Log_NR,
      values_from = temperature,
      id_cols = time
    )|>#now make correct format in wide
    ungroup()|>
    arrange(time)|>
    rename_at(vars(-1), ~paste0("Log_", .))#rename
}else{
  result <- result|> select(humidity,time,Log_NR)|>
    pivot_wider(
      names_from = Log_NR,
      values_from = humidity,
      id_cols = time
    )|>#now make correct format in wide
    ungroup()|>
    arrange(time)|>
    rename_at(vars(-1), ~paste0("Log_", .))#rename

}





    if(interpolate > 0){
      # source("../R/interpolate.R") #Now loaded via github
      # Apply the function to the temperature column
      result <- result |>
        mutate_all(~ fill_missing_temperatures(.,max_gap = interpolate))
    }


    if (write_csv) {
      # Define the folder path
      folder_path <- "./data/"

      # Check if the folder exists, create it if it doesn't
      if (!file.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE)
      }
      if(type == "temperature"){write_csv(result,paste0("./data/Logger_data_T_",date_start,"_",date_end,".csv"))}else{
        write_csv(result,paste0("./data/Logger_data_H_",date_start,"_",date_end,".csv"))
      }

    }

    return(result)


}
