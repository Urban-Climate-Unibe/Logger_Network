###date_start = "2023-05-01",date_end = "2023-09-01"###
Logger_data <- function(date_start = as.character(Sys.Date()-50),date_end = as.character(Sys.Date()-1),write_csv = T,interpolate = T){
  if(sub(".*/([^/]+)$", "\\1", getwd())!= "vignettes"){setwd("./vignettes")} #setting correct working-directory

  packages <- c("influxdbclient", "dplyr", "lubridate", "ggplot2", "tidyverse", "zoo")#requied packages
  source("../R/load_packages.R")#source loading function
  load_packages(packages) #load and install if required the packages

  token = "tu3zUeCazQobS4TrIIRftQS3Tr4xoZQoZaRf0Ve0iCrU4LZSY1jTS3laCJ_OjwJxWJ6WsKuwXN_tVV10R73hyg==" #token for access of data

  client <- InfluxDBClient$new(url = "https://influx.smcs.abilium.io",
                               token = token,
                               org = "abilium")#Influxdb needs token



  # Get the data from grafana.
  tables <- client$query(paste0('from(bucket: "smcs") |> range(start: ', date_start, ', stop: ', date_end, ') |> filter(fn: (r) => r["_measurement"] == "mqtt_consumer") |> filter(fn: (r) => r["_field"] == "decoded_payload_temperature" or r["_field"] == "decoded_payload_humidity") |> filter(fn: (r) => r["topic"] != "v3/dynamicventilation@ttn/devices/eui-f613c9feff19276a/up") |> filter(fn: (r) => r["topic"] != "helium/eeea9617559b/rx") |> pivot(rowKey: ["_time"], columnKey: ["_field"], valueColumn: "_value")'))
  tables <- bind_rows(tables) |> #binding since better this way, tidy
    mutate(across(starts_with("_"), ~as.POSIXct(., format="%Y-%m-%dT%H:%M:%S%z")))|> #format time
    mutate(Code_grafana = name) #add code grafana for joining


  meta <- read_csv("../data/meta_complete.csv")|> #reading complete metadata
    mutate(Quali = if_else(is.na(Quali),1,Quali),
           End = if_else(is.na(End),Sys.Date(),End))|>#end and quali formatting
    filter(Quali != 0) #removing bad quality data

  result <- inner_join(tables,meta, by = "Code_grafana",relationship = "many-to-many") |> #many to many since several code grafanas per entry sometimes
    filter(date(time) >= Start & date(time) <= End) |>ungroup()|> #now correct ones are assigned by date
    mutate(time = round_date(time, unit = "10 minutes")) |> # round to 10minutes interval
    group_by(time, Log_NR) |> #group now to mean since some may have several
    summarize(temperature = mean(decoded_payload_temperature, na.rm = TRUE)) |> #now summarize
    ungroup() |>#important for order
    arrange(Log_NR,time) #now can be arranged



    result <- result|> select(temperature,time,Log_NR)|>
      pivot_wider(
        names_from = Log_NR,
        values_from = temperature,
        id_cols = time
      )|>#now make correct format in wide
      ungroup()|>
      arrange(time)|>
      rename_at(vars(-1), ~paste0("Log_", .))#rename




    if(interpolate){
      source("../R/interpolate.R") #Check for functionality!
      # Apply the function to the temperature column
      result <- result |>
        mutate_all(~ fill_missing_temperatures(.))
    }

    if (write_csv) {
      write_csv(result,paste0("../data/Logger_data_",date_start,"_",date_end,".csv"))
    }

    return(result)


}
