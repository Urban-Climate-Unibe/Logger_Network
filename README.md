This repo contains access to logger data of the urban climate measurement network of the university of Bern
To download the data use: source("https://raw.github.com/Urban-Climate-Unibe/Logger_Network/main/R/Logger_Data.R") or clone the repo.
The function has 4 Arguments. The first two are the starting and ending date of the data querry as a String, for example (date_start = "2023-05-01",date_end = "2023-09-01").
The third option is wether to interpolate. Use interpolate = 0 for no interpolation. The specified number means timesteps, which are 10 minutes. So interpolate = 3 is up to 30 minutes of missing data are interpolated, default is 0.
Finally use write_csv = T or F to write the .csv File into ./data in your working directory.
Clone this repo and inspect the vignette Step_1_Get_Data_all.rmd for more details.
