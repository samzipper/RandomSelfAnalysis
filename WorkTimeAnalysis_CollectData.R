## WorkTimeAnalysis.R
# Tutorial on googlesheets package here: https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/

library(tidyverse)
library(googlesheets4)

# deauthorize - no need to write or read private sheets
gs4_deauth()

## collect for 2023
sheet_url <- "https://docs.google.com/spreadsheets/d/14ob3Ncsa_6ttzFWDB2FSn-NEEeSbKFZ2E7-P2qkWL48/edit"
sheet_info <- gs4_get(sheet_url)
tabs_all <- 
  sheet_info$sheets %>% 
  subset(name != "template")

for (w in 1:length(tabs_all$name)){
  # figure out when this week starts
  week_start_date <- lubridate::mdy(tabs_all$name[w])
  
  # read google sheet
  data <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "A3:H38", col_types = "cccccccc")
  daytype <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "B2:H2", 
                        col_names = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # transform to long-form and combine
  daytype_long <- 
    tibble::tibble(Day = colnames(daytype), 
                   Daytype = c(as.character(daytype[1,])))
  data_long <- 
    reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")
  
  week_summary <- 
    dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
    replace_na(list("Activity" = "Not Work")) %>% 
    dplyr::mutate(week_start_date = week_start_date)
  
  if (w == 1){
    all_weeks <- week_summary
  } else {
    all_weeks <-
      dplyr::bind_rows(week_summary, all_weeks)
  }
  
  # pause to avoid rate limit
  Sys.sleep(8)
  
}

# save output
write_csv(all_weeks, "WorkTimeAnalysis_Hours_2023.csv")

## collect for 2022
sheet_url <- "https://docs.google.com/spreadsheets/d/1uusWA60VTzQMyd86N3PPVGUw3Iaxk_BeSHOPRUgc6R8/edit?usp=sharing"
sheet_info <- gs4_get(sheet_url)
tabs_all <- 
  sheet_info$sheets %>% 
  subset(name != "template")

for (w in 1:length(tabs_all$name)){
  # figure out when this week starts
  week_start_date <- lubridate::mdy(tabs_all$name[w])
  
  # read google sheet
  data <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "A3:H38", col_types = "cccccccc")
  daytype <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "B2:H2", 
                        col_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # transform to long-form and combine
  daytype_long <- 
    tibble::tibble(Day = colnames(daytype), 
                   Daytype = c(as.character(daytype[1,])))
  data_long <- 
    reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")
  
  week_summary <- 
    dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
    replace_na(list("Activity" = "Not Work")) %>% 
    dplyr::mutate(week_start_date = week_start_date)
  
  if (w == 1){
    all_weeks <- week_summary
  } else {
    all_weeks <-
      dplyr::bind_rows(week_summary, all_weeks)
  }
  
  # pause to avoid rate limit
  Sys.sleep(8)
  
}

# save output
write_csv(all_weeks, "WorkTimeAnalysis_Hours_2022.csv")

## 2021
sheet_url <- "https://docs.google.com/spreadsheets/d/1cE-u8IsB96H01Y-3G3_JeL8AugWl2oJEPOJDEMtraj0/edit#gid=2092721761"
sheet_info <- gs4_get(sheet_url)
tabs_all <- 
  sheet_info$sheets %>% 
  subset(name != "template")

for (w in 1:length(tabs_all$name)){
  # figure out when this week starts
  week_start_date <- lubridate::mdy(tabs_all$name[w])
  
  # read google sheet
  data <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "A3:H40", col_types = "cccccccc")
  daytype <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "B2:H2", 
                        col_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # transform to long-form and combine
  daytype_long <- 
    tibble::tibble(Day = colnames(daytype), 
                   Daytype = c(as.character(daytype[1,])))
  data_long <- 
    reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")
  
  week_summary <- 
    dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
    replace_na(list("Activity" = "Not Work")) %>% 
    dplyr::mutate(week_start_date = week_start_date)
  
  if (w == 1){
    all_weeks <- week_summary
  } else {
    all_weeks <-
      dplyr::bind_rows(week_summary, all_weeks)
  }
  
  # pause to avoid rate limit
  Sys.sleep(8)
  
}

# save output
write_csv(all_weeks, "WorkTimeAnalysis_Hours_2021.csv")

## collect for 2020
sheet_url <- "https://docs.google.com/spreadsheets/d/1I4kUeZEEtup1e7Wd3PSqfBpaHyK6ntcL18Dz2zYSCfs/edit#gid=1890047702"
sheet_info <- gs4_get(sheet_url)
tabs_all <- 
  sheet_info$sheets %>% 
  subset(name != "template")

for (w in 1:length(tabs_all$name)){
  # figure out when this week starts
  week_start_date <- lubridate::mdy(tabs_all$name[w])
  
  # read google sheet
  data <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "A3:H38", col_types = "cccccccc")
  daytype <- read_sheet(sheet_url, sheet = tabs_all$name[w], range = "B2:H2", 
                        col_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # transform to long-form and combine
  daytype_long <- 
    tibble::tibble(Day = colnames(daytype), 
                   Daytype = c(as.character(daytype[1,])))
  data_long <- 
    reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")
  
  week_summary <- 
    dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
    replace_na(list("Activity" = "Not Work")) %>% 
    dplyr::mutate(week_start_date = week_start_date)
  
  if (w == 1){
    all_weeks <- week_summary
  } else {
    all_weeks <-
      dplyr::bind_rows(week_summary, all_weeks)
  }
  
  # pause to avoid rate limit
  Sys.sleep(8)
  
}

# save output
write_csv(all_weeks, "WorkTimeAnalysis_Hours_2020.csv")

## collect for 2019
sheet_url_19 <- "https://docs.google.com/spreadsheets/d/1O4tB-IDLZUqP1I2ECJ9JnBOEDbQEoOwNLbbcBjrV4Tk/edit?usp=sharing"
sheet_info_19 <- gs4_get(sheet_url_19)
tabs_all_19 <- 
  sheet_info_19$sheets %>% 
  subset(name != "template")

for (w in 1:length(tabs_all_19$name)){
  # figure out when this week starts
  week_start_date <- lubridate::mdy(tabs_all_19$name[w])
  
  # read google sheet
  data <- read_sheet(sheet_url_19, sheet = tabs_all_19$name[w], range = "A3:H38", col_types = "cccccccc")
  daytype <- read_sheet(sheet_url_19, sheet = tabs_all_19$name[w], range = "B2:H2", 
                        col_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # transform to long-form and combine
  daytype_long <- 
    tibble::tibble(Day = colnames(daytype), 
                   Daytype = c(as.character(daytype[1,])))
  data_long <- 
    reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")
  
  week_summary <- 
    dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
    replace_na(list("Activity" = "Not Work")) %>% 
    dplyr::mutate(week_start_date = week_start_date)
  
  if (w == 1){
    all_weeks_19 <- week_summary
  } else {
    all_weeks_19 <-
      dplyr::bind_rows(week_summary, all_weeks_19)
  }
  
  Sys.sleep(8)
  
}

# save output
write_csv(all_weeks_19, "WorkTimeAnalysis_Hours_2019.csv")