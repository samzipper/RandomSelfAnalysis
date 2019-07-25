## WorkTimeAnalysis.R
# Tutorial on googlesheets package here: https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/

library(tidyverse)
library(googlesheets)

# authorize account
gs_auth(new_user = T)

# register google sheet
sheet <- gs_title("Work Tracker")

# read google sheet
data <- gs_read(sheet, range = "A3:H38")
daytype <- gs_read(sheet, range = "B2:H2", col_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# transform to long-form and combine
daytype_long <- 
  tibble::tibble(Day = colnames(daytype), 
                 Daytype = c(daytype[1,]))
data_long <- 
  reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")

summary <- 
  dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
  replace_na(list("Activity" = "Not Work"))
