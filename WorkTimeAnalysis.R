## WorkTimeAnalysis.R
# Tutorial on googlesheets package here: https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/

library(tidyverse)
library(googlesheets)

## ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(face="bold", size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

# authorize account
gs_auth(new_user = T)

# register google sheet
sheet <- gs_title("Work Tracker")

# I always keep the first two sheets as the ongoing week and the template, so start at worksheet 3
for (w in 3:sheet$n_ws){
  # figure out when this week starts
  week_start_date <- lubridate::mdy(sheet$ws$ws_title[w])
  
  # read google sheet
  data <- gs_read(sheet, ws = w, range = "A3:H38")
  daytype <- gs_read(sheet, ws = w, range = "B2:H2", col_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # transform to long-form and combine
  daytype_long <- 
    tibble::tibble(Day = colnames(daytype), 
                   Daytype = c(daytype[1,]))
  data_long <- 
    reshape2::melt(data, id = "Time", value.name = "Activity", variable.name = "Day")
  
  week_summary <- 
    dplyr::left_join(data_long, daytype_long, by = "Day") %>% 
    replace_na(list("Activity" = "Not Work")) %>% 
    dplyr::mutate(week_start_date = week_start_date)
  
  if (w == 3){
    all_weeks <- week_summary
  } else {
    all_weeks <-
      dplyr::bind_rows(week_summary, all_weeks)
  }
  
}

## set Day factors
all_weeks$Day <- factor(all_weeks$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## daily hours worked plots
daily_hrs <- 
  all_weeks %>% 
  dplyr::group_by(week_start_date, Day) %>% 
  dplyr::summarize(hours_worked = sum(Activity != "Not Work")*0.5) %>% 
  dplyr::mutate(hours_worked_cut = cut(hours_worked, c(0, 0.25, 2, 6, 9, 24), include.lowest = T,
                                       labels = c("0", "0.5 - 2", "2.5 - 6", "6.5 - 9", "> 9.5")))

ggplot(daily_hrs, aes(x = Day, y = week_start_date, fill = hours_worked)) +
  geom_raster()

ggplot(daily_hrs, aes(x = Day, y = week_start_date, fill = hours_worked_cut)) +
  geom_raster() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_date(name = "Week", expand = c(0, 0)) +
  scale_fill_manual(name = "Daily Hours Worked",
                    values = c("gray65", "#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"))

ggplot(daily_hrs, aes(x = hours_worked, fill = hours_worked_cut)) +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(name = "Daily Hours Worked", expand = c(0, 0)) +
  scale_y_continuous(name = "Number of Days") +
  scale_fill_manual(name = "Daily Hours Worked",
                    values = c("gray65", "#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank())

## weekly hours worked plots
weekly_hrs <- 
  all_weeks %>% 
  dplyr::group_by(week_start_date) %>% 
  dplyr::summarize(hours_worked = sum(Activity != "Not Work")*0.5)
min(weekly_hrs$hours_worked)
max(weekly_hrs$hours_worked)

ggplot(weekly_hrs, aes(x = hours_worked)) +
  geom_histogram(breaks = seq(20, 80, 2.5)) +
  scale_x_continuous(name = "Weekly Hours Worked", expand = c(0, 0)) +
  scale_y_continuous(name = "Number of Weeks")

## weekly hours by activity
weekly_hrs_activity <- 
  all_weeks %>% 
  subset(Activity != "Not Work") %>% 
  dplyr::group_by(week_start_date, Activity) %>% 
  dplyr::summarize(hours_worked = n()*0.5)

ggplot(weekly_hrs_activity, aes(x = week_start_date, y = hours_worked, fill = Activity)) +
  geom_col() +
  geom_hline(yintercept = 40, color = "gray65")

## total hours by activity
activity_hrs <- 
  all_weeks %>% 
  subset(Activity != "Not Work") %>% 
  dplyr::group_by(Activity) %>% 
  dplyr::summarize(hours_worked = n()*0.5)

ggplot(activity_hrs, aes(x = 1, y = hours_worked, fill = Activity)) +
  geom_col() +
  scale_y_continuous(name = "Total Hours Worked", expand = c(0, 0)) +
  scale_x_discrete(name = NULL)

## typical work day
weekday_hours <- 
  all_weeks %>% 
  subset(Daytype = "work") %>% 
  subset(Activity != "Not Work") %>% 
  dplyr::group_by(Time) %>% 
  dplyr::summarize(hours_worked = n()*0.5)

ggplot(weekday_hours, aes(x = 1, y = Time, fill = hours_worked)) +
  geom_raster()

