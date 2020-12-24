## WorkTimeAnalysis_PlotData.R

library(tidyverse)

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

## load output from WorkTimeAnalysis_CollectData.R
all_weeks_19 <- read_csv("WorkTimeAnalysis_Hours_2019.csv", col_types = "ccccT")
all_weeks_20 <- read_csv("WorkTimeAnalysis_Hours_2020.csv", col_types = "ccccT")

# combine into single data frame
all_weeks <- 
  dplyr::bind_rows(all_weeks_19, all_weeks_20) %>% 
  subset(!is.na(Time))

## set factors
all_weeks$Day <- factor(all_weeks$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
all_weeks$Time <- factor(all_weeks$Time, levels = unique(all_weeks$Time))

## daily hours worked plots
daily_hrs <- 
  all_weeks %>% 
  dplyr::group_by(week_start_date, Day, Daytype) %>% 
  dplyr::summarize(hours_worked = sum(Activity != "Not Work")*0.5) %>% 
  dplyr::mutate(hours_worked_cut = cut(hours_worked, c(0, 0.25, 2, 6, 9, 24), include.lowest = T,
                                       labels = c("0", "0.5 - 2", "2.5 - 6", "6.5 - 9", "> 9.5")))

hrs_per_workday <- sum(daily_hrs$hours_worked)/sum(daily_hrs$Daytype %in% c("work", "conference"))

ggplot(daily_hrs, aes(x = Day, y = week_start_date, fill = hours_worked)) +
  geom_raster()

ggplot(daily_hrs, aes(x = Day, y = week_start_date, fill = hours_worked_cut)) +
  geom_raster() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_datetime(name = "Week", expand = c(0, 0)) +
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
mean(weekly_hrs$hours_worked)
median(weekly_hrs$hours_worked)

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