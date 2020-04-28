# Title: Homework 4
library(tidyverse)
library(lubridate)
library(rvest)
library(ggplot2)
#
# install.packages("rvest")
# install.packages("tidyverse")
# install.packages("progress")

air <- read_html("https://www.radarbox.com/statistics/total") %>%
  html_node("table#datatable") %>%
  html_table()

# Трохи приведемо до лау назви колонок, для зручності
colnames(air) <- colnames(air) %>%
  str_replace("  ", "_") %>%
  str_replace(" ", ".") %>%
  str_to_lower()

#1
dates <- as.Date(air$day, format = "%Y-%m-%d")
week_numbers <- dates %>% week

#2
us_non_us_flights <- air %>% select(us_non.us, day)

#3
air_with_dates <- us_non_us_flights %>% transform(date = as.Date(day, format = "%Y-%m-%d"))
sorted_by_dates <- air_with_dates %>% arrange(date)

#4
air_with_dates_parts <- sorted_by_dates %>% transform(year = year(date),  month = month(date), day = day(date))

#5
airs_repeated <- air_with_dates_parts %>% group_by(month, day) %>% filter(n() == 2)

#6
airs_compared_by_years <- air_with_dates_parts %>%
  pivot_wider(id_cols = c("day", "month"),
              names_from = "year",
              values_from = "us_non.us",
              names_prefix = "y")
airs_compared_named <- airs_compared_by_years %>% mutate(comparison = paste(day, ".", month, ".", "2019", " vs ", day, ".", month, ".", "2020", " : ", y2019-y2020, sep = ""))

#7
prepare_covid <- function(url, col_name) {
  d <- read.csv(url) %>%
    rename(region = Province.State, country = Country.Region) %>%
    pivot_longer(cols = -c(region, country, Lat, Long),
                 names_to = "date",
                 names_prefix = "X",
                 values_to = col_name
    ) %>%
    mutate(date = mdy(date)) %>%
    select(-c(Lat, Long)) %>%
    group_by(country, date) %>%
    summarise(col_name = sum(get(col_name)))

  cnames <- colnames(d)
  cnames[cnames == "col_name"] <- col_name
  colnames(d) <- cnames
  return(d)
}

covid19 <- prepare_covid("https://bit.ly/3aLLfKw", "cases") %>%
  full_join(prepare_covid("https://bit.ly/2XdZ6W0", "recovered")) %>%
  full_join(prepare_covid("https://bit.ly/2yEhPQg", "deaths")) %>%
  mutate(active_cases = cases - recovered - deaths,
         date = as.Date(date))

flights_during_pandemy <- covid19 %>% filter(country == "US") %>% left_join(air_with_dates, "date") %>% rename(flights = us_non.us)

#8
flights_during_pandemy %>% write.csv("./Final.csv")

#8 additional
input <- flights_during_pandemy[, c("flights", "date")]
plot(x = input$date,y = input$flights,
     xlab = "Flights",
     ylab = "Date",
     main = "Covid vs Flights")
