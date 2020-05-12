# Title     : Communication {r}Evolution
# Created by: Zoriana Yuzvin and Viktoria Lavrynenko
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)

csv <- read.csv("./66f923f3-06c6-4f16-b8dd-32c4539ad8cc_Data.csv")
internet_users <- csv %>% filter(!is.na(Percent)) %>% group_by(Time) %>% summarise(avg = mean(Percent))
print(internet_users)

ggplot(internet_users, aes(Time, avg))
