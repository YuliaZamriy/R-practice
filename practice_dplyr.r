rm(list=ls())
install.packages("nycflights13")
install.packages("tidyverse")
# copy 'Rcpp' into C:\Program Files\R\R-3.4.2\library
install.packages("Rcpp")
library(nycflights13)
library(tidyverse)

flights <- nycflights13::flights
head(flights)

(jan1 <- filter(flights, month == 1, day == 1))
nov_dec <- filter(flights, month %in% c(11,12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
# same as:
filter(flights, arr_delay <= 120, dep_delay <= 120)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
# # A tibble: 1 x 1
filter(df, is.na(x) | x > 1)
# # A tibble: 2 x 1

# Exercises
filter(flights, arr_delay >= 120)
filter(flights, dest %in% c('IAH','HOU'))
filter(flights, carrier %in% c('UA','DL','AA'))
filter(flights, month %in% c(7,8,9))
filter(flights, arr_delay > 120, dep_delay == 0)
filter(flights, arr_delay < (dep_delay - 30), dep_delay >= 60)
filter(flights, dep_time > 0, dep_time <= 600)
filter(flights, between(dep_time, 0, 600))
filter(flights, is.na(dep_time))

NA^0 # 1
NA|TRUE # TRUE
NA&FALSE # FALSE
NA*0 # NA

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))
arrange(df, !is.na(x), x) # missing first

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

select(flights, contains("TIME"))

flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
# keeps only new variables:
transmute(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
# dep_time = 517, where 5 is hours, 17 is minutes
# %/% integer division
# %% remainder
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)

# offsets
(x <- 1:10)
# [1]  1  2  3  4  5  6  7  8  9 10
lag(x)
# [1] NA  1  2  3  4  5  6  7  8  9
lead(x)
# [1]  2  3  4  5  6  7  8  9 10 NA
x - lag(x)
#  [1] NA  1  1  1  1  1  1  1  1  1
x != lag(x)
#  [1]   NA TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# cummulative and rolling aggregates
cumsum(x)
# [1]  1  3  6 10 15 21 28 36 45 55
cummean(x)
# [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5
cummin(x)
# [1] 1 1 1 1 1 1 1 1 1 1
cummax(x)
# [1]  1  2  3  4  5  6  7  8  9 10

# ranking
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
# [1]  1  2  2 NA  4  5
min_rank(desc(y))
# [1]  5  3  3 NA  2  1

# number of minutes since midnight
transmute(flights,
          dep_time,
          sched_dep_time,
          dep_time_min = dep_time %/% 100 * 60 + dep_time %% 100,
          sched_dep_time_min = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100)

transmute(flights,
          origin,
          dest,
          arr_time,
          dep_time,
          air_time,
          arr_time_min_nyctz = dep_time %/% 100 * 60 + dep_time %% 100 + air_time,
          arr_time_nyctz = arr_time_min_nyctz %/% 60 * 100 + arr_time_min_nyctz %% 60,
          time_diff_min = arr_time_min_nyctz - arr_time %/% 100 * 60 - arr_time %% 100) 
          
select(flights,
       dep_time,
       sched_dep_time,
       dep_delay)

filter(select(flights,
       dep_time,
       sched_dep_time,
       dep_delay),
       dep_delay > 100)

select(flights,
       flight,
       carrier,
       dep_time,
       sched_dep_time,
       dep_delay) %>%
    filter(min_rank(desc(dep_delay)) <= 10) %>%
    arrange(desc(dep_delay))

summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
# 1 12.63907

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, 
                    count = n(),
                    dist = mean(distance, na.rm = TRUE), 
                    delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != 'HNL')

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)

delays <- flights %>%
    group_by(dest) %>%
    summarize(count = n(),
              dist = mean(distance, na.rm = TRUE), 
              delay = mean(arr_delay, na.rm = TRUE)) %>%
    filter(count > 20, dest != 'HNL')

not_cancelled <- 
    flights %>% 
    filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarize(mean = mean(dep_delay))

delays <- 
    not_cancelled %>% 
    group_by(tailnum) %>% 
    summarize(delay = mean(arr_delay))

ggplot(data = delays, mapping = aes(x = delay)) +
    geom_freqpoly(binwidth = 10)

delays <-
    not_cancelled %>% 
    group_by(tailnum) %>% 
    summarize(delay = mean(arr_delay, na.rm = TRUE),
              n = n())

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
    geom_point(alpha = 1/10)

delays %>% 
    filter(n > 25) %>% 
    ggplot(mapping = aes(x = n, y = delay)) +
    geom_point(alpha = 1/10)

install.packages("Lahman")
batting <- as_tibble(Lahman::Batting)

batters <-
    batting %>% 
    group_by(playerID) %>% 
    summarize(
        ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
        ab = sum(AB, na.rm = TRUE)
    )
batters %>% 
    filter(ab > 100) %>% 
    ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point() +
    geom_smooth(se = FALSE)

batters %>% 
    arrange(desc(ba))

not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarize(
        avg_delay1 = mean(arr_delay),
        avg_delay2 = mean(arr_delay[arr_delay > 0])
    )

not_cancelled %>% 
    group_by(dest) %>% 
    summarize(
        distance_avg = mean(distance),
        distance_sd = sd(distance)) %>% 
    arrange(desc(distance_sd))

not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarize(
        first_dep = first(dep_time),
        last_dep = last(dep_time)
    )

not_cancelled %>% 
    group_by(year, month, day) %>% 
    mutate(r = min_rank(desc(dep_time))) %>% 
    filter(r %in% range(r))

not_cancelled %>% 
    group_by(dest) %>% 
    summarize(carriers = n_distinct(carrier)) %>% 
    arrange(desc(carriers))

# to count the number of non-missing values:
flights %>% 
    group_by(dest) %>% 
    summarize(n = n(),
              n2 = sum(!is.na(dep_delay))) %>% 
    arrange(desc(n))

not_cancelled %>% 
    count(dest)

not_cancelled %>% 
    count(tailnum)
not_cancelled %>% 
    count(tailnum, wt = distance)

not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarize(n_early = sum(dep_time < 500))

not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarize(hour_perc = mean(arr_delay > 60))

