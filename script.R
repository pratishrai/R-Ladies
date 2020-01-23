library(plyr)
library(ggplot2)

# 2. Reading both data files:

events_2018 <- readRDS("rladies_events_2018.rds")
events_2019 <- readRDS("rladies_events_2019.rds")

################################################

# 1. Obtaining a count (frequency) of events for each group and visualizing the top 50 groups:

### 2018
groups_18 <- count(events_2018, 'group_name')
groups_order_18 <- order(groups_18$freq, decreasing = T)
top_groups_18 <- groups_18[groups_order_18, ]

### 2019
groups_19 <- count(events_2019, 'group_name')
groups_order_19 <- order(groups_19$freq, decreasing = T)
top_groups_19 <- groups_19[groups_order_19, ]

## Visualizing 2018
top_50_groups_18 <- top_groups_18[1:50, ]

group_viz_18 <- ggplot(top_50_groups_18, aes(reorder(group_name, freq), freq)) + geom_bar(stat = "identity", col = "green", fill = "skyblue") + coord_flip()

## Visualizing 2019
top_50_groups_19 <- top_groups_19[1:50, ]

group_viz_19 <- ggplot(top_50_groups_19, aes(reorder(group_name, freq), freq)) + geom_bar(stat = "identity", col = "blue", fill = "pink") + coord_flip()

################################################

# 2. Obtaining a count (frequency) of events for each city (venue_city) and visualizing the top 50 cities:

### 2018
city_18 <- count(events_2018, "venue_city")
city_order_18 <- order(city_18$freq, decreasing = T)
top_cities_18 <- city_18[city_order_18, ]
top_cities_18 <- na.omit(top_cities_18)

### 2019
city_19 <- count(events_2019, "venue_city")
city_order_19 <- order(city_19$freq, decreasing = T)
top_cities_19 <- city_19[city_order_19, ]
top_cities_19 <- na.omit(top_cities_19)

## Visualizing 2018
top_50_cities_18 <- top_cities_18[1:50, ]

city_viz_18 <- ggplot(top_50_cities_18, aes(reorder(venue_city, freq), freq)) + geom_bar(stat = "identity", col = "orange", fill = "brown") + coord_flip()

## Visualizing 2019
top_50_cities_19 <- top_cities_19[1:50, ]

city_viz_19 <- ggplot(top_50_cities_19, aes(reorder(venue_city, freq), freq)) + geom_bar(stat = "identity", col = "purple", fill = "yellow") + coord_flip()

################################################

# 3. Obtaining a count (frequency) of events per country and visualizing the top 20 countries:

### 2018
country_18 <- count(events_2018, "venue_country")
country_order_18 <- order(country_18$freq, decreasing = T)
top_countries_18 <- country_18[country_order_18, ]
top_countries_18 <- na.omit(top_countries_18)

### 2019
country_19 <- count(events_2019, "venue_country")
country_order_19 <- order(country_19$freq, decreasing = T)
top_countries_19 <- country_19[country_order_19, ]
top_countries_19 <- na.omit(top_countries_19)

## Visualizing 2018
top_50_countries_18 <- top_countries_18[1:20, ]

country_viz_18 <- ggplot(top_50_countries_18, aes (reorder(venue_country, freq), freq)) + geom_bar(stat = "identity", col = "red", fill = "blue") + coord_flip()

## Visualizing 2019
top_50_countries_19 <- top_countries_19[1:20, ]

country_viz_19 <- ggplot(top_50_countries_19, aes (reorder(venue_country, freq), freq)) + geom_bar(stat = "identity", col = "red", fill = "skyblue") + coord_flip()

################################################

######################
library(dplyr)
######################

# 4. Computing (in percentage) the increase of R-Ladies events from 2018 to 2019

total_events_2018 <- count(events_2018, "id")
total_events_2019 <- count(events_2019, "id")

events_increase <- (total_events_2019$n - total_events_2018$n)
events_increase_percent <- ((events_increase/total_events_2018$n) * 100)

################################################

# 5. A month-by-month comparison of R-Ladies events for 2018 and 2019 in a CSV table with visualization (as a two line charts in one plot).

#######################
library(lubridate)
#######################

## monthly events in 2018
monthly_events_2018 <- events_2018 %>% group_by(months=floor_date(events_2018$local_date, "month")) %>%
  summarize(events=n())

## monthly events in 2019
monthly_events_2019 <- events_2019 %>% group_by(months=floor_date(events_2019$local_date, "month")) %>%
  summarize(events=n())

increase <- (monthly_events_2019$events - monthly_events_2018$events)

monthly_events <- cbind(monthly_events_2018, monthly_events_2019, increase)

write.csv(monthly_events,"D:\\R-Ladies\\monthly_events.csv", row.names = TRUE)

## Visualization
year_2018 <- events_2018 %>% group_by(year=floor_date(events_2018$local_date, "year")) %>%   summarize(events=n())

year_2019 <- events_2019 %>% group_by(year=floor_date(events_2019$local_date, "year")) %>%
  summarize(events=n())

events <- rbind(year_2018, year_2019)

events_viz <- ggplot(events, aes(year, events)) + geom_bar(stat = "identity", color = "orange", fill = "green")

# ALL RESULTS
group_viz_18
group_viz_19
city_viz_18
city_viz_19
country_viz_18
country_viz_19
events_increase_percent
events_viz