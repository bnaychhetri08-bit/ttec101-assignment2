library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

url <- "https://api.open-meteo.com/v1/forecast?latitude=-33.8688&longitude=151.2093&daily=temperature_2m_max,temperature_2m_min,precipitation_sum&timezone=Australia%2FSydney"

response <- GET(url)

weather_data <- fromJSON(content(response, "text"))

daily <- weather_data$daily

weather_df <- data.frame(
  date = daily$time,
  temp_max = daily$temperature_2m_max,
  temp_min = daily$temperature_2m_min,
  rainfall = daily$precipitation_sum
)

head(weather_df)

weather_df <- weather_df %>%
  mutate(
    avg_temp = (temp_max + temp_min)/2,
    temp_range = temp_max - temp_min
  )

head(weather_df)
summary(weather_df)
weather_df %>%
  summarise(
    Average_Temperature = mean(avg_temp),
    Highest_Temperature = max(temp_max),
    Lowest_Temperature = min(temp_min),
    Total_Rainfall = sum(rainfall),
    Average_Rainfall = mean(rainfall)
  )
ggplot(weather_df,
       aes(x = date,
           y = temp_max,
           group = 1)) +
  geom_line() +
  labs(
    title = "Maximum Temperature Trend in Sydney",
    x = "Date",
    y = "Maximum Temperature (°C)"
  ) +
  theme_minimal()
ggplot(weather_df,
       aes(x = date,
           y = rainfall)) +
  geom_col() +
  labs(
    title = "Daily Rainfall in Sydney",
    x = "Date",
    y = "Rainfall (mm)"
  ) +
  theme_minimal()
ggplot(weather_df,
       aes(x = avg_temp)) +
  geom_histogram(bins = 5) +
  labs(
    title = "Distribution of Average Temperature",
    x = "Average Temperature (°C)",
    y = "Frequency"
  ) +
  theme_minimal()
