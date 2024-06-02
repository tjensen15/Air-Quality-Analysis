#### Changing TimeStamp and making times of day/hours ####

trail4 <- read.csv('cleaned_4.csv')

#change timestamp to just the time, not date
trail4$Timestamp <- as.POSIXct(trail4$Timestamp, format = "%Y-%m-%dT%H:%M:%S")

# add new column called hour with just the hour from the timestamp
trail4$Hour <- as.numeric(format(trail4$Timestamp, "%H"))

# Create a new variable for time of day categories
trail4$TimeOfDay <- cut(trail4$Hour,
                        breaks = c(0, 6, 12, 18, 24),
                        labels = c("Nighttime", "Morning", "Midday", "Nighttime"),
                        include.lowest = TRUE)
# trail 3
trail3 <- read.csv("lakeshore_trail_3.csv")

trail3$Timestamp <- as.POSIXct(trail3$Timestamp, format = "%Y-%m-%dT%H:%M:%S")

trail3$Hour <- as.numeric(format(trail3$Timestamp, "%H"))

# Create a new variable for time of day categories
trail3$TimeOfDay <- cut(trail3$Hour,
                        breaks = c(0, 6, 12, 18, 24),
                        labels = c("Nighttime", "Morning", "Midday", "Nighttime"),
                        include.lowest = TRUE)

#trail 2
trail2 <- read.csv('cleaned_2.csv')

#change timestamp to just the time, not date
trail2$Timestamp <- as.POSIXct(trail2$Timestamp, format = "%Y-%m-%dT%H:%M:%S")

trail2$Hour <- as.numeric(format(trail2$Timestamp, "%H"))

# Create a new variable for time of day categories
trail2$TimeOfDay <- cut(trail2$Hour,
                        breaks = c(0, 6, 12, 18, 24),
                        labels = c("Nighttime", "Morning", "Midday", "Nighttime"),
                        include.lowest = TRUE)

# trail 1
trail1 <- read.csv('cleaned_1.csv')

#change timestamp to just the time, not date
trail1$Timestamp <- as.POSIXct(trail1$Timestamp, format = "%Y-%m-%dT%H:%M:%S")

#make a new column with just the hour
trail1$Hour <- as.numeric(format(trail1$Timestamp, "%H"))

# Create a new variable for time of day categories
trail1$TimeOfDay <- cut(trail1$Hour,
                        breaks = c(0, 6, 12, 18, 24),
                        labels = c("Nighttime", "Morning", "Midday", "Nighttime"),
                        include.lowest = TRUE)

# Rearrange columns of trail2, trail3, and trail4 to match the order in trail1
trail2 <- trail2[, colnames(trail1)]
# change column name, it was spelled wrong
names(trail3)[names(trail3) == "Humidty"] <- "Humidity"
trail3 <- trail3[, colnames(trail1)]
trail4 <- trail4[, colnames(trail1)]

#combine all of the datasets together
all_trails <- rbind(trail1, trail2, trail3, trail4)


#### Time Plots ####

library(ggplot2)

# plot pm 2.5 levels across time of day (density plot)
ggplot(all_trails, aes(x = PM2.5, fill = TimeOfDay)) +
  geom_density(alpha = 0.7) +
  labs(x = 'PM 2.5 Levels (ug/m3)', y = 'Distribution', fill = 'Time of Day', color = 'Time of Day') +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  ggtitle('Distribution of PM 2.5 Levels by Time of Day') +
  scale_x_continuous(limits = c(0, 18)) +
  scale_y_continuous(limits = c(0,1.5)) +
  theme(axis.text.x = element_text(size = 14),  # adjust x-axis label size
        axis.text.y = element_text(size = 14),  # adjust y-axis label size
        axis.title.x = element_text(size = 16), # adjust x-axis title size
        axis.title.y = element_text(size = 16), # adjust y-axis title size
        plot.title = element_text(size = 18),   # adjust title size
        legend.text = element_text(size = 14),  # adjust legend text size
        legend.title = element_text(size = 16)) # adjust legend title size

#plot pm 2.5 density levels across hour

hourly <- aggregate(PM2.5 ~ Hour, data = all_trails, FUN = mean)

library(RColorBrewer)
ggplot(all_trails, aes(x = PM2.5, fill = as.factor(Hour))) +
  geom_density(alpha = 0.7) +
  labs(x = 'PM 2.5 Levels (ug/m3)', y = 'Distribution', fill = 'Hour (24hrs)') +
  scale_fill_brewer(palette = 'Paired') +
  theme_minimal() +
  ggtitle('Distribution of PM 2.5 Levels by Hour of Day') +
  scale_x_continuous(limits = c(0, 18)) +
  scale_y_continuous(limits = c(0,5))

#### spatial analysis ####

library(leaflet)

map <- leaflet(all_trails) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~ifelse(PM2.5 < 9, "#4682B4", 
                    ifelse(PM2.5 < 35.5, "orange",
                           ifelse(PM2.5 < 55.5, "grey", "grey"))),
    radius = 1,
    popup = with(all_trails, paste("PM2.5:", PM2.5, "<br>",
                                   "Time of day:", TimeOfDay))
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#4682B4", "orange", "grey"),
    labels = c("PM2.5 < 9", "9 <= PM2.5 < 35.5", "35.5 <= PM2.5"),
    title = "PM2.5 Levels",
  )

map

library(htmlwidgets)
saveWidget(map, file = "brochure_map_final.html", selfcontained = TRUE)
