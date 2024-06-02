install.packages("mice") #install and load mice package for imputation
library(mice)

trail_4 <- read.csv('lakeshore_trail_4.csv')
sum(is.na(trail_4)) #1270

#filling missing values with predictive mean matching method
four_imputed_data <- mice(trail_4, m = 5, maxit = 50, method = "pmm", seed = 123)
four_completed_data <- complete(imputed_data)

#filling missing values with interpolation (based on time)
install.packages('imputeTS')
library(imputeTS)

trail_4 <- read.csv('lakeshore_trail_4.csv')
trail_4$Timestamp <- as.POSIXct(trail_4$Timestamp, format = "%Y-%m-%dT%H:%M:%S")
trail_4_original <- trail_4
numeric_columns <- c('Temperature', 'Humidity', 'PM1', 'PM10', 'PM2.5')
interpolate_with_timestamps <- function(x) {
  missing <- is.na(x)
  interpolated_values <- na_interpolation(x, option = 'linear')
  interpolated_values[missing] <- NA
  return(interpolated_values)
}

for (col in numeric_columns) {
  trail_4[[col]] <- interpolate_with_timestamps(trail_4[[col]])
}
all(trail_4$Timestamp == trail_4_original$Timestamp)

#need to decide which one is better, the one based on interpolation is more accurate
#could do it based on location, but some of our data backtracks to same area and the air quality may be diff at a diff time

trail_3 <- read.csv("lakeshore_trail_3.csv")
sum(is.na(trail_3)) #check if any NA values; 0

trail_2 <- read.csv("lakeshore_trail_2.csv")
sum(is.na(trail_2)) #3155 NA values

#imputation via interpolation
trail_2$Timestamp <- as.POSIXct(trail_2$Timestamp, format = "%Y-%m-%dT%H:%M:%S")
trail_2_original <- trail_2
numeric_columns <- c('Temperature', 'Humidity', 'PM1', 'PM10', 'PM2.5')
for (col in numeric_columns) {
  interpolated_values <- na_interpolation(trail_2[[col]], option = "linear") #impute missing values
  trail_2_original[[col]][is.na(trail_2[[col]])] <- NA #replace missing values with NA in the original data
  trail_2[[col]] <- interpolated_values #merge interpolated values back into original data
}

#take out anything after timestamp 19:13:42 bc that is when i went into damen and it threw off my data
trail_2$Timestamp <- as.POSIXct(trail_2$Timestamp, format = "%Y-%m-%dT%H:%M:%S")
cutoff_timestamp <- as.POSIXct("2024-03-16 19:13:42", format = "%Y-%m-%d %H:%M:%S")
filtered_trail_2 <- subset(trail_2, Timestamp <= cutoff_timestamp)
all(trail_2$Timestamp == trail_2_original$Timestamp)

trail_1 <- read.csv("lakeshore_trail_1.csv")
sum(is.na(trail_1)) #1010

trail_1$Timestamp <- as.POSIXct(trail_1$Timestamp, format = "%Y-%m-%dT%H:%M:%S")
trail_1_original <- trail_1
numeric_columns <- c('Temperature', 'Humidity', 'PM1', 'PM10', 'PM2.5')
for (col in numeric_columns) {
  interpolated_values <- na_interpolation(trail_1[[col]], option = "linear") #impute missing values
  trail_1_original[[col]][is.na(trail_1[[col]])] <- NA #replace missing values with NA in the original data
  trail_1[[col]] <- interpolated_values #merge interpolated values back into original data
}

write.csv(trail_4_data, file = "cleaned_trail_4.csv")
write.csv(trail_3, file = "cleaned_trail_3.csv")
write.csv(trail_2_data, file = "cleaned_trail_2.csv")
write.csv(trail_1_data, file = "cleaned_trail_1.csv")

#might not need to do any of this, the data just came out weird
