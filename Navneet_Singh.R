# Loading necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)

# Loading the dataset
FuelData <- read.csv("Fuel_Consumption_Ratings.csv", stringsAsFactors = FALSE)

# Printing the first few rows of the data
head(FuelData)

# Printing the last few rows of the data
tail(FuelData)

# Printing the structure of the data
str(FuelData)

# Checking the null values in the data
sum(is.na(FuelData))

# Removing the null values from the data
FuelData <- na.omit(FuelData)

# Printing the first few rows of the cleaned data
head(FuelData)

# Printing the summary statistics of the data
summary(FuelData)

# Printing the column names of the data
colnames(FuelData)

# Calculating average consumption
AvgFuelConsumption <- FuelData %>%
  group_by(`Fuel.Type`) %>%
  summarise(Average_Consumption = mean(`Fuel.Consumption.Combined..L.100km.`, na.rm = TRUE))

# Printing the summary of the Average fuel consumption
summary(AvgFuelConsumption)

# Descriptive Statistics for the Fuel Data

# Summary statistics for numerical columns
numerical_summary <- FuelData %>% 
  select(CO2.Rating, CO2.Emissions..g.km., Smog.Rating, Fuel.Consumption.Combined..L.100km.) %>%
  summarise_all(list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    var = ~var(., na.rm = TRUE)
  ))

print("Descriptive Statistics for Numerical Variables:")
print(round(numerical_summary, 2))

# Calculate Correlation between CO2 Rating and CO2 Emissions (substitute for Price)
correlation <- cor(FuelData$CO2.Rating, FuelData$CO2.Emissions..g.km., use = "complete.obs")
print(paste("Correlation between CO2 Rating and CO2 Emissions:", round(correlation, 2)))

# Calculate Correlation Matrix for CO2 Rating, CO2 Emissions, Smog Rating, and Combined Fuel Consumption
cor_matrix <- cor(FuelData %>% 
                    select(CO2.Rating, CO2.Emissions..g.km., Smog.Rating, Fuel.Consumption.Combined..L.100km.), use = "complete.obs")
print("Correlation Matrix for CO2 Rating, CO2 Emissions, Smog Rating, and Combined Fuel Consumption:")
print(round(cor_matrix, 2)) # Rounded for clarity


# Selecting numeric columns for correlation
FuelDataNumericalColumns <- FuelData %>% select_if(is.numeric)

# Calculating correlation matrix
FuelDataCorrelationMatrix <- cor(FuelDataNumericalColumns, use = "complete.obs")

# Melting the correlation matrix 
FuelDataMeltedCorrelations <- melt(FuelDataCorrelationMatrix)

# Plotting the correlation matrix
ggplot(FuelDataMeltedCorrelations, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap",
    x = "",
    y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Build Linear Regression Model for CO2 Emissions Prediction
model <- lm(CO2.Emissions..g.km. ~ CO2.Rating + Smog.Rating + Fuel.Consumption.Combined..L.100km., data = FuelData)
print(summary(model)) # Display model summary

# Plot Regression Line for CO2 Rating vs CO2 Emissions
ggplot(FuelData, aes(x = CO2.Rating, y = CO2.Emissions..g.km.)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Regression: CO2 Emissions vs CO2 Rating",
    x = "CO2 Rating",
    y = "CO2 Emissions (g/km)"
  ) +
  theme_minimal()

# Scatter Plot for CO2 Rating vs CO2 Emissions
ggplot(FuelData, aes(x = CO2.Rating, y = CO2.Emissions..g.km.)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(
    title = "Scatter Plot: CO2 Rating vs CO2 Emissions",
    x = "CO2 Rating",
    y = "CO2 Emissions (g/km)"
  ) +
  theme_minimal()

# Scatter Plot for Smog Rating vs CO2 Emissions
ggplot(FuelData, aes(x = Smog.Rating, y = CO2.Emissions..g.km.)) +
  geom_point(color = "green", alpha = 0.7) +
  labs(
    title = "Scatter Plot: Smog Rating vs CO2 Emissions",
    x = "Smog Rating",
    y = "CO2 Emissions (g/km)"
  ) +
  theme_minimal()

# Scatter Plot for Combined Fuel Consumption vs CO2 Emissions
ggplot(FuelData, aes(x = Fuel.Consumption.Combined..L.100km., y = CO2.Emissions..g.km.)) +
  geom_point(color = "purple", alpha = 0.7) +
  labs(
    title = "Scatter Plot: Combined Fuel Consumption vs CO2 Emissions",
    x = "Combined Fuel Consumption (L/100km)",
    y = "CO2 Emissions (g/km)"
  ) +
  theme_minimal()


# Plotting a Boxplot of Fuel Consumption by Vehicle Class
ggplot(FuelData, aes(x = `Vehicle.Class`, y = `Fuel.Consumption.Combined..L.100km.`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Fuel Consumption by Vehicle Class",
    x = "Vehicle Class",
    y = "Fuel Consumption (L/100km)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting a Bar chart for the distribution of Average Fuel Consumption by Fuel Type
ggplot(AvgFuelConsumption, aes(x = `Fuel.Type`, y = Average_Consumption, fill = `Fuel.Type`)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Fuel Consumption by Fuel Type",
    x = "Fuel Type",
    y = "Average Fuel Consumption (L/100km)"
  ) +
  theme_minimal()

# Plotting a histogram for distribution of CO2 Emissions
ggplot(FuelData, aes(x = `CO2.Emissions..g.km.`)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of CO2 Emissions",
    x = "CO2 Emissions (g/km)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plotting a histogram for distribution of Engine Size
ggplot(FuelData, aes(x = `Engine.Size.in.Litres`)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Engine Size",
    x = "Engine Size (Litres)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plotting a histogram for distribution of Transmission Types
ggplot(FuelData, aes(x = `Transmission`, fill = `Transmission`)) +
  geom_bar() +
  labs(
    title = "Distribution of Transmission Types",
    x = "Transmission Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting a histogram for distribution of Fuel Types
ggplot(FuelData, aes(x = `Fuel.Type`, fill = `Fuel.Type`)) +
  geom_bar() +
  labs(
    title = "Distribution of Fuel Types",
    x = "Fuel Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
