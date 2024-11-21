# https://data.police.uk/data/

# Load required libraries
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(ggplot2)

Sys.setlocale("LC_TIME", "C")

# Define a function to process all .csv files in a specified folder for Oxford city data
combine_monthly_data_from_folder <- function(folder_path, output_path) {
  # List all .csv files in the specified folder
  file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  print(file_paths)

  # Initialize an empty data frame to store combined data
  combined_data <- data.frame()

  # Loop through each file path in the list
  for (file_path in file_paths) {
    # Read the data from each .csv file
    monthly_data <- read_csv(file_path)

    # Print column names to check for any inconsistencies
    print(colnames(monthly_data))

    # Standardize column names by trimming any whitespace
    colnames(monthly_data) <- trimws(colnames(monthly_data))

    # Filter for records where the LSOA name column contains 'Oxford'
    # but does not contain 'South Oxfordshire' or 'West Oxfordshire'
    if("LSOA.name" %in% colnames(monthly_data)) {
      oxford_data <- monthly_data %>%
        filter(grepl("Oxford", `LSOA.name`, ignore.case = TRUE) &
                 !grepl("South Oxfordshire|West Oxfordshire", `LSOA.name`, ignore.case = TRUE))

      # Append the filtered data to the combined data frame
      combined_data <- bind_rows(combined_data, oxford_data)
    } else {
      cat("Column 'LSOA.name' not found in file:", file_path, "\n")
    }
  }

  # Write the combined data to a single CSV file
  write_csv(combined_data, output_path, row.names = FALSE)

  # Return a message indicating completion
  cat("Combined data saved to:", output_path, "\n")
}

# Call the function with the specified folder and output paths
combine_monthly_data_from_folder("./datasets", "combined_oxford_crime_data.csv")

# Load combined data
combined_data <- read_csv("combined_oxford_crime_data.csv")

# Remove unnecessary columns
cleaned_data <- combined_data %>%
  select(-Context, -LSOA.code, -LSOA.name, -Last.outcome.category, -Crime.ID, -Reported.by, -Falls.within)

# Handle missing values for Location
cleaned_data <- cleaned_data %>%
  mutate(Location = ifelse(is.na(Location), "Unknown Location", Location))

# Convert Longitude and Latitude to numeric
cleaned_data <- cleaned_data %>%
  mutate(
    Month = as.Date(paste0(Month, "-01"), format = "%Y-%m-%d"),
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )

# Remove duplicate rows
cleaned_data <- cleaned_data %>%
  distinct()

# Save cleaned data
write_csv(cleaned_data, "data.csv")

# Summary of cleaned data
summary(cleaned_data)


# Create a bar plot for crime type distribution
crime_type_bar <- ggplot(cleaned_data, aes(x = Crime.type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Crime Type Distribution",
    x = "Crime Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as a PNG file
ggsave("crime_type_distribution.png", plot = crime_type_bar, width = 8, height = 6)



# Create a line plot for crime trends
crime_trend_plot <- ggplot(crime_trend_data, aes(x = Month, y = Count, color = Crime.type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Crime Trends by Month (Jan-Sep 2024)",  # Updated title for clarity
    x = "Month",
    y = "Count",
    color = "Crime Type"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Show all months on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as a PNG file
ggsave("crime_trends_by_month.png", plot = crime_trend_plot, width = 8, height = 6)


# Filter data for a specific month and crime type (e.g., January 2024, Violence and sexual offences)
filtered_data <- cleaned_data %>%
  filter(Month == as.Date("2024-01-01"), Crime.type == "Violence and sexual offences")

# Create a scatter plot for geographical distribution
geo_plot <- ggplot(filtered_data, aes(x = Longitude, y = Latitude)) +
  geom_point(color = "red", alpha = 0.6, size = 2) +
  labs(
    title = "Geographical Distribution of Violence and Sexual Offences (Jan 2024)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Save the plot as a PNG file
ggsave("geo_distribution_jan2024.png", plot = geo_plot, width = 8, height = 6)


# Final visualisation
interactive_map <- plot_ly(
  data = cleaned_data,
  lat = ~Latitude,
  lon = ~Longitude,
  type = "scattermapbox",
  mode = "markers",
  marker = list(size = 8, opacity = 0.6),
  color = ~Crime.type,
  text = ~paste(
    "Crime Type:", Crime.type, "<br>",
    "Month:", format(Month, "%Y-%m"), "<br>",
    "Location:", Location
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Crime Distribution by Month and Type (Jan-Sep 2024)",  # Updated title
    mapbox = list(
      style = "carto-positron",
      center = list(
        lon = mean(cleaned_data$Longitude, na.rm = TRUE),
        lat = mean(cleaned_data$Latitude, na.rm = TRUE)
      ),
      zoom = 12
    )
  ) %>%
  config(displayModeBar = TRUE)

htmlwidgets::saveWidget(interactive_map, "visualisation_2.html", selfcontained = FALSE)









