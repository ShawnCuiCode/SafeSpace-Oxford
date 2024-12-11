# https://data.police.uk/data/

# Load required libraries
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggmap)
library(viridis)

combine_monthly_data_from_folder <- function(folder_path, output_path) {
  # List all .csv files in the specified folder
  file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(file_paths) == 0) {
    stop("No CSV files found in the specified folder.")
  }
  
  # Initialize an empty data frame to store combined data
  combined_data <- data.frame()
  
  for (file_path in file_paths) {
    # Read the data
    monthly_data <- read_csv(file_path)
    
    # Clean column names
    colnames(monthly_data) <- colnames(monthly_data) %>%
      str_replace_all(" ", "_") %>%
      str_to_lower()
    
    # Filter for Oxford data
    if ("lsoa_name" %in% colnames(monthly_data)) {
      oxford_data <- monthly_data %>%
        filter(
          !is.na(lsoa_name) &
            grepl("oxford", lsoa_name, ignore.case = TRUE) &
            !grepl("south oxfordshire|west oxfordshire", lsoa_name, ignore.case = TRUE)
        )
      if (nrow(oxford_data) > 0) {
        combined_data <- bind_rows(combined_data, oxford_data)
      }
    } else {
      cat("Column 'lsoa_name' not found in file:", file_path, "\n")
    }
  }
  
  # Write combined data
  write_csv(combined_data, output_path)
  cat("Combined data saved to:", output_path, "\n")
}

combine_monthly_data_from_folder("./datasets","combined_oxford_crime_data.csv")

# Load combined data
combined_data <- read_csv("combined_oxford_crime_data.csv")

# Remove unnecessary columns using the cleaned column names
cleaned_data <- combined_data %>%
  select(-context, -lsoa_code, -lsoa_name, -last_outcome_category, -crime_id, -reported_by, -falls_within)

# Handle missing values for location
cleaned_data <- cleaned_data %>%
  mutate(location = ifelse(is.na(location), "unknown_location", location))

# Convert longitude and latitude to numeric, and convert month to Date
cleaned_data <- cleaned_data %>%
  mutate(
    month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d"),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# Save the cleaned data to a CSV file
write_csv(cleaned_data, "data.csv")


# Verify cleaned column names
colnames(cleaned_data)

cleaned_data <- read_csv("data.csv")
# Summary of cleaned data
summary(cleaned_data)

head(cleaned_data)
str(cleaned_data)



# Summarize total counts by crime type to determine the order
crime_type_order <- cleaned_data %>%
  group_by(crime_type) %>%
  summarise(total_count = n(), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(crime_type)  # Extract the ordered crime types

# Reorder crime_type in the cleaned data based on the total count order
cleaned_data <- cleaned_data %>%
  mutate(crime_type = factor(crime_type, levels = crime_type_order))

# Recalculate crime_trend_data with reordered crime types
crime_trend_data <- cleaned_data %>%
  group_by(month, crime_type) %>%
  summarise(Count = n(), .groups = "drop")

# Create a line plot for crime trends
crime_trend_plot <- ggplot(crime_trend_data, aes(x = month, y = Count, color = crime_type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "plasma") +  # Use Plasma palette for crime types
  labs(
    title = "Crime Trends by Month (Jan-Sep 2024)",
    x = "Month",
    y = "Count",
    color = "Crime Type"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Customize x-axis date labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as a PNG file
ggsave("crime_trends_by_month.png", plot = crime_trend_plot, width = 8, height = 6)
# Ensure crime_type_summary is sorted by total cases in descending order
crime_type_summary <- crime_type_summary %>%
  arrange(desc(total))

# Create the crime type distribution bar plot
crime_type_bar <- ggplot(crime_type_summary, aes(x = reorder(crime_type, -total), y = total)) +
  geom_bar(stat = "identity", color = "black", fill = "gray") +  # Single-color fill
  labs(
    title = "Crime Type Distribution (Jan-Sep 2024)",
    x = "Crime Type",
    y = "Total Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

# Save the plot as a PNG file
ggsave("crime_type_distribution.png", plot = crime_type_bar, width = 10, height = 6)



register_google(key = "AIzaSyCN1SlnwrrBwo_dyghA2aaQ7xbrNyNKXaY")

# Filter data for "Violence and sexual offences" over the entire year
filtered_data <- cleaned_data %>%
  filter(crime_type == "Violence and sexual offences")

# Get the background map
map <- get_map(
  location = c(
    lon = mean(filtered_data$longitude, na.rm = TRUE),
    lat = mean(filtered_data$latitude, na.rm = TRUE)
  ),
  zoom = 12,  # Adjust zoom level as needed
  maptype = "roadmap",  # Options: roadmap, satellite, hybrid, terrain
  source = "google"  # Use Google Maps as the source
)

# Create the geographical distribution plot using Plasma color palette
geo_plot <- ggmap(map) +
  geom_point(
    data = filtered_data,
    aes(x = longitude, y = latitude, color = crime_type),  # Use crime_type for color
    alpha = 0.7,
    size = 3
  ) +
  scale_color_viridis_d(option = "plasma", name = "Crime Type") +  # Use discrete Plasma palette for crime_type
  labs(
    title = "Geographical Distribution of Violence and Sexual Offences (2024)",
    x = "Longitude",
    y = "Latitude",
    color = "Crime Type"
  ) +
  theme_minimal()

# Save the plot
ggsave("geo_distribution_violence_2024.png", plot = geo_plot, width = 10, height = 8)


# Summarise crime data by type
crime_type_summary <- cleaned_data %>%
  group_by(crime_type) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))  # Sort by descending order of total counts

# Generate sorted list of crime types with counts
sorted_crime_types <- crime_type_summary$crime_type
sorted_crime_types_with_counts <- paste0(crime_type_summary$crime_type, " (", crime_type_summary$total, ")")

# Reorder 'crime_type' in 'cleaned_data' based on sorted order and update labels
cleaned_data <- cleaned_data %>%
  mutate(crime_type = factor(
    crime_type,
    levels = sorted_crime_types,  # Reorder by descending counts
    labels = sorted_crime_types_with_counts  # Add counts to labels
  ))

# Generate the interactive map
interactive_map <- plot_ly(
  data = cleaned_data,
  lat = ~latitude,
  lon = ~longitude,
  type = "scattermapbox",
  mode = "markers",
  marker = list(
    size = 8,
    opacity = 0.6
  ),
  color = ~crime_type,  # Specify the column to map colors
  colors = viridis::viridis(length(sorted_crime_types), option = "plasma"),  # Apply plasma color palette
  text = ~paste(
    "Crime Type:", crime_type, "<br>",
    "Month:", format(month, "%Y-%m"), "<br>",
    "Location:", location
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "Crime Distribution by Month and Type (Jan-Sep 2024)",
    mapbox = list(
      style = "carto-positron",
      center = list(
        lon = mean(cleaned_data$longitude, na.rm = TRUE),
        lat = mean(cleaned_data$latitude, na.rm = TRUE)
      ),
      zoom = 12
    ),
    legend = list(
      title = list(text = "Crime Type (Descending Order with Counts)"),
      bgcolor = "rgba(255,255,255,0.5)"
    )
  )

# Save the map as an HTML file
htmlwidgets::saveWidget(interactive_map, "visualisation_2.html", selfcontained = TRUE)