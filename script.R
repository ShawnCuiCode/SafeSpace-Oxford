# Load required libraries
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggmap)
library(viridis)
library(stringr)

# Function to combine monthly CSV files from a folder into a single file, filtered for Oxford-specific data
combine_monthly_data_from_folder <- function(folder_path, output_path) {
  # List all CSV files in the specified folder
  file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(file_paths) == 0) {
    stop("No CSV files found in the specified folder.")
  }

  # Initialize an empty data frame to store combined data
  combined_data <- data.frame()

  for (file_path in file_paths) {
    # Read each file into a data frame
    monthly_data <- read_csv(file_path)

    # Standardize column names (remove spaces, convert to lowercase)
    colnames(monthly_data) <- colnames(monthly_data) %>%
      str_replace_all(" ", "_") %>%
      str_to_lower()

    # Filter rows for Oxford-specific data, excluding surrounding areas
    if ("lsoa_name" %in% colnames(monthly_data)) {
      oxford_data <- monthly_data %>%
        filter(
          !is.na(lsoa_name) &
            grepl("oxford", lsoa_name, ignore.case = TRUE) &
            !grepl("south oxfordshire|west oxfordshire", lsoa_name, ignore.case = TRUE)
        )
      if (nrow(oxford_data) > 0) {
        combined_data <- bind_rows(combined_data, oxford_data)  # Add filtered rows to the combined data
      }
    } else {
      cat("Column 'lsoa_name' not found in file:", file_path, "\n")  # Notify if column is missing
    }
  }

  # Save the combined data as a CSV file
  write_csv(combined_data, output_path)
  cat("Combined data saved to:", output_path, "\n")
}

# Combine data from the specified folder and save the output as "combined_oxford_crime_data.csv"
combine_monthly_data_from_folder("./datasets","combined_oxford_crime_data.csv")

# Load the combined data
combined_data <- read_csv("combined_oxford_crime_data.csv")

# Clean the data by removing unnecessary columns and handling missing values
cleaned_data <- combined_data %>%
  select(-context, -lsoa_code, -lsoa_name, -last_outcome_category, -crime_id, -reported_by, -falls_within) %>%  # Drop irrelevant columns
  mutate(
    location = ifelse(is.na(location), "unknown_location", location),  # Replace missing locations with "unknown_location"
    month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d"),  # Convert "month" to Date format
    longitude = as.numeric(longitude),  # Ensure longitude is numeric
    latitude = as.numeric(latitude)     # Ensure latitude is numeric
  )

# Save the cleaned data to a new CSV file
write_csv(cleaned_data, "data.csv")

# Verify the structure of the cleaned data
colnames(cleaned_data)  # Check column names
summary(cleaned_data)   # Get a summary of the data
head(cleaned_data)      # Preview the first few rows

# Reorder crime types based on total counts for better visualization
crime_type_order <- cleaned_data %>%
  group_by(crime_type) %>%
  summarise(total_count = n(), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  pull(crime_type)

# Apply the reordered crime type levels to the cleaned data
cleaned_data <- cleaned_data %>%
  mutate(crime_type = factor(crime_type, levels = crime_type_order))

# Summarize the monthly trend for each crime type
crime_trend_data <- cleaned_data %>%
  group_by(month, crime_type) %>%
  summarise(Count = n(), .groups = "drop")

# Create a line plot to visualize crime trends over time
crime_trend_plot <- ggplot(crime_trend_data, aes(x = month, y = Count, color = crime_type)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 2) +  # Add points
  scale_color_viridis_d(option = "plasma") +  # Use Plasma color palette for crime types
  labs(
    title = "Crime Trends by Month (Jan-Sep 2024)",  # Chart title
    x = "Month",  # X-axis label
    y = "Count",  # Y-axis label
    color = "Crime Type"  # Legend title
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Format x-axis labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Save the line plot as a PNG file
ggsave("crime_trends_by_month.png", plot = crime_trend_plot, width = 8, height = 6)

# Summarize total cases by crime type
crime_type_summary <- cleaned_data %>%
  group_by(crime_type) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))

# Create a bar chart for crime type distribution
crime_type_bar <- ggplot(crime_type_summary, aes(x = reorder(crime_type, -total), y = total)) +
  geom_bar(stat = "identity", color = "black", fill = "gray") +  # Simple bar chart
  labs(
    title = "Crime Type Distribution (Jan-Sep 2024)",  # Chart title
    x = "Crime Type",  # X-axis label
    y = "Total Cases"  # Y-axis label
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Save the bar chart as a PNG file
ggsave("crime_type_distribution.png", plot = crime_type_bar, width = 10, height = 6)

# Filter data for "Violence and sexual offences" for mapping
filtered_data <- cleaned_data %>%
  filter(crime_type == "Violence and sexual offences")

register_google(key = "AIzaSyCN1SlnwrrBwo_dyghA2aaQ7xbrNyNKXaY")

# Fetch a map centered on the mean coordinates of the filtered data
map <- get_map(
  location = c(
    lon = mean(filtered_data$longitude, na.rm = TRUE),
    lat = mean(filtered_data$latitude, na.rm = TRUE)
  ),
  zoom = 12,
  maptype = "roadmap",
  source = "google"
)

# Plot geographic distribution of "Violence and sexual offences"
geo_plot <- ggmap(map) +
  geom_point(
    data = filtered_data,
    aes(x = longitude, y = latitude, color = crime_type),
    alpha = 0.7,
    size = 3
  ) +
  scale_color_viridis_d(option = "plasma", name = "Crime Type") +
  labs(
    title = "Geographical Distribution of Violence and Sexual Offences (2024)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Save the geographic distribution plot
ggsave("geo_distribution_violence_2024.png", plot = geo_plot, width = 10, height = 8)

# Create an interactive map of crimes
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
  color = ~crime_type,
  colors = viridis::viridis(length(unique(cleaned_data$crime_type)), option = "plasma"),
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

# Save the interactive map as an HTML file
htmlwidgets::saveWidget(interactive_map, "visualisation_2.html", selfcontained = TRUE)