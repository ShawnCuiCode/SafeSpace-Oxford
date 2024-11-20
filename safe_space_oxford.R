# Load required libraries
library(dplyr)
library(readr)

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

# Load the combined data
combined_data <- read.csv("combined_oxford_crime_data.csv")

# Summarize data
summary(combined_data)

cleaned_data <- combined_data %>% select(-Context, -LSOA.code, -LSOA.name, -Last.outcome.category, -Crime.ID, -Reported.by,-Falls.within)


library(ggplot2)
library(ggmap)

register_google(key = "AIzaSyCN1SlnwrrBwo_dyghA2aaQ7xbrNyNKXaY", write = TRUE)

if (has_google_key()) {
  cat("Google API key registered successfully.")
} else {
  cat("Failed to register Google API key.")
}



