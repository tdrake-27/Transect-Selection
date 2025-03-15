library(readr)
library(dplyr)

# Load your dataset from a CSV file (adjust the file path)
transect_data_12 <- read_csv("10.01.2024_Transects.csv")


set.seed(376) # For reproducibility

# Assuming 'transect_data' is your dataset of transects and has a column 'Length'
total_transects <- nrow(transect_data_12)
sample_size <- round(0.12 * total_transects)

# Calculate total length of all transects
total_length <- sum(transect_data_12$Length)
required_sample_length <- 0.12 * total_length  # 12% of the total length

# Function to check if back-to-back transects are in the sample
no_consecutive <- function(sample_indices) {
  diff_sample <- diff(sort(sample_indices))
  all(diff_sample > 1)  # This can be changed for more space between transects (i.e. 2+)
}

# Generate a sample avoiding back-to-back transects and meeting length requirement
repeat {
  sample_indices <- sort(sample(1:total_transects, sample_size, replace = FALSE))
  
  # Check for back-to-back transects
  if (no_consecutive(sample_indices)) {
    # Calculate the length of the sampled transects
    sampled_length <- sum(transect_data_12$Length[sample_indices])
    
    # Check if the sampled length meets the required 10% threshold
    if (sampled_length >= required_sample_length) {
      break
    }
  }
}

# Extracting the 10% sample from the dataset
sampled_data <- transect_data_12[sample_indices, ]

# Display the results
print(sampled_data)
print(paste("Total length of sampled transects:", sampled_length))
print(paste("Total transect length:", total_length))
print(paste("Required sample length (12%):", required_sample_length))

# Export the sampled data to a CSV file
write.csv(sampled_data, "12%_sampled_transects.csv", row.names = FALSE) # Change this to whatever pathway and file name you would like depending on the survey.

print("Sampled data has been exported to '12%_sampled_transects.csv'.") # This is just to show that the file above has been written and finalized.

