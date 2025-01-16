# Load necessary libraries
library(readxl)
library(dplyr)

# Load true parents data
true_parents <- read_excel("/Users/madhuparnachatterjee/Desktop/Molecular ecology/True_parents.xlsx")

# Load true offspring data
true_offsprings <- read_excel("/Users/madhuparnachatterjee/Desktop/Molecular ecology/Offsprings.xlsx")

# Adding the true parent pairs
true_parent_ids <- data.frame(
  Parent1 = c("N0942", "N0986", "N0869", "N0118", "N0528", "N0329", "N0314", "N0362", "N0362", "N0290", 
              "N0704", "N0827", "N0362", "N1053", "N0785", "N0214", "N0212", "N0709", "N0329", "N0904", 
              "N0877", "N0118", "N0182", "N0213", "N0118", "N0214", "N0118", "N0704", "N0712", "N0942", 
              "N0782", "N0698", "N0510", "N0600", "N0118", "N0823", "N0794", "N0823", "N0212", "N0212", 
              "N0528", "N0712", "N0528", "N0709", "N0511", "N0901", "N0874", "N0329", "N0923", "N0877", 
              "N0600", "N0528", "N0212", "N1040", "N0712", "N0199", "N0772", "N0600", "N0942"),
  Parent2 = c("N0986", "N1030", "N1005", "N0986", "N0927", "N0511", "N1030", "N1051", "N1005", "N1057", 
              "N0709", "N1052", "N0785", "N1095", "N0912", "N0876", "N0698", "N1052", "N0362", "N0907", 
              "N0912", "N0511", "N0510", "N0704", "N1053", "N0872", "N0869", "N0709", "N1053", "N1052", 
              "N0869", "N0794", "N0923", "N0888", "N1053", "N1053", "N1030", "N0923", "N0889", "N1095", 
              "N1053", "N0942", "N0901", "N0888", "N1057", "N0942", "N0511", "N0888", "N1057", "N0942", 
              "N0960", "N0797", "N0357", "N1052", "N1053", "N1052", "N1089", "N1052", "N1005")
)

# Function to get coordinates of parent pairs
get_parent_coords <- function(parent_ids, parent_data) {
  parent_coords <- lapply(parent_ids, function(id) {
    coords <- parent_data %>% filter(Sample == id)
    return(coords)
  })
  return(parent_coords)
}

# Get coordinates for each parent pair
parent1_coords <- get_parent_coords(true_parent_ids$Parent1, true_parents)
parent2_coords <- get_parent_coords(true_parent_ids$Parent2, true_parents)

# Extract offspring coordinates
offspring_coords <- true_offsprings %>% select(X, Y)

# Initialize vector to store distances
distances <- vector()

# Calculate distances between offspring and their parents
for (i in 1:nrow(true_offsprings)) {
  offspring_x <- offspring_coords$X[i]
  offspring_y <- offspring_coords$Y[i]
  
  parent1_x <- parent1_coords[[i]]$x
  parent1_y <- parent1_coords[[i]]$y
  
  parent2_x <- parent2_coords[[i]]$x
  parent2_y <- parent2_coords[[i]]$y
  
  if (length(parent1_x) > 0 & length(parent1_y) > 0) {
    dist1 <- sqrt((offspring_x - parent1_x)^2 + (offspring_y - parent1_y)^2)
    distances <- c(distances, dist1)
  }
  
  if (length(parent2_x) > 0 & length(parent2_y) > 0) {
    dist2 <- sqrt((offspring_x - parent2_x)^2 + (offspring_y - parent2_y)^2)
    distances <- c(distances, dist2)
  }
}

# Calculate mean, minimum, maximum, and range of distances
mean_distance <- mean(distances)
min_distance <- min(distances)
max_distance <- max(distances)
range_distance <- max_distance - min_distance

# Print results
cat("Mean distance:", mean_distance, "\n")
cat("Minimum distance:", min_distance, "\n")
cat("Maximum distance:", max_distance, "\n")
cat("Range of distances:", range_distance, "\n")
