# Read the CSV file
data <- read.csv("birds_collision.csv")
str(data)

# Start PDF file
pdf("Normal_plots.pdf")

# Define a function to create normal distribution plots
create_normal_distribution_plot <- function(variable_data, variable_name) {
  hist(variable_data, probability = TRUE, col = "lightblue", 
       main = paste("Normal Distribution of", variable_name), 
       xlab = variable_name, ylab = "Density")

  # Add a blue curve representing the theoretical normal distribution
  curve(dnorm(x, mean = mean(variable_data), sd = sd(variable_data)), 
        col = "blue", lwd = 2, add = TRUE)

  # Add a red density plot
  dens <- density(variable_data)
  lines(dens, col = "red", lwd = 2)
}

# Define a function to create density plots
create_density_plot <- function(variable_data, variable_name) {
  den <- density(variable_data)
  plot(den, frame = FALSE, col = "blue", 
       main = paste("Density plot of", variable_name))
}

# Define a function to create box plots
create_box_plot <- function(variable_data, variable_name) {
  boxplot(variable_data, main = paste("Box Plot of", variable_name), 
          ylab = variable_name)
}

# Define a function to create scatter matrix
create_scatter_matrix <- function(data, vars) {
  pairs(data[, vars], main = "Scatter Matrix")
}

# Define a function to create scatter plots
create_scatter_plot <- function(x_data, y_data, x_name, y_name) {
  plot(x_data, y_data, 
       main = paste("Scatter Plot of", y_name, "vs.", x_name),
       xlab = x_name, ylab = y_name,
       pch = 19, col = "red")
}

# Define a function to create a correlation heatmap
create_correlation_heatmap <- function(data, vars) {
  correlation_matrix <- cor(data[, vars])
  heatmap(correlation_matrix, main = "Correlation Heatmap",
          col = colorRampPalette(c("blue", "white", "red"))(100))
}

# Specify variables for analysis
variables <- c("speed", "height", "ac_mass")

# Generate visualizations
for (variable in variables) {
  create_normal_distribution_plot(data[[variable]], variable)
}

# Density plot
create_density_plot(data$speed, "speed")
create_density_plot(data$height, "height")
create_density_plot(data$ac_mass, "ac_mass")

# Box plots
create_box_plot(data$speed, "speed")
create_box_plot(data$height, "height")
create_box_plot(data$ac_mass, "ac_mass")

# Scatter Matrix
create_scatter_matrix(data, c("speed", "height", "ac_mass"))

# Scatter Plot
create_scatter_plot(data$speed, data$height, "speed", "height")

# Correlation Heatmap
create_correlation_heatmap(data, c("speed", "height", "ac_mass"))

# End PDF file
dev.off()
