# Murder-rate-simulation
# Install necessary packages if not already installed
install.packages("deSolve")
install.packages("ggplot2")

# Load libraries
library(deSolve)
library(ggplot2)

# Define the model
murder_rate_model <- function(t, state, parameters) 
{
  # Extract state variables and parameters
  M <- state[1]  # Murder rate
  r <- parameters["r"]
  K <- parameters["K"]
  A <- parameters["A"]
  F <- parameters["F"]
  
  # Seasonal fluctuation term (sine function)
  seasonal_effect <- A * sin(2 * pi * t / 12)
  
  # Differential equation for murder rate with seasonal fluctuation and feedback
  dM <- r * M * (1 - M / K) + seasonal_effect - F * M
  
  # Return the rate of change
  list(c(dM))
}
# Initial conditions
state <- c(M = 0.5)  # Initial murder rate (scaled between 0 and 1)

# Parameters
parameters <- c(
  r = 0.05,    # Growth/decay rate of the murder rate
  K = 1,       # Carrying capacity (maximum sustainable murder rate)
  A = 0.2,     # Amplitude of the seasonal fluctuation (seasonal effect)
  F = 0.1      # Feedback factor (how current rates influence future rates)
)

# Time vector (representing each month of the year)
time <- seq(0, 12, by = 1)  # 12 months, monthly time steps
# Solve the differential equations using ode()
output <- ode(y = state, times = time, func = murder_rate_model, parms = parameters)

# Convert output to a data frame for easier plotting
output_df <- as.data.frame(output)

# Print the first few rows of the output to inspect
head(output_df)
# Plot the murder rate over time (months of the year)
ggplot(output_df, aes(x = time, y = M)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Murder Rate Simulation Over Time of Year",
    x = "Month",
    y = "Murder Rate",
    caption = "Seasonal dynamics with feedback influences"
  ) +
  scale_x_continuous(breaks = 0:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "End")) +
  theme_minimal()
