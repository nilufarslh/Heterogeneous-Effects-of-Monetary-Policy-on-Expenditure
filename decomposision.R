# Load necessary libraries
library(vars)
library(readxl)
library(ggplot2)

# Load the data
#data <- read_excel("Documents/Aca_Year/2024/Uni_2024_Summer/RM/Data/Final_Results/final_data_for_svar.xlsx")

# Define the variables for SVAR
variables <- c('Delta_SP500', 'Delta_FedRate', 'AtkinsonIndex', 'Inflation')

# Prepare the data for SVAR
data_for_svar <- data[variables]
data_for_svar <- na.omit(data_for_svar)  # Remove any rows with missing values

# Create separate datasets based on the "Help" column
data_monetary_policy <- data_for_svar[data$Help == 0, ]
data_information_shock <- data_for_svar[data$Help == 1, ]

# Convert data to time series format
data_ts_monetary_policy <- ts(data_monetary_policy, start = c(1, 1), frequency = 1)
data_ts_information_shock <- ts(data_information_shock, start = c(1, 1), frequency = 1)

# Fit the VAR model for both datasets
var_model_monetary_policy <- VAR(data_ts_monetary_policy, p = 4, type = "const")
var_model_information_shock <- VAR(data_ts_information_shock, p = 4, type = "const")

# Define the A matrix for SVAR with sign restrictions (same for both models)
A_matrix <- diag(4)
A_matrix[2, 1] <- NA  # Delta_FedRate to Delta_SP500 should be estimated
A_matrix[1, 2] <- 1   # Delta_SP500 to Delta_FedRate is fixed as positive
A_matrix[3, 3] <- 1   # AtkinsonIndex to itself is fixed as positive
A_matrix[4, 4] <- 1   # Inflation to itself is fixed as positive

# Fit the SVAR model using the 'direct' method for both models
svar_model_monetary_policy <- SVAR(var_model_monetary_policy, estmethod = "direct", Amat = A_matrix)
svar_model_information_shock <- SVAR(var_model_information_shock, estmethod = "direct", Amat = A_matrix)

# Calculate impulse response functions (IRFs) for both models over 10 periods with confidence intervals
irf_monetary_policy <- irf(svar_model_monetary_policy, n.ahead = 30, boot = TRUE, ci = 0.95)
irf_information_shock <- irf(svar_model_information_shock, n.ahead = 30, boot = TRUE, ci = 0.95)

# Determine the actual number of periods from the IRF results
n_periods_monetary <- nrow(irf_monetary_policy$irf$Delta_FedRate)
n_periods_info <- nrow(irf_information_shock$irf$Delta_FedRate)

# Extract IRF data for plotting with confidence intervals
irf_data_monetary_policy <- data.frame(
  Period = rep(1:n_periods_monetary, 4),
  Response = c(irf_monetary_policy$irf$Delta_FedRate[, "Delta_SP500"],
               irf_monetary_policy$irf$Delta_FedRate[, "AtkinsonIndex"],
               irf_monetary_policy$irf$Delta_FedRate[, "Inflation"],
               irf_monetary_policy$irf$Delta_SP500[, "Delta_FedRate"]),
  Lower_CI = c(irf_monetary_policy$Lower$Delta_FedRate[, "Delta_SP500"],
               irf_monetary_policy$Lower$Delta_FedRate[, "AtkinsonIndex"],
               irf_monetary_policy$Lower$Delta_FedRate[, "Inflation"],
               irf_monetary_policy$Lower$Delta_SP500[, "Delta_FedRate"]),
  Upper_CI = c(irf_monetary_policy$Upper$Delta_FedRate[, "Delta_SP500"],
               irf_monetary_policy$Upper$Delta_FedRate[, "AtkinsonIndex"],
               irf_monetary_policy$Upper$Delta_FedRate[, "Inflation"],
               irf_monetary_policy$Upper$Delta_SP500[, "Delta_FedRate"]),
  Variable = rep(c("Surprise in SP500", "Atkinson Index", "Inflation", "Surprise in FedRate"), each = n_periods_monetary),
  Category = "Monetary policy (negative co-movement)"
)

irf_data_information_shock <- data.frame(
  Period = rep(1:n_periods_info, 4),
  Response = c(irf_information_shock$irf$Delta_FedRate[, "Delta_SP500"],
               irf_information_shock$irf$Delta_FedRate[, "AtkinsonIndex"],
               irf_information_shock$irf$Delta_FedRate[, "Inflation"],
               irf_information_shock$irf$Delta_SP500[, "Delta_FedRate"]),
  Lower_CI = c(irf_information_shock$Lower$Delta_FedRate[, "Delta_SP500"],
               irf_information_shock$Lower$Delta_FedRate[, "AtkinsonIndex"],
               irf_information_shock$Lower$Delta_FedRate[, "Inflation"],
               irf_information_shock$Lower$Delta_SP500[, "Delta_FedRate"]),
  Upper_CI = c(irf_information_shock$Upper$Delta_FedRate[, "Delta_SP500"],
               irf_information_shock$Upper$Delta_FedRate[, "AtkinsonIndex"],
               irf_information_shock$Upper$Delta_FedRate[, "Inflation"],
               irf_information_shock$Upper$Delta_SP500[, "Delta_FedRate"]),
  Variable = rep(c("Surprise in SP500", "Atkinson Index", "Inflation", "Surprise in FedRate"), each = n_periods_info),
  Category = "CB information (positive co-movement)"
)

# Combine both datasets for plotting
irf_data_combined <- rbind(irf_data_monetary_policy, irf_data_information_shock)

# Generate the plot with confidence intervals and a zero line
ggplot(irf_data_combined, aes(x = Period, y = Response)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "lightblue", alpha = 0.5) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add zero line
  facet_wrap(~Variable + Category, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Panel A. Sign restrictions", x = "Months", y = "Response") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10),
        legend.position = "none")  # Remove legend as all lines are the same color

# Save the plot
ggsave("IRF_plot_separated_with_CI_and_Zero_Line_10_Periods.png", width = 10, height = 8)
