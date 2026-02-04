##January 28 2026
##This is a script for viewing yearly global CMIP6 temperature data

library(tidyverse)
library(ggplot2)

temp <- read_csv('cmip6_ensemble_means_all_scenarios.csv')


scenarios <- unique(temp$scenario) 


ggplot(data=temp, aes(year, ensemble_mean, color = scenario)) + 
  geom_point() + 
  geom_line()


temp <- read_csv('cmip6_ensemble_means_all_scenarios.csv')
scenarios <- unique(temp$scenario)

# Create empty dataframe with years as rownames
all_years <- unique(temp$year)
temp_by_scen <- data.frame(row.names = years)

# Fill with for loop
for(scen in scenarios) {
  # Extract data for this scenario
  scen_data <- temp %>%
    filter(scenario == scen) %>%
    arrange(year)
  
  # Create a vector with NAs for all years
  col_values <- rep(NA, length(all_years))
  
  # Fill in values where data exists
  year_indices <- match(scen_data$year, all_years)
  col_values[year_indices] <- scen_data$ensemble_mean
  
  # Add as column
  temp_by_scen[[scen]] <- col_values
}

# View result
print(head(temp_by_scen))
print(tail(temp_by_scen))

write_csv(temp_by_scen, "temp_by_scenarios.csv")

