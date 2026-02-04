#getting average co2 concentration for four pathways from CMIP6

library(ncdf4)
library(tidyverse)
library(lubridate)

# Set path to netCDF files
nc_dir <- "~/Desktop/CMIP_concentration_data"

# Define scenarios and their corresponding files
scenarios <- c("A CO2 ssp126", "A CO2 ssp245", "A CO2 ssp370", "A CO2 ssp585")
filenames <- c(
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-IMAGE-ssp126-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-MESSAGE-GLOBIOM-ssp245-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-AIM-ssp370-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-REMIND-MAGPIE-ssp585-1-2-1_gr1-GMNHSH_201501-250012.nc"
)

# Process all files
co2_data <- map2_dfr(scenarios, filenames, function(scen, file) {
  # Open netCDF
  nc <- nc_open(file.path(nc_dir, file))
  
  # Extract CO2 (mole fraction, likely in ppm)
  co2 <- ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air")
  time <- ncvar_get(nc, "time")
  
  # Get time units to convert to dates
  time_units <- ncatt_get(nc, "time", "units")$value
  
  nc_close(nc)
  
  # Convert time to dates (assumes time is in days since some reference)
  # Extract reference date from units string
  ref_date <- str_extract(time_units, "\\d{4}-\\d{2}-\\d{2}")
  dates <- as.Date(ref_date) + time
  
  # Create dataframe with monthly data
  df <- data.frame(
    date = dates,
    year = year(dates),
    month = month(dates),
    co2_ppm = co2[1,],  # First dimension is global mean (GM)
    scenario = scen
  )
  
  return(df)
})

# Aggregate to annual means
co2_annual <- co2_data %>%
  group_by(scenario, year) %>%
  summarise(co2_ppm = mean(co2_ppm, na.rm = TRUE), .groups = 'drop') %>%
  filter(year >= 2022 & year <= 2100)

# Create wide format matching your temperature data
years <- sort(unique(co2_annual$year))
co2_by_scen <- data.frame(row.names = years)

for(scen in scenarios) {
  scen_data <- co2_annual %>%
    filter(scenario == scen) %>%
    arrange(year)
  
  # Create vector with NAs for all years
  col_values <- rep(NA, length(years))
  
  # Fill in values where data exists
  year_indices <- match(scen_data$year, years)
  col_values[year_indices] <- scen_data$co2_ppm
  
  co2_by_scen[[scen]] <- col_values
}

# Save
write_csv(co2_annual, "cmip6_co2_concentrations.csv")
write_csv(co2_by_scen %>% rownames_to_column("year"), "cmip6_co2_by_scenario.csv")

# View
print(head(co2_by_scen))
print(tail(co2_by_scen))
