##January 28 2026
##This is a script for extracting and cleaning CMIP6 temperature data

###Dataset: Monthly information aggregated on IPCC reference regions for CMIP5/6 and CORDEX
#Reference: https://doi.org/10.5194/essd-12-2959-2020
#Project: CMIP6
#Variable_longname: mean near-surface air temperature
#Units: degC

library(tidyverse)
library(lubridate)

# Set data directory
data_dir <- "IPCC-WG1-Atlas-a7acb0e/datasets-aggregated-regionally/data/CMIP6/CMIP6_tas_landsea/"

# Get all files
files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)



# Process all files
all_data <- map_dfr(files, function(f) {
  # Extract model, scenario, realization from filename
  filename <- basename(f)
  parts <- str_split(str_remove(filename, "\\.csv$"), "_")[[1]]
  
  model <- parts[2]
  scenario <- parts[3]
  realization <- parts[4]
  
  # Read file, skip metadata header
  read_csv(f, skip = 15, show_col_types = FALSE) %>%
    mutate(date = ymd(paste0(date, "-01")),
           year = year(date),
           model = model,
           scenario = scenario,
           realization = realization) %>%
    select(year, world, model, scenario, realization)
})


# Calculate yearly means and ensemble statistics
ensemble_data <- all_data %>%
  # First average monthly to yearly within each model-scenario-realization
  group_by(model, scenario, realization, year) %>%
  summarise(temp = mean(world, na.rm = TRUE), .groups = 'drop') %>%
  # Then average across realizations within each model-scenario
  group_by(model, scenario, year) %>%
  summarise(model_temp = mean(temp, na.rm = TRUE), .groups = 'drop') %>%
  # Finally calculate ensemble mean across models
  group_by(scenario, year) %>%
  summarise(ensemble_mean = mean(model_temp, na.rm = TRUE),
            sd = sd(model_temp, na.rm = TRUE),
            n_models = n(),
            .groups = 'drop')

# Create continuous timeseries (historical + future)
output <- ensemble_data %>%
  filter((scenario == "historical" & year <= 2014) | 
           (scenario != "historical" & year >= 2015 & year <= 2100)) %>%
  arrange(scenario, year)

# Save
write_csv(output, "cmip6_ensemble_means_all_scenarios.csv")

# View summary
print(output)
