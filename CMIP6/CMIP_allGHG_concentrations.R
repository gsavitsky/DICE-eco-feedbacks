# Getting CO2, CH4, and N2O concentrations for four pathways from CMIP6
library(ncdf4)
library(tidyverse)
library(lubridate)

# Set path to netCDF files
nc_dir <- "~/Desktop/CMIP_concentration_data"

# Define scenarios (base names without gas prefix)
scenario_base <- c("ssp126", "ssp245", "ssp370", "ssp585")

# Define all filenames
co2_files <- c(
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-IMAGE-ssp126-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-MESSAGE-GLOBIOM-ssp245-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-AIM-ssp370-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-carbon-dioxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-REMIND-MAGPIE-ssp585-1-2-1_gr1-GMNHSH_201501-250012.nc"
)

ch4_files <- c(
  "mole-fraction-of-methane-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-IMAGE-ssp126-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-methane-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-MESSAGE-GLOBIOM-ssp245-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-methane-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-AIM-ssp370-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-methane-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-REMIND-MAGPIE-ssp585-1-2-1_gr1-GMNHSH_201501-250012.nc"
)

n2o_files <- c(
  "mole-fraction-of-nitrous-oxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-IMAGE-ssp126-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-nitrous-oxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-MESSAGE-GLOBIOM-ssp245-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-nitrous-oxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-AIM-ssp370-1-2-1_gr1-GMNHSH_201501-250012.nc",
  "mole-fraction-of-nitrous-oxide-in-air_input4MIPs_GHGConcentrations_ScenarioMIP_UoM-REMIND-MAGPIE-ssp585-1-2-1_gr1-GMNHSH_201501-250012.nc"
)

# Process CO2 files
co2_data <- map2_dfr(scenario_base, co2_files, function(scen, file) {
  nc <- nc_open(file.path(nc_dir, file))
  co2 <- ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air")
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  ref_date <- str_extract(time_units, "\\d{4}-\\d{2}-\\d{2}")
  dates <- as.Date(ref_date) + time
  
  data.frame(
    date = dates,
    year = year(dates),
    co2_ppm = co2[1,],
    scenario = scen
  )
})

# Process CH4 files
ch4_data <- map2_dfr(scenario_base, ch4_files, function(scen, file) {
  nc <- nc_open(file.path(nc_dir, file))
  ch4 <- ncvar_get(nc, "mole_fraction_of_methane_in_air")
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  ref_date <- str_extract(time_units, "\\d{4}-\\d{2}-\\d{2}")
  dates <- as.Date(ref_date) + time
  
  data.frame(
    date = dates,
    year = year(dates),
    ch4_ppb = ch4[1,],
    scenario = scen
  )
})

# Process N2O files
n2o_data <- map2_dfr(scenario_base, n2o_files, function(scen, file) {
  nc <- nc_open(file.path(nc_dir, file))
  n2o <- ncvar_get(nc, "mole_fraction_of_nitrous_oxide_in_air")
  time <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  ref_date <- str_extract(time_units, "\\d{4}-\\d{2}-\\d{2}")
  dates <- as.Date(ref_date) + time
  
  data.frame(
    date = dates,
    year = year(dates),
    n2o_ppb = n2o[1,],
    scenario = scen
  )
})

# Aggregate to annual means
co2_annual <- co2_data %>%
  group_by(scenario, year) %>%
  summarise(co2_ppm = mean(co2_ppm, na.rm = TRUE), .groups = 'drop') %>%
  filter(year >= 2022 & year <= 2100)

ch4_annual <- ch4_data %>%
  group_by(scenario, year) %>%
  summarise(ch4_ppb = mean(ch4_ppb, na.rm = TRUE), .groups = 'drop') %>%
  filter(year >= 2022 & year <= 2100)

n2o_annual <- n2o_data %>%
  group_by(scenario, year) %>%
  summarise(n2o_ppb = mean(n2o_ppb, na.rm = TRUE), .groups = 'drop') %>%
  filter(year >= 2022 & year <= 2100)

# Combine all gases with CO2-equivalent conversions
# GWP-100 values (AR6): CH4 = 29.8, N2O = 273
ghg_combined <- co2_annual %>%
  left_join(ch4_annual, by = c("scenario", "year")) %>%
  left_join(n2o_annual, by = c("scenario", "year")) %>%
  mutate(
    # Convert to PgC and PgC-equivalent
    co2_PgC = co2_ppm * 2.124,
    ch4_co2e_ppm = (ch4_ppb / 1000) * 29.8,
    n2o_co2e_ppm = (n2o_ppb / 1000) * 273,
    ch4_PgC_equiv = ch4_co2e_ppm * 2.124,
    n2o_PgC_equiv = n2o_co2e_ppm * 2.124,
    total_PgC_equiv = co2_PgC + ch4_PgC_equiv + n2o_PgC_equiv
  )

# Create wide format
years <- sort(unique(ghg_combined$year))
ghg_by_scen <- data.frame(row.names = years)

for(scen in scenario_base) {
  scen_data <- ghg_combined %>%
    filter(scenario == scen) %>%
    arrange(year)
  
  year_indices <- match(scen_data$year, years)
  
  # CO2 in ppm and PgC
  col_co2_ppm <- rep(NA, length(years))
  col_co2_ppm[year_indices] <- scen_data$co2_ppm
  ghg_by_scen[[paste0(scen, "_co2_ppm")]] <- col_co2_ppm
  
  col_co2_pgc <- rep(NA, length(years))
  col_co2_pgc[year_indices] <- scen_data$co2_PgC
  ghg_by_scen[[paste0(scen, "_co2_PgC")]] <- col_co2_pgc
  
  # CH4 in ppb and PgC-equivalent
  col_ch4_ppb <- rep(NA, length(years))
  col_ch4_ppb[year_indices] <- scen_data$ch4_ppb
  ghg_by_scen[[paste0(scen, "_ch4_ppb")]] <- col_ch4_ppb
  
  col_ch4_pgc <- rep(NA, length(years))
  col_ch4_pgc[year_indices] <- scen_data$ch4_PgC_equiv
  ghg_by_scen[[paste0(scen, "_ch4_PgC_equiv")]] <- col_ch4_pgc
  
  # N2O in ppb and PgC-equivalent
  col_n2o_ppb <- rep(NA, length(years))
  col_n2o_ppb[year_indices] <- scen_data$n2o_ppb
  ghg_by_scen[[paste0(scen, "_n2o_ppb")]] <- col_n2o_ppb
  
  col_n2o_pgc <- rep(NA, length(years))
  col_n2o_pgc[year_indices] <- scen_data$n2o_PgC_equiv
  ghg_by_scen[[paste0(scen, "_n2o_PgC_equiv")]] <- col_n2o_pgc
  
  # Total CO2-equivalent in PgC
  col_total <- rep(NA, length(years))
  col_total[year_indices] <- scen_data$total_PgC_equiv
  ghg_by_scen[[paste0(scen, "_total_PgC_equiv")]] <- col_total
}

# Save files
write_csv(ghg_combined, "cmip6_all_ghg_concentrations.csv")
write_csv(ghg_by_scen %>% rownames_to_column("year"), "cmip6_all_ghg_by_scenario.csv")

# View
print(head(ghg_combined))
print(head(ghg_by_scen))
