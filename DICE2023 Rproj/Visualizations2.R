# Import all cleaned sensitivity analysis data for visualization
# Data output from STELLA and cleaned in F file STELLA output processing.R

library(readr)
library(purrr)
library(dplyr)

# ---- CONFIG ----
vis_dir <- "vis"
fs_dir  <- file.path(vis_dir, "feedback sensitivity")
ws_dir  <- file.path(vis_dir, "welfare sensitivity")

# Read every CSV in a by_variable folder into a named list (name = variable)
read_variable_csvs <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  var_names <- tools::file_path_sans_ext(basename(files))
  set_names(map(files, read_csv, show_col_types = FALSE), var_names)
}

# ---- Feedback sensitivity analysis ----
fs_params <- read_csv(file.path(fs_dir, "run_configurations.csv"), show_col_types = FALSE)
fs_vars   <- read_variable_csvs(file.path(fs_dir, "by_variable"))

# ---- Welfare/discounting sensitivity analysis ----
ws_params <- read_csv(file.path(ws_dir, "run_configurations.csv"), show_col_types = FALSE)
ws_vars   <- read_variable_csvs(file.path(ws_dir, "by_variable"))
ws_gams   <- read_csv(file.path(ws_dir, "GAMS_reference.csv"), show_col_types = FALSE)

# ---- Quick check ----
cat("Feedback sensitivity: ", nrow(fs_params), "runs,", length(fs_vars), "variables\n")
cat("Welfare sensitivity:  ", nrow(ws_params), "runs,", length(ws_vars), "variables\n")
cat("Feedback variables:  ", paste(names(fs_vars), collapse = ", "), "\n")
cat("Welfare variables:   ", paste(names(ws_vars), collapse = ", "), "\n")


##########################################################

##VISUALIZATIONS

# Figure: Projected atmospheric temperature anomaly and atmospheric carbon
# concentration under three model configurations, 2020-2420
# Data source: welfare sensitivity results (ws_params / ws_vars / ws_gams),
# using the DICE 2023 / DICE-C / DICE-CP runs under default welfare and
# discounting settings (Economy.altdam = 0, Economy.altdisc = 0)
# Assumes ws_params / ws_vars / ws_gams are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 17) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 20, face = "bold"),
    axis.text = element_text(color = "black", size = 19, face = "bold"),
    legend.text = element_text(color = "black", size = 19),
    legend.title = element_text(color = "black", size = 20),
    plot.tag = element_text(color = "black", size = 21, face = "bold"),
    plot.margin = margin(t = 55, r = 12, b = 10, l = 10)
  )

# ---- Identify the three configuration runs within ws_params ----
# (default welfare/discounting settings: Economy.altdam = 0, Economy.altdisc = 0)

find_run <- function(params, switch_values) {
  out <- params
  for (col in names(switch_values)) out <- out[out[[col]] == switch_values[[col]], ]
  if (nrow(out) != 1) stop("Expected exactly one matching run, found ", nrow(out))
  out$Run
}

default_welfare <- c("Economy.altdam" = 0, "Economy.altdisc" = 0)

run_dice2023 <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 0,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicec    <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicecp   <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 1))

# ---- Pull temperature and carbon series for those three runs ----

find_var <- function(vars_list, pattern) {
  m <- grep(pattern, names(vars_list), value = TRUE, ignore.case = TRUE)
  if (length(m) != 1) stop("Expected exactly one variable matching '", pattern, "', found ", length(m))
  vars_list[[m]]
}

temp_data   <- find_var(ws_vars, "temperature_anomaly")
carbon_data <- find_var(ws_vars, "atmospheric_carbon")

extract_config <- function(run, label) {
  tibble(
    Year        = temp_data$Years,
    Config      = label,
    Temperature = temp_data[[paste0("Run_", run)]],
    Carbon      = carbon_data[[paste0("Run_", run)]]
  )
}

configs_df <- bind_rows(
  extract_config(run_dice2023, "DICE 2023"),
  extract_config(run_dicec,    "DICE-C"),
  extract_config(run_dicecp,   "DICE-CP")
) %>%
  mutate(Config = factor(Config, levels = c("DICE 2023", "DICE-C", "DICE-CP")))

# ---- Styling shared across panels ----

series_colors <- c(
  "DICE 2023" = "#1f77b4", "DICE-C" = "#ff7f0e",
  "DICE-CP" = "#d62728", "GAMS benchmark" = "black"
)
series_linetypes <- c(
  "DICE 2023" = "solid", "DICE-C" = "dashed",
  "DICE-CP" = "dotdash", "GAMS benchmark" = "blank"
)
series_shapes <- c(
  "DICE 2023" = NA, "DICE-C" = NA,
  "DICE-CP" = NA, "GAMS benchmark" = 21
)

# ---- Panels (a)/(b): time series with GAMS benchmark + peak-year vlines ----

make_timeseries_panel <- function(value_col, gams_col, y_lab, tag) {
  main <- configs_df %>% transmute(Year, Config, Value = .data[[value_col]])
  gams <- ws_gams %>% transmute(Year = Years, Config = "GAMS benchmark", Value = .data[[gams_col]])
  combined <- bind_rows(main, gams) %>%
    mutate(Config = factor(Config, levels = names(series_colors)))
  
  peak_years <- main %>%
    group_by(Config) %>%
    slice_max(Value, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  ggplot(combined, aes(Year, Value, color = Config, linetype = Config, shape = Config)) +
    geom_vline(data = peak_years, aes(xintercept = Year, color = Config),
               linewidth = 0.4, linetype = "dashed", alpha = 0.6, show.legend = FALSE) +
    geom_line(data = filter(combined, Config != "GAMS benchmark"), linewidth = 1) +
    geom_point(data = filter(combined, Config == "GAMS benchmark"), size = 1.2) +
    scale_color_manual(values = series_colors, name = NULL,
                       guide = guide_legend(override.aes = list(linewidth = 1))) +
    scale_linetype_manual(values = series_linetypes, name = NULL) +
    scale_shape_manual(values = series_shapes, name = NULL) +
    labs(x = "Year", y = y_lab, tag = tag) +
    base_theme +
    theme(legend.position = c(0.78, 0.75), legend.background = element_blank())
}

p_a <- make_timeseries_panel("Temperature", "temperature_anomaly",
                             "Temperature anomaly\n(\u00b0C above pre-industrial)", "(a)")
p_b <- make_timeseries_panel("Carbon", "atmospheric_co2",
                             "Atmospheric carbon (GtC)", "(b)")

# ---- Panels (c)/(d): phase plots with directional arrows ----

phase_df <- configs_df %>%
  mutate(ConfigKey = recode(Config, "DICE 2023" = "D23", "DICE-C" = "DC", "DICE-CP" = "DCP")) %>%
  select(Year, ConfigKey, Temperature, Carbon) %>%
  pivot_wider(names_from = ConfigKey, values_from = c(Temperature, Carbon)) %>%
  arrange(Year)

# Build a few arrowhead segments spaced evenly along the path's arc length
# (not evenly by index), so arrows don't bunch up where the curve moves slowly
make_arrows <- function(px, py, n_arrows = 6) {
  d <- sqrt(diff(px)^2 + diff(py)^2)
  cum_d <- c(0, cumsum(d))
  total <- cum_d[length(cum_d)]
  targets <- seq(0, total, length.out = n_arrows + 2)[2:(n_arrows + 1)]
  idx <- vapply(targets, function(t) which.min(abs(cum_d - t)), integer(1))
  idx <- unique(pmin(pmax(idx, 1), length(px) - 1))
  tibble(x = px[idx], y = py[idx], xend = px[idx + 1], yend = py[idx + 1])
}

make_phase_panel <- function(x_col, y_dc_col, y_dcp_col, axis_lab_x, axis_lab_y, tag) {
  rng <- range(c(phase_df[[x_col]], phase_df[[y_dc_col]], phase_df[[y_dcp_col]]), na.rm = TRUE)
  
  arrows_dc  <- make_arrows(phase_df[[x_col]], phase_df[[y_dc_col]])
  arrows_dcp <- make_arrows(phase_df[[x_col]], phase_df[[y_dcp_col]])
  
  ggplot(phase_df, aes(x = .data[[x_col]])) +
    geom_abline(slope = 1, intercept = 0, color = "grey60") +
    geom_path(aes(y = .data[[y_dc_col]], color = "DICE-C"), linewidth = 1, linetype = "dashed") +
    geom_path(aes(y = .data[[y_dcp_col]], color = "DICE-CP"), linewidth = 1, linetype = "dotdash") +
    geom_segment(data = arrows_dc, aes(x = x, y = y, xend = xend, yend = yend),
                 color = series_colors[["DICE-C"]], linewidth = 1,
                 arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
    geom_segment(data = arrows_dcp, aes(x = x, y = y, xend = xend, yend = yend),
                 color = series_colors[["DICE-CP"]], linewidth = 1,
                 arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
    scale_color_manual(values = series_colors, name = NULL,
                       breaks = c("DICE-C", "DICE-CP")) +
    coord_cartesian(xlim = rng, ylim = rng) +
    labs(x = axis_lab_x, y = axis_lab_y, tag = tag) +
    base_theme +
    theme(legend.position = c(0.8, 0.15), legend.background = element_blank())
}

p_c <- make_phase_panel(
  "Temperature_D23", "Temperature_DC", "Temperature_DCP",
  "DICE 2023 temperature anomaly\n(\u00b0C above pre-industrial)",
  "Feedback model\ntemperature anomaly\n(\u00b0C above pre-industrial)",
  "(c)"
)
p_d <- make_phase_panel(
  "Carbon_D23", "Carbon_DC", "Carbon_DCP",
  "DICE 2023 atmospheric carbon (GtC)",
  "Feedback model\natmospheric carbon (GtC)",
  "(d)"
)

# ---- Combine and save ----

fig <- (p_a | p_b) / plot_spacer() / (p_c | p_d) + plot_layout(heights = c(1, 0.06, 1))

ggsave("vis/fig_temp_co2.png", fig, width = 15, height = 12, dpi = 300)

# Figure: DICE configurations vs. CMIP6 SSP ensemble means, 2020-2100
# Data sources: CMIP_temp_by_scenario.csv (SSP temperature anomalies, already
# expressed relative to 1850-1900) and ws_vars/ws_params (welfare sensitivity
# results) for the DICE 2023 / DICE-C / DICE-CP runs.
# Assumes ws_params / ws_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# ---- Load CMIP6 SSP temperature data ----

cmip <- read_csv("CMIP_temp_by_scenario.csv", show_col_types = FALSE) %>%
  rename(
    `SSP1-2.6` = `temp ssp126`,
    `SSP2-4.5` = `temp ssp245`,
    `SSP3-7.0` = `temp ssp370`,
    `SSP5-8.5` = `temp ssp585`
  ) %>%
  filter(Year >= 2020, Year <= 2100)

# Rebase each SSP series to 2020 (subtract each column's 2020 value)
cmip <- cmip %>%
  mutate(across(-Year, ~ . - .[Year == 2020])+1.24)

ssp_long <- cmip %>%
  pivot_longer(-Year, names_to = "Series", values_to = "Value")

ssp_ribbon <- cmip %>%
  transmute(
    Year,
    ymin = pmin(`SSP1-2.6`, `SSP2-4.5`, `SSP3-7.0`, `SSP5-8.5`),
    ymax = pmax(`SSP1-2.6`, `SSP2-4.5`, `SSP3-7.0`, `SSP5-8.5`)
  )

# ---- Identify DICE 2023 / DICE-C / DICE-CP runs (default welfare/discounting) ----

find_run <- function(params, switch_values) {
  out <- params
  for (col in names(switch_values)) out <- out[out[[col]] == switch_values[[col]], ]
  if (nrow(out) != 1) stop("Expected exactly one matching run, found ", nrow(out))
  out$Run
}

default_welfare <- c("Economy.altdam" = 0, "Economy.altdisc" = 0)

run_dice2023 <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 0,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicec    <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicecp   <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 1))

find_var <- function(vars_list, pattern) {
  m <- grep(pattern, names(vars_list), value = TRUE, ignore.case = TRUE)
  if (length(m) != 1) stop("Expected exactly one variable matching '", pattern, "', found ", length(m))
  vars_list[[m]]
}

temp_data <- find_var(ws_vars, "temperature_anomaly")

dice_long <- bind_rows(
  tibble(Year = temp_data$Years, Series = "DICE 2023", Value = temp_data[[paste0("Run_", run_dice2023)]]),
  tibble(Year = temp_data$Years, Series = "DICE-C",    Value = temp_data[[paste0("Run_", run_dicec)]]),
  tibble(Year = temp_data$Years, Series = "DICE-CP",   Value = temp_data[[paste0("Run_", run_dicecp)]])
) %>%
  filter(Year >= 2020, Year <= 2100)

# ---- Combine and plot ----

series_levels <- c("DICE 2023", "DICE-C", "DICE-CP",
                   "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")

series_colors <- c(
  "DICE 2023" = "#000000", "DICE-C" = "#e67e22", "DICE-CP" = "#27ae60",
  "SSP1-2.6"  = "#8ecae6", "SSP2-4.5" = "#8e44ad",
  "SSP3-7.0"  = "#f4978e", "SSP5-8.5" = "#5c2a1a"
)

series_linewidth <- c(
  "DICE 2023" = 2.2, "DICE-C" = 2.2, "DICE-CP" = 2.2,
  "SSP1-2.6"  = 0.7, "SSP2-4.5" = 0.7, "SSP3-7.0" = 0.7, "SSP5-8.5" = 0.7
)

all_long <- bind_rows(dice_long, ssp_long) %>%
  mutate(Series = factor(Series, levels = series_levels))

p <- ggplot() +
  geom_ribbon(data = ssp_ribbon, aes(x = Year, ymin = ymin, ymax = ymax),
              fill = "grey85", alpha = 0.6) +
  geom_line(data = all_long, aes(Year, Value, color = Series, linewidth = Series)) +
  scale_color_manual(values = series_colors, name = NULL,
                     guide = guide_legend(override.aes = list(linewidth = 8))) +
  scale_linewidth_manual(values = series_linewidth, guide = "none") +
  labs(
    x = "Year", y = "Temperature anomaly\n(\u00b0C above 1850-1900)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    axis.title = element_text(color = "black", size = 17, face = "bold"),
    axis.text = element_text(color = "black", size = 16, face = "bold"),
    legend.text = element_text(color = "black", size = 16),
    legend.title = element_text(color = "black", size = 17),
    legend.key.width = unit(1.0, "cm"),
    legend.key.height = unit(1.0, "cm")
  )

ggsave("vis/fig_cmip6_comparison.png", p, width = 12, height = 8, dpi = 300)

ggsave("vis/fig_cmip6_comparison.png", p, width = 12, height = 8, dpi = 300)

# Figure 4: Isolated and conditional contributions of individual Lenton carbon
# cycle feedbacks to atmospheric temperature anomaly, 2020-2420
# Conditional on Lenton dynamics being active (Climate.Lenton? = 1), boxplots
# of peak temperature anomaly per run, split by each feedback's on/off state.
# Assumes fs_params / fs_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Peak temperature per run ----

temp_data <- fs_vars[[grep("temperature_anomaly", names(fs_vars), value = TRUE, ignore.case = TRUE)]]
run_cols  <- setdiff(names(temp_data), "Years")

peak_temp <- tibble(
  Run = as.integer(sub("Run_", "", run_cols)),
  PeakTemp = sapply(temp_data[run_cols], max, na.rm = TRUE)
)

# ---- Restrict to runs where Lenton dynamics are active, reshape to long ----

switch_labels <- c(
  "Lenton Carbon.Ocean CO2 switch"       = "Ocean CO2\nfeedback",
  "Lenton Carbon.Terr CO2 switch"        = "Terrestrial CO2\nfeedback",
  "Lenton Carbon.photo temp switch"      = "Photosynthesis\ntemp feedback",
  "Lenton Carbon.temp-resp switch soil"  = "Soil temp-\nresponse feedback",
  "Lenton Carbon.temp-resp switch veg"   = "Vegetation temp-\nresponse feedback",
  "Permafrost dynamics.permafrost switch" = "Permafrost\ndynamics"
)

long_df <- fs_params %>%
  filter(`Climate.Lenton?` == 1) %>%
  left_join(peak_temp, by = "Run") %>%
  select(Run, PeakTemp, all_of(names(switch_labels))) %>%
  pivot_longer(-c(Run, PeakTemp), names_to = "switch_col", values_to = "state") %>%
  mutate(
    Feedback = factor(switch_labels[switch_col], levels = unname(switch_labels)),
    State = factor(ifelse(state == 1, "On", "Off"), levels = c("Off", "On"))
  )

# ---- Plot ----

p <- ggplot(long_df, aes(x = Feedback, y = PeakTemp, fill = State)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, color = "black") +
  scale_fill_manual(values = c("Off" = "#5b7c99", "On" = "#e07a5f"), name = NULL) +
  labs(x = NULL, y = "Peak temperature anomaly\n(\u00b0C above pre-industrial)") +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 19, face = "bold"),
    axis.text = element_text(color = "black", size = 19, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 19),
    legend.text = element_text(color = "black", size = 17),
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(1.1, "cm"),
    plot.margin = margin(t = 15, r = 12, b = 10, l = 10)
  )

ggsave("vis/fig_feedback_boxplot.png", p, width = 16, height = 8, dpi = 300)

# Figure: Isolated and conditional contributions of individual Lenton carbon
# cycle feedbacks to atmospheric carbon concentration, 2020-2420
# Conditional on Lenton dynamics being active (Climate.Lenton? = 1), boxplots
# of peak atmospheric carbon per run, split by each feedback's on/off state.
# Assumes fs_params / fs_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Peak atmospheric carbon per run ----

carbon_data <- fs_vars[[grep("Atmospheric_Carbon", names(fs_vars), value = TRUE, ignore.case = TRUE)]]
run_cols    <- setdiff(names(carbon_data), "Years")

peak_carbon <- tibble(
  Run = as.integer(sub("Run_", "", run_cols)),
  PeakCarbon = sapply(carbon_data[run_cols], max, na.rm = TRUE)
)

# ---- Restrict to runs where Lenton dynamics are active, reshape to long ----

switch_labels <- c(
  "Lenton Carbon.Ocean CO2 switch"       = "Ocean CO2\nfeedback",
  "Lenton Carbon.Terr CO2 switch"        = "Terrestrial CO2\nfeedback",
  "Lenton Carbon.photo temp switch"      = "Photosynthesis\ntemp feedback",
  "Lenton Carbon.temp-resp switch soil"  = "Soil temp-\nresponse feedback",
  "Lenton Carbon.temp-resp switch veg"   = "Vegetation temp-\nresponse feedback",
  "Permafrost dynamics.permafrost switch" = "Permafrost\ndynamics"
)

long_df <- fs_params %>%
  filter(`Climate.Lenton?` == 1) %>%
  left_join(peak_carbon, by = "Run") %>%
  select(Run, PeakCarbon, all_of(names(switch_labels))) %>%
  pivot_longer(-c(Run, PeakCarbon), names_to = "switch_col", values_to = "state") %>%
  mutate(
    Feedback = factor(switch_labels[switch_col], levels = unname(switch_labels)),
    State = factor(ifelse(state == 1, "On", "Off"), levels = c("Off", "On"))
  )

# ---- Plot ----

p <- ggplot(long_df, aes(x = Feedback, y = PeakCarbon, fill = State)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, color = "black") +
  scale_fill_manual(values = c("Off" = "#5b7c99", "On" = "#e07a5f"), name = NULL) +
  labs(x = NULL, y = "Peak atmospheric carbon\n(GtC)") +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 19, face = "bold"),
    axis.text = element_text(color = "black", size = 15, face = "bold"),
    axis.text.x = element_text(size = 13),
    legend.text = element_text(color = "black", size = 17),
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(1.1, "cm"),
    plot.margin = margin(t = 15, r = 12, b = 10, l = 10)
  )

ggsave("fig_feedback_boxplot_carbon.png", p, width = 16, height = 8, dpi = 300)

# Figure: Climate damage fraction under three model configurations, 2020-2420
# Structurally matches fig_temp_co2.R, but for a single variable (damage
# fraction) rather than two, so there are two panels instead of four:
#   (a) time series with GAMS benchmark + peak-year vlines
#   (b) phase plot (DICE 2023 vs. DICE-C/DICE-CP) with 1:1 reference + arrows
# Data source: welfare sensitivity results (ws_params / ws_vars / ws_gams),
# using the DICE 2023 / DICE-C / DICE-CP runs under default welfare and
# discounting settings (Economy.altdam = 0, Economy.altdisc = 0)
# Assumes ws_params / ws_vars / ws_gams are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 15) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 17, face = "bold"),
    axis.text = element_text(color = "black", size = 16, face = "bold"),
    legend.text = element_text(color = "black", size = 16),
    legend.title = element_text(color = "black", size = 17),
    plot.tag = element_text(color = "black", size = 18, face = "bold"),
    plot.margin = margin(t = 34, r = 12, b = 10, l = 10)
  )

# ---- Identify the three configuration runs within ws_params ----
# (default welfare/discounting settings: Economy.altdam = 0, Economy.altdisc = 0)

find_run <- function(params, switch_values) {
  out <- params
  for (col in names(switch_values)) out <- out[out[[col]] == switch_values[[col]], ]
  if (nrow(out) != 1) stop("Expected exactly one matching run, found ", nrow(out))
  out$Run
}

default_welfare <- c("Economy.altdam" = 0, "Economy.altdisc" = 0)

run_dice2023 <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 0,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicec    <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicecp   <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 1))

# ---- Pull damage fraction series for those three runs ----

find_var <- function(vars_list, pattern) {
  m <- grep(pattern, names(vars_list), value = TRUE, ignore.case = TRUE)
  if (length(m) != 1) stop("Expected exactly one variable matching '", pattern, "', found ", length(m))
  vars_list[[m]]
}

dfrac_data <- find_var(ws_vars, "damages_as_fraction")

extract_config <- function(run, label) {
  tibble(
    Year   = dfrac_data$Years,
    Config = label,
    Dfrac  = dfrac_data[[paste0("Run_", run)]]
  )
}

configs_df <- bind_rows(
  extract_config(run_dice2023, "DICE 2023"),
  extract_config(run_dicec,    "DICE-C"),
  extract_config(run_dicecp,   "DICE-CP")
) %>%
  mutate(Config = factor(Config, levels = c("DICE 2023", "DICE-C", "DICE-CP")))

# ---- Styling shared across panels ----

series_colors <- c(
  "DICE 2023" = "#1f77b4", "DICE-C" = "#ff7f0e",
  "DICE-CP" = "#d62728", "GAMS benchmark" = "black"
)
series_linetypes <- c(
  "DICE 2023" = "solid", "DICE-C" = "dashed",
  "DICE-CP" = "dotdash", "GAMS benchmark" = "blank"
)
series_shapes <- c(
  "DICE 2023" = NA, "DICE-C" = NA,
  "DICE-CP" = NA, "GAMS benchmark" = 21
)

# ---- Panel (a): time series with GAMS benchmark + peak-year vlines ----

main <- configs_df %>% transmute(Year, Config, Value = Dfrac)
gams <- ws_gams %>% transmute(Year = Years, Config = "GAMS benchmark",
                              Value = damages_as_fraction_of_gross_output)
combined <- bind_rows(main, gams) %>%
  mutate(Config = factor(Config, levels = names(series_colors)))

peak_years <- main %>%
  group_by(Config) %>%
  slice_max(Value, n = 1, with_ties = FALSE) %>%
  ungroup()

p_a <- ggplot(combined, aes(Year, Value, color = Config, linetype = Config, shape = Config)) +
  geom_vline(data = peak_years, aes(xintercept = Year, color = Config),
             linewidth = 0.4, linetype = "dashed", alpha = 0.6, show.legend = FALSE) +
  geom_line(data = filter(combined, Config != "GAMS benchmark"), linewidth = 1) +
  geom_point(data = filter(combined, Config == "GAMS benchmark"), size = 1.2) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_linetype_manual(values = series_linetypes, name = NULL) +
  scale_shape_manual(values = series_shapes, name = NULL) +
  labs(x = "Year", y = "Climate damage fraction\n(fraction of gross output)", tag = "(a)") +
  base_theme +
  theme(legend.position = c(0.78, 0.75), legend.background = element_blank())

# ---- Panel (b): phase plot with directional arrows ----

phase_df <- configs_df %>%
  mutate(ConfigKey = recode(Config, "DICE 2023" = "D23", "DICE-C" = "DC", "DICE-CP" = "DCP")) %>%
  select(Year, ConfigKey, Dfrac) %>%
  pivot_wider(names_from = ConfigKey, values_from = Dfrac) %>%
  arrange(Year)

# Build a few arrowhead segments spaced evenly along the path's arc length
# (not evenly by index), so arrows don't bunch up where the curve moves slowly
make_arrows <- function(px, py, n_arrows = 6) {
  d <- sqrt(diff(px)^2 + diff(py)^2)
  cum_d <- c(0, cumsum(d))
  total <- cum_d[length(cum_d)]
  targets <- seq(0, total, length.out = n_arrows + 2)[2:(n_arrows + 1)]
  idx <- vapply(targets, function(t) which.min(abs(cum_d - t)), integer(1))
  idx <- unique(pmin(pmax(idx, 1), length(px) - 1))
  tibble(x = px[idx], y = py[idx], xend = px[idx + 1], yend = py[idx + 1])
}

rng <- range(c(phase_df$D23, phase_df$DC, phase_df$DCP), na.rm = TRUE)

arrows_dc  <- make_arrows(phase_df$D23, phase_df$DC)
arrows_dcp <- make_arrows(phase_df$D23, phase_df$DCP)

p_b <- ggplot(phase_df, aes(x = D23)) +
  geom_abline(slope = 1, intercept = 0, color = "grey60") +
  geom_path(aes(y = DC, color = "DICE-C"), linewidth = 1, linetype = "dashed") +
  geom_path(aes(y = DCP, color = "DICE-CP"), linewidth = 1, linetype = "dotdash") +
  geom_segment(data = arrows_dc, aes(x = x, y = y, xend = xend, yend = yend),
               color = series_colors[["DICE-C"]], linewidth = 1,
               arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
  geom_segment(data = arrows_dcp, aes(x = x, y = y, xend = xend, yend = yend),
               color = series_colors[["DICE-CP"]], linewidth = 1,
               arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
  scale_color_manual(values = series_colors, name = NULL,
                     breaks = c("DICE-C", "DICE-CP")) +
  coord_cartesian(xlim = rng, ylim = rng) +
  labs(
    x = "DICE 2023 climate damage fraction",
    y = "Feedback model climate damage fraction",
    tag = "(b)"
  ) +
  base_theme +
  theme(legend.position = c(0.8, 0.15), legend.background = element_blank())

# ---- Combine and save ----

fig <- p_a | p_b

ggsave("vis/fig_damage_fraction.png", fig, width = 15, height = 7, dpi = 300)

# Figure: Difference in economic output relative to the DICE 2023 baseline,
# 2020-2420 (single panel, per Dale's comments - abatement cost and
# accumulated utility deltas dropped; abatement cost moves to prose in the
# output/welfare section, accumulated utility becomes a final-value table)
# Data source: welfare sensitivity results (ws_params / ws_vars), using the
# DICE 2023 / DICE-C / DICE-CP runs under default welfare and discounting
# settings (Economy.altdam = 0, Economy.altdisc = 0)
# Assumes ws_params / ws_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 15) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 17, face = "bold"),
    axis.text = element_text(color = "black", size = 16, face = "bold"),
    legend.text = element_text(color = "black", size = 16),
    legend.title = element_text(color = "black", size = 17),
    plot.margin = margin(t = 15, r = 12, b = 10, l = 10)
  )

# ---- Identify the three configuration runs within ws_params ----
# (default welfare/discounting settings: Economy.altdam = 0, Economy.altdisc = 0)

find_run <- function(params, switch_values) {
  out <- params
  for (col in names(switch_values)) out <- out[out[[col]] == switch_values[[col]], ]
  if (nrow(out) != 1) stop("Expected exactly one matching run, found ", nrow(out))
  out$Run
}

default_welfare <- c("Economy.altdam" = 0, "Economy.altdisc" = 0)

run_dice2023 <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 0,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicec    <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicecp   <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 1))

# ---- Pull economic output series and compute deltas relative to DICE 2023 ----

find_var <- function(vars_list, pattern) {
  m <- grep(pattern, names(vars_list), value = TRUE, ignore.case = TRUE)
  if (length(m) != 1) stop("Expected exactly one variable matching '", pattern, "', found ", length(m))
  vars_list[[m]]
}

output_data <- find_var(ws_vars, "^Economy_Output_Y")

baseline <- output_data[[paste0("Run_", run_dice2023)]]

delta_df <- bind_rows(
  tibble(Year = output_data$Years, Config = "DICE-C",
         Delta = output_data[[paste0("Run_", run_dicec)]] - baseline),
  tibble(Year = output_data$Years, Config = "DICE-CP",
         Delta = output_data[[paste0("Run_", run_dicecp)]] - baseline)
) %>%
  mutate(Config = factor(Config, levels = c("DICE-C", "DICE-CP")))

# ---- Plot ----

series_colors    <- c("DICE-C" = "#ff7f0e", "DICE-CP" = "#d62728")
series_linetypes <- c("DICE-C" = "dashed",  "DICE-CP" = "dotted")

p <- ggplot(delta_df, aes(Year, Delta, color = Config, linetype = Config)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_linetype_manual(values = series_linetypes, name = NULL) +
  labs(
    x = "Year",
    # NOTE: update units to match your model's actual output units
    y = "Difference in economic output\nrelative to DICE 2023 baseline"
  ) +
  base_theme +
  theme(legend.position = c(0.15, 0.15), legend.background = element_blank())

ggsave("fig_output_delta.png", p, width = 10, height = 7, dpi = 300)

# Figure: Climate damage fraction under three model configurations, 2020-2420
# Structurally matches fig_temp_co2.R, but for a single variable (damage
# fraction) rather than two, so there are two panels instead of four:
#   (a) time series with GAMS benchmark + peak-year vlines
#   (b) phase plot (DICE 2023 vs. DICE-C/DICE-CP) with 1:1 reference + arrows
# Data source: welfare sensitivity results (ws_params / ws_vars / ws_gams),
# using the DICE 2023 / DICE-C / DICE-CP runs under default welfare and
# discounting settings (Economy.altdam = 0, Economy.altdisc = 0)
# Assumes ws_params / ws_vars / ws_gams are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 17) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 20, face = "bold"),
    axis.text = element_text(color = "black", size = 19, face = "bold"),
    legend.text = element_text(color = "black", size = 19),
    legend.title = element_text(color = "black", size = 20),
    plot.tag = element_text(color = "black", size = 21, face = "bold"),
    plot.margin = margin(t = 30, r = 12, b = 10, l = 10)
  )

# ---- Identify the three configuration runs within ws_params ----
# (default welfare/discounting settings: Economy.altdam = 0, Economy.altdisc = 0)

find_run <- function(params, switch_values) {
  out <- params
  for (col in names(switch_values)) out <- out[out[[col]] == switch_values[[col]], ]
  if (nrow(out) != 1) stop("Expected exactly one matching run, found ", nrow(out))
  out$Run
}

default_welfare <- c("Economy.altdam" = 0, "Economy.altdisc" = 0)

run_dice2023 <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 0,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicec    <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicecp   <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 1))

# ---- Pull damage fraction series for those three runs ----

find_var <- function(vars_list, pattern) {
  m <- grep(pattern, names(vars_list), value = TRUE, ignore.case = TRUE)
  if (length(m) != 1) stop("Expected exactly one variable matching '", pattern, "', found ", length(m))
  vars_list[[m]]
}

dfrac_data <- find_var(ws_vars, "damages_as_fraction")

extract_config <- function(run, label) {
  tibble(
    Year   = dfrac_data$Years,
    Config = label,
    Dfrac  = dfrac_data[[paste0("Run_", run)]]
  )
}

configs_df <- bind_rows(
  extract_config(run_dice2023, "DICE 2023"),
  extract_config(run_dicec,    "DICE-C"),
  extract_config(run_dicecp,   "DICE-CP")
) %>%
  mutate(Config = factor(Config, levels = c("DICE 2023", "DICE-C", "DICE-CP")))

# ---- Styling shared across panels ----

series_colors <- c(
  "DICE 2023" = "#1f77b4", "DICE-C" = "#ff7f0e",
  "DICE-CP" = "#d62728", "GAMS benchmark" = "black"
)
series_linetypes <- c(
  "DICE 2023" = "solid", "DICE-C" = "dashed",
  "DICE-CP" = "dotted", "GAMS benchmark" = "blank"
)
series_shapes <- c(
  "DICE 2023" = NA, "DICE-C" = NA,
  "DICE-CP" = NA, "GAMS benchmark" = 21
)

# ---- Panel (a): time series with GAMS benchmark + peak-year vlines ----

main <- configs_df %>% transmute(Year, Config, Value = Dfrac)
gams <- ws_gams %>% transmute(Year = Years, Config = "GAMS benchmark",
                              Value = damages_as_fraction_of_gross_output)
combined <- bind_rows(main, gams) %>%
  mutate(Config = factor(Config, levels = names(series_colors)))

peak_years <- main %>%
  group_by(Config) %>%
  slice_max(Value, n = 1, with_ties = FALSE) %>%
  ungroup()

p_a <- ggplot(combined, aes(Year, Value, color = Config, linetype = Config, shape = Config)) +
  geom_vline(data = peak_years, aes(xintercept = Year, color = Config),
             linewidth = 0.4, linetype = "dashed", alpha = 0.6, show.legend = FALSE) +
  geom_line(data = filter(combined, Config != "GAMS benchmark"), linewidth = 1) +
  geom_point(data = filter(combined, Config == "GAMS benchmark"), size = 1.2) +
  scale_color_manual(values = series_colors, name = NULL) +
  scale_linetype_manual(values = series_linetypes, name = NULL) +
  scale_shape_manual(values = series_shapes, name = NULL) +
  labs(x = "Year", y = "Climate damage fraction\n(fraction of gross output)", tag = "(a)") +
  base_theme +
  theme(legend.position = c(0.78, 0.75), legend.background = element_blank())

# ---- Panel (b): phase plot with directional arrows ----

phase_df <- configs_df %>%
  mutate(ConfigKey = recode(Config, "DICE 2023" = "D23", "DICE-C" = "DC", "DICE-CP" = "DCP")) %>%
  select(Year, ConfigKey, Dfrac) %>%
  pivot_wider(names_from = ConfigKey, values_from = Dfrac) %>%
  arrange(Year)

# Build a few arrowhead segments spaced evenly along the path's arc length
# (not evenly by index), so arrows don't bunch up where the curve moves slowly
make_arrows <- function(px, py, n_arrows = 6) {
  d <- sqrt(diff(px)^2 + diff(py)^2)
  cum_d <- c(0, cumsum(d))
  total <- cum_d[length(cum_d)]
  targets <- seq(0, total, length.out = n_arrows + 2)[2:(n_arrows + 1)]
  idx <- vapply(targets, function(t) which.min(abs(cum_d - t)), integer(1))
  idx <- unique(pmin(pmax(idx, 1), length(px) - 1))
  tibble(x = px[idx], y = py[idx], xend = px[idx + 1], yend = py[idx + 1])
}

rng <- range(c(phase_df$D23, phase_df$DC, phase_df$DCP), na.rm = TRUE)

arrows_dc  <- make_arrows(phase_df$D23, phase_df$DC)
arrows_dcp <- make_arrows(phase_df$D23, phase_df$DCP)

p_b <- ggplot(phase_df, aes(x = D23)) +
  geom_abline(slope = 1, intercept = 0, color = "grey60") +
  geom_path(aes(y = DC, color = "DICE-C"), linewidth = 1, linetype = "dashed") +
  geom_path(aes(y = DCP, color = "DICE-CP"), linewidth = 1, linetype = "dotted") +
  geom_segment(data = arrows_dc, aes(x = x, y = y, xend = xend, yend = yend),
               color = series_colors[["DICE-C"]], linewidth = 1,
               arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
  geom_segment(data = arrows_dcp, aes(x = x, y = y, xend = xend, yend = yend),
               color = series_colors[["DICE-CP"]], linewidth = 1,
               arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
  scale_color_manual(values = series_colors, name = NULL,
                     breaks = c("DICE-C", "DICE-CP")) +
  coord_cartesian(xlim = rng, ylim = rng) +
  labs(
    x = "DICE 2023 climate damage fraction",
    y = "Feedback model climate damage fraction",
    tag = "(b)"
  ) +
  base_theme +
  theme(legend.position = c(0.8, 0.15), legend.background = element_blank())

# ---- Combine and save ----

fig <- p_a | p_b

ggsave("fig_damage_fraction.png", fig, width = 15, height = 7, dpi = 300)


# Figure: Welfare loss vs. DICE 2023 standard-damage baseline, across damage
# function and discounting assumptions (slope graph showing sensitivity of
# welfare loss to the damage function choice, faceted by discount regime)
# Data source: welfare sensitivity results (ws_params / ws_vars), using final-
# year accumulated utility. Each discount regime (standard vs. zero time
# preference) is normalized against its own DICE 2023 / standard-damage run,
# since welfare totals are not comparable across discounting conventions.
# Assumes ws_params / ws_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 15) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 17, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    strip.text = element_text(color = "black", size = 16, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.text = element_text(color = "black", size = 16),
    legend.title = element_text(color = "black", size = 17),
    plot.margin = margin(t = 15, r = 12, b = 10, l = 10)
  )

# ---- Compute welfare loss (%) for each config x damage x discount combo ----

util <- ws_vars[[grep("Accumulated_Utility", names(ws_vars), value = TRUE, ignore.case = TRUE)]]
final_util <- util %>% filter(Years == max(Years))

run_util <- tibble(
  Run = as.integer(sub("Run_", "", names(final_util)[-1])),
  Utility = as.numeric(final_util[1, -1])
)

df <- ws_params %>%
  left_join(run_util, by = "Run") %>%
  # drop the "permafrost only, Lenton off" combination - not one of the three named configs
  filter(!(`Climate.Lenton?` == 0 & `Permafrost dynamics.permafrost switch` == 1)) %>%
  mutate(
    Config = case_when(
      `Climate.Lenton?` == 0 ~ "DICE 2023",
      `Climate.Lenton?` == 1 & `Permafrost dynamics.permafrost switch` == 0 ~ "DICE-C",
      `Climate.Lenton?` == 1 & `Permafrost dynamics.permafrost switch` == 1 ~ "DICE-CP"
    ),
    Config = factor(Config, levels = c("DICE 2023", "DICE-C", "DICE-CP")),
    Damage = factor(ifelse(Economy.altdam == 0, "Standard\ndamage", "Howard-Sterner\ndamage"),
                    levels = c("Standard\ndamage", "Howard-Sterner\ndamage")),
    Discount = ifelse(Economy.altdisc == 0, "Standard discount", "Zero time preference")
  ) %>%
  group_by(Discount) %>%
  mutate(Baseline = Utility[Config == "DICE 2023" & Damage == "Standard\ndamage"]) %>%
  ungroup() %>%
  mutate(WelfareLoss = (Utility - Baseline) / Baseline * 100)

# ---- Plot ----

series_colors <- c("DICE 2023" = "#1f77b4", "DICE-C" = "#ff7f0e", "DICE-CP" = "#d62728")

p <- ggplot(df, aes(x = Damage, y = WelfareLoss, group = Config, color = Config)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4.5) +
  facet_wrap(~Discount) +
  scale_color_manual(values = series_colors, name = NULL) +
  labs(x = NULL, y = "Welfare loss vs. DICE 2023\nstandard-damage baseline (%)") +
  base_theme

ggsave("vis/fig_welfare_slopegraph.png", p, width = 12, height = 6.5, dpi = 300)

# Figure: Welfare loss vs. DICE 2023 standard-damage baseline, across damage
# function and discounting assumptions (grouped bar chart, faceted by discount
# regime, shared y-axis for direct visual comparison across panels)
# Data source: welfare sensitivity results (ws_params / ws_vars), using final-
# year accumulated utility. Each discount regime (standard vs. zero time
# preference) is normalized against its own DICE 2023 / standard-damage run,
# since welfare totals are not comparable across discounting conventions.
# Assumes ws_params / ws_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 15) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 17, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    strip.text = element_text(color = "black", size = 16, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.text = element_text(color = "black", size = 16),
    legend.title = element_text(color = "black", size = 17),
    plot.margin = margin(t = 15, r = 12, b = 10, l = 10)
  )

# ---- Compute welfare loss (%) for each config x damage x discount combo ----

util <- ws_vars[[grep("Accumulated_Utility", names(ws_vars), value = TRUE, ignore.case = TRUE)]]
final_util <- util %>% filter(Years == max(Years))

run_util <- tibble(
  Run = as.integer(sub("Run_", "", names(final_util)[-1])),
  Utility = as.numeric(final_util[1, -1])
)

df <- ws_params %>%
  left_join(run_util, by = "Run") %>%
  # drop the "permafrost only, Lenton off" combination - not one of the three named configs
  filter(!(`Climate.Lenton?` == 0 & `Permafrost dynamics.permafrost switch` == 1)) %>%
  mutate(
    Config = case_when(
      `Climate.Lenton?` == 0 ~ "DICE 2023",
      `Climate.Lenton?` == 1 & `Permafrost dynamics.permafrost switch` == 0 ~ "DICE-C",
      `Climate.Lenton?` == 1 & `Permafrost dynamics.permafrost switch` == 1 ~ "DICE-CP"
    ),
    Config = factor(Config, levels = c("DICE 2023", "DICE-C", "DICE-CP")),
    Damage = ifelse(Economy.altdam == 0, "Standard damage", "Howard-Sterner damage"),
    Damage = factor(Damage, levels = c("Standard damage", "Howard-Sterner damage")),
    Discount = ifelse(Economy.altdisc == 0, "Standard discount", "Zero time preference")
  ) %>%
  group_by(Discount) %>%
  mutate(Baseline = Utility[Config == "DICE 2023" & Damage == "Standard damage"]) %>%
  ungroup() %>%
  mutate(WelfareLoss = (Utility - Baseline) / Baseline * 100)

# ---- Plot ----

damage_colors <- c("Standard damage" = "#e8a33d", "Howard-Sterner damage" = "#2a9d8f")

p <- ggplot(df, aes(x = Config, y = WelfareLoss, fill = Damage)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", WelfareLoss),
                vjust = ifelse(WelfareLoss < 0, 1.4, -0.6)),
            position = position_dodge(width = 0.75), size = 4.5, fontface = "bold") +
  facet_wrap(~Discount) +
  scale_fill_manual(values = damage_colors, name = NULL) +
  labs(x = NULL, y = "Welfare loss vs. DICE 2023\nstandard-damage baseline (%)") +
  base_theme

ggsave("vis/fig_welfare_barchart.png", p, width = 12, height = 6.5, dpi = 300)

# Figure: Economic output, abatement cost, and discounted social welfare per
# year under three model configurations, 2020-2420 (levels, not deltas, per
# Dale's comments - the deltas figure remains separate and unchanged)
# Panel (c) is the discounted per-year welfare flow (d(Uacc)/dt), not the
# running accumulated-utility sum, since only the final-year total and the
# per-period flow are informative on their own.
# Data source: welfare sensitivity results (ws_params / ws_vars), using the
# DICE 2023 / DICE-C / DICE-CP runs under default welfare and discounting
# settings (Economy.altdam = 0, Economy.altdisc = 0)
# Assumes ws_params / ws_vars are already loaded (see import_data.R)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---- Shared theme: larger, darker text throughout ----

base_theme <- theme_bw(base_size = 17) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 20, face = "bold"),
    axis.text = element_text(color = "black", size = 19, face = "bold"),
    legend.text = element_text(color = "black", size = 19),
    legend.title = element_text(color = "black", size = 20),
    plot.tag = element_text(color = "black", size = 21, face = "bold"),
    plot.tag.position = "topleft",
    plot.margin = margin(t = 30, r = 12, b = 10, l = 10)
  )

# ---- Identify the three configuration runs within ws_params ----
# (default welfare/discounting settings: Economy.altdam = 0, Economy.altdisc = 0)

find_run <- function(params, switch_values) {
  out <- params
  for (col in names(switch_values)) out <- out[out[[col]] == switch_values[[col]], ]
  if (nrow(out) != 1) stop("Expected exactly one matching run, found ", nrow(out))
  out$Run
}

default_welfare <- c("Economy.altdam" = 0, "Economy.altdisc" = 0)

run_dice2023 <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 0,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicec    <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 0))
run_dicecp   <- find_run(ws_params, c(default_welfare, "Climate.Lenton?" = 1,
                                      "Permafrost dynamics.permafrost switch" = 1))

find_var <- function(vars_list, pattern) {
  m <- grep(pattern, names(vars_list), value = TRUE, ignore.case = TRUE)
  if (length(m) != 1) stop("Expected exactly one variable matching '", pattern, "', found ", length(m))
  vars_list[[m]]
}

output_data <- find_var(ws_vars, "Ygross")
cabate_data <- find_var(ws_vars, "cost_of_emissions_abatement")
uacc_data   <- find_var(ws_vars, "Accumulated_Utility")

# ---- Styling shared across panels ----

series_colors <- c("DICE 2023" = "#1f77b4", "DICE-C" = "#ff7f0e", "DICE-CP" = "#d62728")
series_linetypes <- c("DICE 2023" = "solid", "DICE-C" = "dashed", "DICE-CP" = "dotted")

configs <- tibble(
  run = c(run_dice2023, run_dicec, run_dicecp),
  Config = factor(c("DICE 2023", "DICE-C", "DICE-CP"), levels = c("DICE 2023", "DICE-C", "DICE-CP"))
)

# Extract a level-series (Years, Config, Value) for a given variable's wide table
extract_levels <- function(var_data) {
  purrr::pmap_dfr(configs, function(run, Config) {
    tibble(Year = var_data$Years, Config = Config, Value = var_data[[paste0("Run_", run)]])
  })
}

# Extract a per-year discounted flow by differencing a cumulative series
extract_flow <- function(var_data) {
  purrr::pmap_dfr(configs, function(run, Config) {
    y <- var_data$Years
    v <- var_data[[paste0("Run_", run)]]
    tibble(Year = y[-1], Config = Config, Value = diff(v) / diff(y))
  })
}

# ---- Generic panel builder: levels, no GAMS overlay ----

make_panel <- function(combined, y_lab, tag, legend_pos) {
  ggplot(combined, aes(Year, Value, color = Config, linetype = Config)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = series_colors, name = NULL) +
    scale_linetype_manual(values = series_linetypes, name = NULL) +
    labs(x = "Year", y = y_lab, tag = tag) +
    base_theme +
    theme(legend.position = legend_pos, legend.background = element_blank())
}

# ---- Panel (a): Economic output (levels) ----

output_main <- extract_levels(output_data) %>%
  mutate(Config = factor(Config, levels = names(series_colors)))

p_a <- make_panel(output_main, "Gross economic output\n(trillions 2019 USD)", "(a)", c(0.2, 0.8))

# ---- Panel (b): Abatement cost (levels) ----

cabate_main <- extract_levels(cabate_data) %>%
  mutate(Config = factor(Config, levels = names(series_colors)))

p_b <- make_panel(cabate_main, "Cost of emissions abatement\n(trillions 2019 USD)", "(b)", c(0.2, 0.8))

# ---- Panel (c): Discounted social welfare per year ----

welfare_main <- extract_flow(uacc_data) %>%
  mutate(Config = factor(Config, levels = names(series_colors)))

p_c <- make_panel(welfare_main, "Discounted social welfare\nper year (dimensionless)", "(c)", c(0.8, 0.8))

# ---- Combine and save ----

fig <- p_a | p_b | p_c

ggsave("fig_output_welfare_levels.png", fig, width = 20, height = 7, dpi = 300)