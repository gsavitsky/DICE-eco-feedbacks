# STELLA Sensitivity Analysis Output Organizer
# Reads feedbacksensi.xlsx (feedback sensitivity) and FINALresults.xlsx
# (welfare/discounting sensitivity) and writes organized CSVs:
#   - run_configurations.csv per file (constant parameters, one row per run)
#   - response variable time series, split by variable OR by run
#   - GAMS reference results, passed through as-is

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---- CONFIG: update to match your machine ----
input_dir  <- "STELLA results"
output_dir <- "vis"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Parse "Years", "Run X: Module.Variable" columns into long format
parse_stella_sheet <- function(df, year_col = "Years") {
  df %>%
    rename(Years = all_of(year_col)) %>%
    pivot_longer(-Years, names_to = "raw_col", values_to = "Value") %>%
    mutate(
      Run      = as.integer(str_match(raw_col, "^Run (\\d+):")[, 2]),
      rest     = str_trim(str_match(raw_col, "^Run \\d+:\\s*(.+)$")[, 2]),
      Module   = str_trim(str_match(rest, "^([^.]+)\\.")[, 2]),
      Variable = str_trim(str_match(rest, "^[^.]+\\.(.+)$")[, 2])
    ) %>%
    select(Years, Run, Module, Variable, Value)
}

# Collapse a parsed parameter sheet into one row per run (parameters are
# constant through time). Warns if any variable isn't actually constant.
summarize_parameters <- function(parsed_long) {
  bad <- parsed_long %>%
    group_by(Run, Module, Variable) %>%
    summarize(n_unique = n_distinct(Value), .groups = "drop") %>%
    filter(n_unique > 1)
  if (nrow(bad) > 0) warning("Non-constant 'parameter' values found:\n",
                             paste(capture.output(print(bad)), collapse = "\n"))
  
  parsed_long %>%
    group_by(Run, Module, Variable) %>%
    summarize(Value = first(Value), .groups = "drop") %>%
    mutate(colname = paste(Module, Variable, sep = ".")) %>%
    select(Run, colname, Value) %>%
    pivot_wider(names_from = colname, values_from = Value) %>%
    arrange(Run)
}

#Write one CSV per variable (Module.Variable): Years as rows, runs as columns
write_by_variable <- function(parsed_long, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  groups <- parsed_long %>% distinct(Module, Variable)
  walk2(groups$Module, groups$Variable, function(mod, var) {
    sub <- parsed_long %>%
      filter(Module == mod, Variable == var) %>%
      transmute(Years, RunLabel = paste0("Run_", Run), Value) %>%
      pivot_wider(names_from = RunLabel, values_from = Value) %>%
      arrange(Years)
    fname <- str_replace_all(paste(mod, var, sep = "_"), "[^A-Za-z0-9]+", "_")
    write.csv(sub, file.path(out_dir, paste0(fname, ".csv")), row.names = FALSE)
  })
}

# ---- feedbacksensi.xlsx ----
fb_params   <- parse_stella_sheet(read_excel(file.path(input_dir, "feedbacksensi.xlsx"), sheet = "parameters"))
fb_response <- parse_stella_sheet(read_excel(file.path(input_dir, "feedbacksensi.xlsx"), sheet = "response variables"))

fb_out <- file.path(output_dir, "feedback sensitivity")
dir.create(fb_out, showWarnings = FALSE, recursive = TRUE)
write.csv(summarize_parameters(fb_params), file.path(fb_out, "run_configurations.csv"), row.names = FALSE)
write_by_variable(fb_response, file.path(fb_out, "by_variable"))

# ---- FINALresults.xlsx ----
fr_results <- parse_stella_sheet(read_excel(file.path(input_dir, "FINALresults.xlsx"), sheet = "Results"))
fr_params  <- parse_stella_sheet(read_excel(file.path(input_dir, "FINALresults.xlsx"), sheet = "Params"))
fr_gams    <- read_excel(file.path(input_dir, "FINALresults.xlsx"), sheet = "GAMS")

fr_out <- file.path(output_dir, "Welfare sensitivity")
dir.create(fr_out, showWarnings = FALSE, recursive = TRUE)
write.csv(summarize_parameters(fr_params), file.path(fr_out, "run_configurations.csv"), row.names = FALSE)
write_by_variable(fr_results, file.path(fr_out, "by_variable"))

# GAMS sheet isn't in "Run X:" format (single reference run) - pass through
write.csv(fr_gams %>% rename(Years = Year) %>% arrange(Years),
          file.path(fr_out, "GAMS_reference.csv"), row.names = FALSE)

cat("Done. Organized CSVs written to:", output_dir, "\n")
