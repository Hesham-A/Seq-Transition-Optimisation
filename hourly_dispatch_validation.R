# ============================================================
# HOURLY DISPATCH VALIDATION / VISUALIZATION
# ============================================================
# Purpose:
#   Demonstrate how the representative-day method retains actual
#   24-hour heat-demand profiles and how the optimization responds
#   to those profiles in selected future years.
#
# This is POST-PROCESSING ONLY. It does not rerun or modify the LP.
#
# The script:
#   1) plots all selected representative-day demand profiles;
#   2) selects one representative day per season for a readable
#      future-operation illustration;
#   3) applies the scenario demand multiplier for 2030, 2040, 2050;
#   4) combines the scenario-scaled demand with the saved hourly
#      technology dispatch from the optimization;
#   5) includes thermal-storage discharge when available;
#   6) calculates hourly technology shares;
#   7) checks the hourly heat balance;
#   8) exports plotting data and figures.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

# ------------------------------------------------------------
# 0. Results directory
# ------------------------------------------------------------
if (exists("results_dir", inherits = TRUE)) {
  results_dir_use <- results_dir
} else {
  results_dir_use <- file.path(
    "results",
    "dh_transition_model_results"
  )
}

plot_dir_use <- file.path(results_dir_use, "plots")
dir.create(plot_dir_use, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Required files
# ------------------------------------------------------------
heat_hourly_file <- file.path(
  results_dir_use,
  "heat_dispatch_hourly_by_representative_day.csv"
)
rep_profile_file <- file.path(
  results_dir_use,
  "representative_hourly_profiles.csv"
)
rep_days_file <- file.path(
  results_dir_use,
  "representative_days.csv"
)
scenario_file <- file.path(
  results_dir_use,
  "scenario_table.csv"
)

required_files <- c(
  heat_hourly_file,
  rep_profile_file,
  rep_days_file,
  scenario_file
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required output file(s): ",
    paste(basename(missing_files), collapse = ", "),
    ". The modified main model must be run first."
  )
}

heat_hourly <- read_csv(heat_hourly_file, show_col_types = FALSE)
rep_profile <- read_csv(rep_profile_file, show_col_types = FALSE)
rep_days <- read_csv(rep_days_file, show_col_types = FALSE)
scenario_table <- read_csv(scenario_file, show_col_types = FALSE)

# ------------------------------------------------------------
# 2. Required-column checks and type normalization
# ------------------------------------------------------------
check_columns <- function(df, required, object_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      object_name, " is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }
}

check_columns(
  heat_hourly,
  c("scenario", "planning_year", "rep_id", "hour", "technology", "dispatch_mw"),
  "heat_hourly"
)

check_columns(
  rep_profile,
  c("rep_id", "source_date", "season", "hour", "demand_mw"),
  "rep_profile"
)

check_columns(
  rep_days,
  c("rep_id", "day_id", "season", "weight_days"),
  "rep_days"
)

check_columns(
  scenario_table,
  c("scenario", "planning_year", "demand_mult"),
  "scenario_table"
)

heat_hourly <- heat_hourly |>
  mutate(
    scenario = as.character(scenario),
    planning_year = as.integer(planning_year),
    rep_id = as.integer(rep_id),
    hour = as.integer(hour),
    technology = as.character(technology),
    dispatch_mw = as.numeric(dispatch_mw)
  )

rep_profile <- rep_profile |>
  mutate(
    rep_id = as.integer(rep_id),
    source_date = as.Date(source_date),
    season = as.character(season),
    hour = as.integer(hour),
    demand_mw = as.numeric(demand_mw)
  )

rep_days <- rep_days |>
  mutate(
    rep_id = as.integer(rep_id),
    day_id = as.Date(day_id),
    season = as.character(season),
    weight_days = as.numeric(weight_days)
  )

scenario_table <- scenario_table |>
  mutate(
    scenario = as.character(scenario),
    planning_year = as.integer(planning_year),
    demand_mult = as.numeric(demand_mult)
  )

# ------------------------------------------------------------
# 3. Basic integrity checks
# ------------------------------------------------------------
if (anyDuplicated(rep_days$rep_id) > 0) {
  dup_ids <- rep_days |>
    count(rep_id) |>
    filter(n > 1)
  print(dup_ids)
  stop("representative_days.csv contains duplicated rep_id values.")
}

rep_profile_check <- rep_profile |>
  count(rep_id, name = "n_hours")

if (any(rep_profile_check$n_hours != 24)) {
  print(rep_profile_check |> filter(n_hours != 24))
  stop("Each representative day must contain exactly 24 hourly observations.")
}

# ------------------------------------------------------------
# 4. Plot all representative-day heat-demand profiles
# ------------------------------------------------------------
# These are the actual historical 24-hour medoid profiles retained
# by the method. No optimisation is performed here.

rep_demand <- rep_profile |>
  select(rep_id, source_date, season, hour, demand_mw) |>
  mutate(
    representative_day = paste0("R", rep_id, " (", season, ")")
  ) |>
  arrange(season, rep_id, hour)

if (nrow(rep_demand) == 0) {
  stop("No representative-day demand profiles were found.")
}

p_rep_demand <- ggplot(
  rep_demand,
  aes(
    x = hour,
    y = demand_mw,
    group = representative_day,
    linetype = representative_day
  )
) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ season, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 1:24) +
  labs(
    x = "Hour of day",
    y = "Heat demand (MW)",
    linetype = "Representative day",
    title = "Hourly heat-demand profiles of selected representative days",
    subtitle = "Each curve is an actual 24-hour historical profile retained by the representative-day method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(p_rep_demand)

ggsave(
  file.path(plot_dir_use, "validation_representative_day_heat_profiles.png"),
  p_rep_demand,
  width = 10,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------
# 5. Select one dominant representative day per season
# ------------------------------------------------------------
# This selection is ONLY for a readable future-operation illustration.
# The optimisation itself used all representative days.
selected_rep_days <- rep_days |>
  group_by(season) |>
  slice_max(
    order_by = weight_days,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  transmute(
    rep_id = as.integer(rep_id),
    season = as.character(season),
    representative_date = as.Date(day_id),
    weight_days = as.numeric(weight_days)
  )

if (nrow(selected_rep_days) == 0) {
  stop("No representative days could be selected.")
}

if (anyDuplicated(selected_rep_days$rep_id) > 0) {
  stop("Selected representative days contain duplicate rep_id values.")
}

cat("Selected representative days for future-operation illustration:\n")
print(selected_rep_days)

# ------------------------------------------------------------
# 6. Build future demand profiles
# ------------------------------------------------------------
# IMPORTANT:
# rep_id is the unique identifier of each representative day.
# We therefore use rep_id for all joins between representative-day
# profiles and optimisation dispatch. Season is attached from rep_days,
# but is NOT used as a join key.

selected_profiles <- rep_profile |>
  semi_join(
    selected_rep_days |> select(rep_id),
    by = "rep_id"
  ) |>
  select(rep_id, hour, demand_mw)

# Ensure that every selected representative day has exactly 24 hours.
selected_profile_check <- selected_profiles |>
  count(rep_id, name = "n_hours")

if (nrow(selected_profile_check) != nrow(selected_rep_days) ||
    any(selected_profile_check$n_hours != 24)) {
  print(selected_profile_check)
  stop("Selected representative-day profiles are incomplete or missing.")
}

selected_years <- c(2030L, 2040L, 2050L)
selected_scenarios <- c("Scenario I", "Scenario II", "Scenario III")

selected_scenario_table <- scenario_table |>
  filter(
    planning_year %in% selected_years,
    scenario %in% selected_scenarios
  ) |>
  select(scenario, planning_year, demand_mult)

expected_scenario_rows <- length(selected_years) * length(selected_scenarios)

if (nrow(selected_scenario_table) != expected_scenario_rows) {
  stop(
    "scenario_table does not contain exactly one demand multiplier for each ",
    "selected scenario/year combination."
  )
}

scenario_duplicates <- selected_scenario_table |>
  count(scenario, planning_year) |>
  filter(n != 1)

if (nrow(scenario_duplicates) > 0) {
  print(scenario_duplicates)
  stop("scenario_table contains duplicate selected scenario/year combinations.")
}

future_demand <- selected_profiles |>
  crossing(
    scenario = selected_scenarios,
    planning_year = selected_years
  ) |>
  left_join(
    selected_scenario_table,
    by = c("scenario", "planning_year")
  ) |>
  mutate(
    demand_required_mw = demand_mw * demand_mult,
    network_heat_required_mw = demand_required_mw * 1.03
  ) |>
  arrange(scenario, planning_year, rep_id, hour)

expected_future_rows <- nrow(selected_profiles) *
  length(selected_years) *
  length(selected_scenarios)

if (nrow(future_demand) != expected_future_rows) {
  stop("Unexpected number of rows in future_demand.")
}

if (anyNA(future_demand$demand_mult) ||
    anyNA(future_demand$demand_required_mw)) {
  stop("Missing scenario-scaled future demand values.")
}

# Add the season only for labelling.
future_demand <- future_demand |>
  left_join(
    selected_rep_days |> select(rep_id, season),
    by = "rep_id"
  )

if (anyNA(future_demand$season)) {
  stop("Some selected representative days could not be assigned a season.")
}

# ------------------------------------------------------------
# 7. Restrict saved hourly dispatch to the selected representative days
# ------------------------------------------------------------
# heat_hourly does not contain season, so season is attached from rep_days.
# Crucially, this join uses rep_id only.

heat_hourly_selected <- heat_hourly |>
  filter(
    planning_year %in% selected_years,
    scenario %in% selected_scenarios,
    rep_id %in% selected_rep_days$rep_id
  ) |>
  left_join(
    selected_rep_days |> select(rep_id, season),
    by = "rep_id"
  ) |>
  select(
    scenario,
    planning_year,
    rep_id,
    season,
    hour,
    technology,
    dispatch_mw
  )

if (nrow(heat_hourly_selected) == 0) {
  stop(
    "No hourly dispatch rows were found for the selected years, scenarios, " ,
    "and representative days."
  )
}

# Basic check: every selected rep_id/hour in the dispatch output must exist
# in the representative profile.
dispatch_hours <- heat_hourly_selected |>
  distinct(rep_id, hour)

missing_profile_hours <- dispatch_hours |>
  anti_join(selected_profiles |> distinct(rep_id, hour),
            by = c("rep_id", "hour"))

if (nrow(missing_profile_hours) > 0) {
  print(head(missing_profile_hours, 30))
  stop("Some dispatch representative-day hours do not exist in the selected profiles.")
}

# ------------------------------------------------------------
# 8. Add thermal-storage discharge, when available
# ------------------------------------------------------------
storage_file <- file.path(results_dir_use, "storage_operation.csv")

if (file.exists(storage_file)) {
  storage_operation <- read_csv(storage_file, show_col_types = FALSE)

  required_storage_cols <- c(
    "scenario", "planning_year", "d", "h", "discharge_mw"
  )

  if (all(required_storage_cols %in% names(storage_operation))) {
    storage_hourly <- storage_operation |>
      filter(
        planning_year %in% selected_years,
        scenario %in% selected_scenarios,
        d %in% selected_rep_days$rep_id
      ) |>
      transmute(
        scenario = as.character(scenario),
        planning_year = as.integer(planning_year),
        rep_id = as.integer(d),
        hour = as.integer(h),
        technology = "Thermal storage discharge",
        dispatch_mw = pmax(0, as.numeric(discharge_mw))
      ) |>
      left_join(
        selected_rep_days |> select(rep_id, season),
        by = "rep_id"
      ) |>
      select(
        scenario,
        planning_year,
        rep_id,
        season,
        hour,
        technology,
        dispatch_mw
      )

    if (nrow(storage_hourly) > 0) {
      heat_hourly_selected <- bind_rows(
        heat_hourly_selected,
        storage_hourly
      )
    }
  }
}

# ------------------------------------------------------------
# 9. Match dispatch to scenario-scaled demand
# ------------------------------------------------------------
# IMPORTANT:
# The demand profile is uniquely identified by scenario + planning_year
# + rep_id + hour. Season is a descriptive attribute and is deliberately
# NOT used as a join key.

dispatch_plot <- heat_hourly_selected |>
  left_join(
    future_demand |>
      select(
        scenario,
        planning_year,
        rep_id,
        hour,
        season,
        demand_required_mw,
        network_heat_required_mw
      ),
    by = c("scenario", "planning_year", "rep_id", "hour"),
    suffix = c("_dispatch", "_demand")
  )

if (anyNA(dispatch_plot$demand_required_mw)) {
  missing_rows <- dispatch_plot |>
    filter(is.na(demand_required_mw)) |>
    distinct(scenario, planning_year, rep_id, season_dispatch, hour)

  cat("\nRows that failed to match demand:\n")
  print(head(missing_rows, 30))

  stop(
    "Some hourly dispatch rows could not be matched to the scenario-scaled ",
    "representative-day demand profile."
  )
}

# Keep one season column for subsequent plotting.
dispatch_plot <- dispatch_plot |>
  mutate(
    season = coalesce(season_dispatch, season_demand)
  ) |>
  select(
    scenario,
    planning_year,
    rep_id,
    season,
    hour,
    technology,
    dispatch_mw,
    demand_required_mw,
    network_heat_required_mw
  )

# ------------------------------------------------------------
# 10. Calculate technology shares and heat-supply balance
# ------------------------------------------------------------
dispatch_plot <- dispatch_plot |>
  group_by(
    scenario,
    planning_year,
    rep_id,
    season,
    hour
  ) |>
  mutate(
    total_supply_mw = sum(dispatch_mw, na.rm = TRUE),
    supply_share = ifelse(
      total_supply_mw > 0,
      dispatch_mw / total_supply_mw,
      0
    ),
    supply_minus_required_mw = total_supply_mw - network_heat_required_mw
  ) |>
  ungroup()

balance_check <- dispatch_plot |>
  distinct(
    scenario,
    planning_year,
    rep_id,
    season,
    hour,
    total_supply_mw,
    demand_required_mw,
    network_heat_required_mw,
    supply_minus_required_mw
  )

max_balance_error <- max(
  abs(balance_check$supply_minus_required_mw),
  na.rm = TRUE
)

cat(
  "Maximum absolute hourly heat-supply balance difference: ",
  format(max_balance_error, digits = 10),
  " MW\n",
  sep = ""
)

# The LP heat-balance equation includes 3% network losses, therefore the
# required-supply line is demand * 1.03.

# 11. Export data
# ------------------------------------------------------------
write_csv(
  selected_rep_days,
  file.path(
    results_dir_use,
    "hourly_validation_selected_representative_days.csv"
  )
)

write_csv(
  future_demand,
  file.path(
    results_dir_use,
    "hourly_validation_future_demand_profiles.csv"
  )
)

write_csv(
  dispatch_plot,
  file.path(
    results_dir_use,
    "hourly_dispatch_validation_selected_days.csv"
  )
)

write_csv(
  balance_check,
  file.path(
    results_dir_use,
    "hourly_dispatch_validation_balance_check.csv"
  )
)

# ------------------------------------------------------------
# 12. Hourly technology-dispatch plots
# ------------------------------------------------------------
for (yr in selected_years) {

  yr_df <- dispatch_plot |>
    filter(planning_year == yr)

  if (nrow(yr_df) == 0) {
    warning("No hourly dispatch data available for year ", yr)
    next
  }

  # Demand line is identical for all technologies at a given hour.
  demand_line <- yr_df |>
    distinct(
      scenario,
      season,
      hour,
      network_heat_required_mw
    )

  p <- ggplot(
    yr_df,
    aes(
      x = hour,
      y = dispatch_mw,
      fill = technology
    )
  ) +
    geom_area(
      position = "stack",
      alpha = 0.9,
      colour = "white",
      linewidth = 0.15
    ) +
    geom_line(
      data = demand_line,
      aes(
        x = hour,
        y = network_heat_required_mw
      ),
      inherit.aes = FALSE,
      linewidth = 0.9
    ) +
    facet_grid(
      scenario ~ season
    ) +
    scale_x_continuous(
      breaks = c(1, 4, 8, 12, 16, 20, 24)
    ) +
    labs(
      x = "Hour of day",
      y = "Heat supply / required heat (MW)",
      fill = "Heat-supply technology",
      title = paste0(
        "Hourly heat supply and technology contributions, ",
        yr
      ),
      subtitle = "Dominant representative day in each season; line = heat required including 3% network losses"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  print(p)

  ggsave(
    file.path(
      plot_dir_use,
      paste0("validation_hourly_dispatch_", yr, ".png")
    ),
    p,
    width = 13,
    height = 8,
    dpi = 300
  )
}

# ------------------------------------------------------------
# 13. Hourly technology-share plots
# ------------------------------------------------------------
for (yr in selected_years) {

  share_df <- dispatch_plot |>
    filter(planning_year == yr)

  if (nrow(share_df) == 0) next

  p_share <- ggplot(
    share_df,
    aes(
      x = hour,
      y = supply_share,
      fill = technology
    )
  ) +
    geom_area(
      position = "fill",
      colour = "white",
      linewidth = 0.15
    ) +
    facet_grid(
      scenario ~ season
    ) +
    scale_x_continuous(
      breaks = c(1, 4, 8, 12, 16, 20, 24)
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1)
    ) +
    labs(
      x = "Hour of day",
      y = "Share of hourly heat supply",
      fill = "Heat-supply technology",
      title = paste0(
        "Hourly heat-supply shares, ",
        yr
      ),
      subtitle = "Technology contribution across the dominant representative-day profile in each season"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  print(p_share)

  ggsave(
    file.path(
      plot_dir_use,
      paste0("validation_hourly_supply_shares_", yr, ".png")
    ),
    p_share,
    width = 13,
    height = 8,
    dpi = 300
  )
}

# ------------------------------------------------------------
# 14. Final summary
# ------------------------------------------------------------
validation_summary <- tibble(
  item = c(
    "Representative days in model",
    "Selected representative days for illustration",
    "Selected future years",
    "Selected scenarios",
    "Maximum absolute hourly heat-balance difference (MW)"
  ),
  value = c(
    nrow(rep_days),
    nrow(selected_rep_days),
    paste(selected_years, collapse = ", "),
    paste(selected_scenarios, collapse = "; "),
    max_balance_error
  )
)

write_csv(
  validation_summary,
  file.path(
    results_dir_use,
    "hourly_dispatch_validation_summary.csv"
  )
)

message("------------------------------------------------------------")
message("Hourly dispatch validation visualization completed.")
message("Results folder: ", results_dir_use)
message("------------------------------------------------------------")
