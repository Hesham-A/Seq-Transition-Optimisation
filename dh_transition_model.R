
```{r}
# ============================================================
# District heating transition optimization model
#
# Scenario logic:
# Scenario I   = cost-oriented transition with weak CO2 reduction
# Scenario II  = moderate transition with stronger CO2 reduction
# Scenario III = strict transition reaching zero CO2 by 2050
#
# Objective:
# Least-cost system operation and investment under scenario-specific constraints.
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(cluster)
  library(purrr)
  library(tibble)
  library(stringr)
  library(ggplot2)
  library(openxlsx)
  library(ompr)
  library(ompr.roi)
  library(ROI)
  library(ROI.plugin.glpk)
  library(scales)
})

# ============================================================
# 1) USER SETTINGS
# ============================================================

input_file <- file.path("data", "Data2.xlsx")

sheet_dynamic <- "Dynamic data"
sheet_tech    <- "Technologies parameters"
sheet_config  <- "Current configuration"

base_year <- 2024
scenario_reference_year <- 2023
planning_years <- 2025:2050

results_dir <- file.path("results", "dh_transition_model_results")
plot_dir <- file.path(results_dir, "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

cluster_k_min <- 2
cluster_k_max <- 6

discount_rate <- 0.05
network_loss_rate <- 0.03

# Least-cost model. CO2 reduction is handled through scenario-specific caps.
emissions_penalty_eur_per_tco2 <- 0
electrification_reward_eur_per_mwh <- 0

# Long-duration thermal energy storage assumption.
# 168 h = one week at rated discharge power.
storage_duration_h <- 168
storage_roundtrip_eff <- 0.90

default_grid_limit_mw <- 80
default_annual_investment_budget_eur <- 250e6

use_biomass_fuel_cap <- TRUE
use_biomass_share_cap <- TRUE
use_excel_current_config <- FALSE

limited_source_heating_mwth <- 0.5
limited_source_shoulder_mwth <- 1.0
limited_source_summer_mwth <- 1.5

# Biomass sustainability is represented by biomass caps, not a hidden cost adder.
biomass_sustainability_cost_eur_per_mwh_fuel <- 0

# Keep this zero unless you have a defensible CHP electricity/heat co-production credit.
biomass_chp_coproduction_credit_eur_per_mwh_heat <- 0

# Minimum available system capacity requirement.
# Counts heat-generation capacity plus thermal-storage power capacity.
minimum_total_available_capacity_mw <- 170

scenario_colors <- c(
  "Scenario I" = "#1f77b4",
  "Scenario II" = "#ff7f0e",
  "Scenario III" = "#d62728"
)

scenario_linetypes <- c(
  "Scenario I" = "dashed",
  "Scenario II" = "dotdash",
  "Scenario III" = "dotted"
)

# ============================================================
# 2) HELPERS
# ============================================================

clean_names_simple <- function(df) {
  names(df) <- names(df) |>
    str_trim() |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("_+$", "") |>
    str_to_lower()
  df
}

read_optional_sheet <- function(path, sheet) {
  if (!(sheet %in% excel_sheets(path))) return(NULL)
  read_excel(path, sheet = sheet) |> clean_names_simple()
}

parse_source_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && median(x, na.rm = TRUE) > 10000) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  parsed <- suppressWarnings(parse_date_time(as.character(x), orders = c("dmy", "mdy", "ymd", "Ymd")))
  as.Date(parsed)
}

assign_operational_season <- function(month_value, day_value) {
  case_when(
    month_value %in% c(12, 1, 2, 3) | (month_value == 11 & day_value >= 15) ~ "Heating",
    month_value %in% c(7, 8) | (month_value == 6 & day_value >= 11) ~ "Summer",
    TRUE ~ "Shoulder"
  )
}

annuity_factor <- function(r, n) {
  if (is.na(r) || r <= 0) return(1 / n)
  r / (1 - (1 + r)^(-n))
}

interp_series <- function(anchor_years, anchor_values, years_out) {
  approx(anchor_years, anchor_values, xout = years_out, method = "linear", rule = 2)$y
}

compute_cop <- function(source_temp_c, sink_temp_c = 75, quality_factor = 0.45) {
  source_k <- source_temp_c + 273.15
  sink_k <- sink_temp_c + 273.15
  cop <- quality_factor * sink_k / pmax(5, sink_k - source_k)
  pmin(7, pmax(1.2, cop))
}

theme_pub <- function() {
  theme_minimal(base_size = 13) +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.7),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

zero_y <- function(labels = waiver()) {
  scale_y_continuous(
    labels = labels,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06))
  )
}

scalar_value <- function(x) {
  if (is.data.frame(x)) return(as.numeric(x$value[1]))
  as.numeric(x[1])
}

clean_small_values <- function(df, tol = 1e-6) {
  df |> mutate(across(where(is.numeric), ~ ifelse(abs(.x) < tol, 0, .x)))
}

# ============================================================
# 3) READ AND PREPARE HOURLY DATA
# ============================================================

if (!file.exists(input_file)) stop("Input file not found: ", input_file)

raw_dynamic <- read_optional_sheet(input_file, sheet_dynamic)
if (is.null(raw_dynamic)) stop("Dynamic sheet not found: ", sheet_dynamic)

if (!("year" %in% names(raw_dynamic)) && "date" %in% names(raw_dynamic)) {
  raw_dynamic$year <- raw_dynamic$date
}
if (!("demand_mw" %in% names(raw_dynamic))) stop("Required column missing: demand_MW")
if (!("hour" %in% names(raw_dynamic))) stop("Required column missing: hour")

add_default_col <- function(df, nm, value) {
  if (!(nm %in% names(df))) df[[nm]] <- value
  df
}

dyn <- raw_dynamic |>
  add_default_col("temp_air", 5) |>
  add_default_col("temp_ground", 8) |>
  add_default_col("temp_seawater", 8) |>
  add_default_col("temp_wastewater", 15) |>
  add_default_col("elec_price", 90) |>
  add_default_col("gas_price", 45) |>
  add_default_col("biomass_price", 35) |>
  add_default_col("carbon_price", 90) |>
  add_default_col("max_power_mw", default_grid_limit_mw) |>
  add_default_col("renewable_share", 0.45) |>
  add_default_col("carbon_intensity", 0.35) |>
  mutate(
    source_date = parse_source_date(year),
    hour = as.integer(hour),
    source_year = year(source_date),
    source_month = month(source_date),
    source_day = day(source_date),
    day_id = source_date,
    season = assign_operational_season(source_month, source_day)
  ) |>
  mutate(season = factor(season, levels = c("Heating", "Shoulder", "Summer")))

if (any(is.na(dyn$source_date))) stop("Date parsing failed.")
if (any(dyn$hour < 1 | dyn$hour > 24, na.rm = TRUE)) stop("hour must be 1...24.")

if (!("cop_air" %in% names(dyn))) dyn$cop_air <- compute_cop(dyn$temp_air)
if (!("cop_ground" %in% names(dyn))) dyn$cop_ground <- compute_cop(dyn$temp_ground)
if (!("cop_seawater" %in% names(dyn))) dyn$cop_seawater <- compute_cop(dyn$temp_seawater)
if (!("cop_wastewater" %in% names(dyn))) dyn$cop_wastewater <- compute_cop(dyn$temp_wastewater)

ci_mean <- mean(dyn$carbon_intensity, na.rm = TRUE)
dyn <- dyn |>
  mutate(carbon_intensity_t_per_mwh = ifelse(ci_mean > 5, carbon_intensity / 1000, carbon_intensity))

valid_days <- dyn |>
  count(source_date, name = "n_hours") |>
  filter(n_hours == 24)

dyn <- dyn |>
  semi_join(valid_days, by = "source_date") |>
  arrange(source_date, hour)

historical_annual <- dyn |>
  filter(source_year >= 2018, source_year <= 2023) |>
  group_by(source_year) |>
  summarise(
    heat_demand_gwh = sum(demand_mw, na.rm = TRUE) / 1000,
    electricity_price = mean(elec_price, na.rm = TRUE),
    gas_price = mean(gas_price, na.rm = TRUE),
    biomass_price = mean(biomass_price, na.rm = TRUE),
    carbon_price = mean(carbon_price, na.rm = TRUE),
    grid_limit_mw = mean(max_power_mw, na.rm = TRUE),
    renewable_share = mean(renewable_share, na.rm = TRUE),
    carbon_intensity = mean(carbon_intensity_t_per_mwh, na.rm = TRUE),
    .groups = "drop"
  )

if (nrow(historical_annual) == 0) {
  historical_annual <- dyn |>
    group_by(source_year) |>
    summarise(
      heat_demand_gwh = sum(demand_mw, na.rm = TRUE) / 1000,
      electricity_price = mean(elec_price, na.rm = TRUE),
      gas_price = mean(gas_price, na.rm = TRUE),
      biomass_price = mean(biomass_price, na.rm = TRUE),
      carbon_price = mean(carbon_price, na.rm = TRUE),
      grid_limit_mw = mean(max_power_mw, na.rm = TRUE),
      renewable_share = mean(renewable_share, na.rm = TRUE),
      carbon_intensity = mean(carbon_intensity_t_per_mwh, na.rm = TRUE),
      .groups = "drop"
    )
}

reference_row <- historical_annual |> filter(source_year == scenario_reference_year)
if (nrow(reference_row) == 0) {
  reference_row <- historical_annual |>
    filter(source_year <= scenario_reference_year) |>
    slice_max(source_year, n = 1)
}
if (nrow(reference_row) == 0) {
  reference_row <- historical_annual |> slice_max(source_year, n = 1)
}

scenario_reference_year <- reference_row$source_year[1]
reference_heat_mwh <- reference_row$heat_demand_gwh[1] * 1000
reference_emissions_tco2 <- reference_heat_mwh / 0.85 * 183 / 1000
base_network_capacity_mw <- max(dyn$demand_mw, na.rm = TRUE) * 1.10

write.csv(historical_annual, file.path(results_dir, "historical_annual_measured.csv"), row.names = FALSE)

# ============================================================
# 4) REPRESENTATIVE DAYS
# ============================================================

daily <- dyn |>
  group_by(day_id, source_year, season) |>
  summarise(
    demand_sum_mwh = sum(demand_mw, na.rm = TRUE),
    demand_peak_mw = max(demand_mw, na.rm = TRUE),
    demand_mean_mw = mean(demand_mw, na.rm = TRUE),
    temp_air_mean = mean(temp_air, na.rm = TRUE),
    temp_air_min = min(temp_air, na.rm = TRUE),
    hdh18 = sum(pmax(0, 18 - temp_air), na.rm = TRUE),
    elec_price_mean = mean(elec_price, na.rm = TRUE),
    gas_price_mean = mean(gas_price, na.rm = TRUE),
    biomass_price_mean = mean(biomass_price, na.rm = TRUE),
    carbon_price_mean = mean(carbon_price, na.rm = TRUE),
    renewable_share_mean = mean(renewable_share, na.rm = TRUE),
    carbon_intensity_mean = mean(carbon_intensity_t_per_mwh, na.rm = TRUE),
    grid_limit_mean = mean(max_power_mw, na.rm = TRUE),
    grid_limit_min = min(max_power_mw, na.rm = TRUE),
    .groups = "drop"
  )

feature_cols <- c(
  "demand_sum_mwh", "demand_peak_mw", "demand_mean_mw",
  "temp_air_mean", "temp_air_min", "hdh18",
  "elec_price_mean", "gas_price_mean", "biomass_price_mean",
  "carbon_price_mean", "renewable_share_mean", "carbon_intensity_mean",
  "grid_limit_mean", "grid_limit_min"
)

feature_sd <- sapply(daily[feature_cols], sd, na.rm = TRUE)
feature_cols <- feature_cols[!is.na(feature_sd) & feature_sd > 0]

season_days_annual <- tibble(
  season = factor(c("Heating", "Shoulder", "Summer"), levels = levels(daily$season)),
  season_days = c(137, 146, 82)
)

choose_k_pam <- function(x, k_min = 2, k_max = 6) {
  n <- nrow(x)
  if (n < 3) stop("Too few observations for clustering.")

  k_max <- min(k_max, n - 1)
  k_min <- min(k_min, k_max)
  k_grid <- k_min:k_max

  scores <- map_dfr(k_grid, function(k) {
    fit <- pam(x, k = k)
    sil <- silhouette(fit$clustering, dist(x))
    tibble(k = k, avg_silhouette = mean(sil[, 3]), fit = list(fit))
  })

  best <- scores |>
    arrange(desc(avg_silhouette), k) |>
    slice(1)

  list(best_fit = best$fit[[1]], scores = scores |> select(k, avg_silhouette))
}

cluster_one_season <- function(season_name) {
  df_s <- daily |> filter(season == season_name)
  local_sd <- sapply(df_s[feature_cols], sd, na.rm = TRUE)
  local_feature_cols <- feature_cols[!is.na(local_sd) & local_sd > 0]

  if (nrow(df_s) < 3) stop("Too few valid days for clustering in ", season_name)
  if (length(local_feature_cols) < 2) stop("Too few variable features for clustering in ", season_name)

  x <- df_s |>
    select(all_of(local_feature_cols)) |>
    scale() |>
    as.matrix()

  selected <- choose_k_pam(x, cluster_k_min, cluster_k_max)
  fit <- selected$best_fit
  cluster_sizes <- table(fit$clustering)

  rep_days_s <- df_s[fit$id.med, ] |>
    mutate(
      cluster_id = as.integer(fit$clustering[fit$id.med]),
      cluster_size_hist = as.integer(cluster_sizes[as.character(cluster_id)]),
      season_total_days = nrow(df_s),
      k_selected = length(fit$id.med),
      avg_silhouette_selected = max(selected$scores$avg_silhouette, na.rm = TRUE)
    )

  cluster_map_s <- df_s |>
    mutate(cluster_id = as.integer(fit$clustering))

  list(
    rep_days = rep_days_s,
    cluster_map = cluster_map_s,
    scores = selected$scores,
    fit = fit
  )
}

season_levels_present <- levels(droplevels(daily$season))

season_results <- season_levels_present |>
  set_names() |>
  map(cluster_one_season)

silhouette_tbl <- imap_dfr(season_results, function(res, season_name) {
  res$scores |> mutate(season = season_name)
}) |>
  mutate(season = factor(season, levels = levels(daily$season)))

rep_days <- bind_rows(map(season_results, "rep_days")) |>
  arrange(season, cluster_id) |>
  mutate(rep_id = row_number()) |>
  left_join(daily |> count(season, name = "season_days_hist"), by = "season") |>
  left_join(season_days_annual, by = "season") |>
  mutate(
    weight_days = cluster_size_hist / season_days_hist * season_days,
    weight_hours = weight_days * 24
  )

cluster_map <- bind_rows(map(season_results, "cluster_map"))

message("Total weighted representative days = ", round(sum(rep_days$weight_days), 2))

rep_hourly <- dyn |>
  filter(source_date %in% rep_days$day_id) |>
  left_join(
    rep_days |> select(day_id, rep_id, cluster_id, weight_days, weight_hours),
    by = c("source_date" = "day_id")
  ) |>
  arrange(rep_id, hour)

write.csv(daily, file.path(results_dir, "daily_features.csv"), row.names = FALSE)
write.csv(rep_days, file.path(results_dir, "representative_days.csv"), row.names = FALSE)
write.csv(cluster_map, file.path(results_dir, "cluster_map.csv"), row.names = FALSE)
write.csv(silhouette_tbl, file.path(results_dir, "silhouette_scores.csv"), row.names = FALSE)
write.csv(
  rep_hourly,
  file.path(results_dir, "representative_hourly_profiles.csv"),
  row.names = FALSE
)

p_silhouette <- silhouette_tbl |>
  ggplot(aes(k, avg_silhouette, color = season)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.3) +
  scale_x_continuous(breaks = sort(unique(silhouette_tbl$k))) +
  zero_y() +
  labs(x = "Number of clusters (k)", y = "Average silhouette width", color = "Season") +
  theme_pub()

ggsave(file.path(plot_dir, "clustering_01_silhouette_scores.png"),
       p_silhouette, width = 8.5, height = 5.5, dpi = 300)

x_all <- daily |>
  select(all_of(feature_cols)) |>
  scale() |>
  as.matrix()

pca <- prcomp(x_all, center = FALSE, scale. = FALSE)

pca_df <- daily |>
  mutate(PC1 = pca$x[, 1], PC2 = pca$x[, 2])

rep_pca <- pca_df |>
  inner_join(rep_days |> select(day_id, rep_id, season, cluster_id),
             by = c("day_id", "season"))

p_pca <- ggplot(pca_df, aes(PC1, PC2, color = season)) +
  geom_point(alpha = 0.35, size = 1.3) +
  geom_point(
    data = rep_pca,
    aes(PC1, PC2),
    shape = 23,
    fill = "yellow",
    color = "black",
    size = 3.0,
    stroke = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    data = rep_pca,
    aes(PC1, PC2, label = rep_id),
    color = "black",
    size = 3,
    vjust = -1.1,
    show.legend = FALSE
  ) +
  labs(x = "Principal component 1", y = "Principal component 2", color = "Season") +
  theme_pub()

ggsave(file.path(plot_dir, "clustering_02_pca_medoids.png"),
       p_pca, width = 8.5, height = 6, dpi = 300)

p_cluster_weights <- rep_days |>
  mutate(rep_label = paste0("R", rep_id)) |>
  ggplot(aes(rep_label, weight_days, fill = season)) +
  geom_col(color = "black", width = 0.7) +
  facet_wrap(~ season, scales = "free_x") +
  zero_y() +
  labs(x = "Representative day", y = "Weight (days)", fill = "Season") +
  theme_pub()

ggsave(file.path(plot_dir, "clustering_03_representative_day_weights.png"),
       p_cluster_weights, width = 10, height = 5.8, dpi = 300)

# ============================================================
# 5) TECHNOLOGY DATA
# ============================================================

tech_params <- tribble(
  ~technology, ~fuel_type, ~capex_eur_per_mw, ~fixed_opex_eur_per_mw_yr, ~var_opex_eur_per_mwh, ~efficiency, ~lifetime_yr, ~co2_kg_per_mwh_fuel, ~min_load, ~max_load, ~available_from, ~available_until, ~max_total_capacity_mw, ~max_annual_invest_mw, ~source_col,
  "fossil_CHP",      "gas",         900000, 20000, 4.0, 0.85, 25, 183, 0.30, 1.00, 2025, 2050, 250, 40, NA_character_,
  "fossil_boiler",   "gas",         300000,  6000, 2.0, 0.92, 25, 183, 0.20, 1.00, 2025, 2050, 250, 40, NA_character_,
  "Biomass_CHP",     "biomass",    3500000, 35000, 6.0, 0.80, 30,   0, 0.30, 1.00, 2025, 2050, 120, 20, NA_character_,
  "Biomass_boiler",  "biomass",    1200000, 18000, 4.5, 0.90, 30,   0, 0.20, 1.00, 2025, 2050, 120, 20, NA_character_,
  "Electric_Boiler", "electricity", 180000,  3000, 0.8, NA,   20,   0, 0.00, 1.00, 2025, 2050, 250, 60, NA_character_,
  "Air_HP",          "electricity",1100000, 14000, 1.2, NA,   20,   0, 0.00, 1.00, 2025, 2050, 180, 35, "cop_air",
  "Ground_HP",       "electricity",1400000, 15000, 1.2, NA,   25,   0, 0.00, 1.00, 2025, 2050,  70, 15, "cop_ground",
  "WasteWater_HP",   "electricity",1150000, 13000, 1.0, NA,   20,   0, 0.00, 1.00, 2025, 2050,  20, 10, "cop_wastewater",
  "SeaWater_HP",     "electricity",1450000, 15000, 1.2, NA,   20,   0, 0.00, 1.00, 2028, 2050,  80, 20, "cop_seawater",
  "ExcessHeat_HP",   "electricity", 650000, 10000, 0.8, NA,   20,   0, 0.00, 1.00, 2025, 2050,  10,  5, NA_character_,
  "ThermalStorage",  "storage",     220000,  1500, 0.3, 0.90, 30,   0, 0.00, 1.00, 2025, 2050, 150, 30, NA_character_
)

user_tech <- read_optional_sheet(input_file, sheet_tech)
if (!is.null(user_tech) && "technology" %in% names(user_tech)) {
  user_tech$technology <- as.character(user_tech$technology)
  provided_cols <- intersect(names(user_tech), names(tech_params))

  for (i in seq_len(nrow(user_tech))) {
    tn <- user_tech$technology[i]
    if (!(tn %in% tech_params$technology)) next

    row_id <- which(tech_params$technology == tn)
    for (nm in setdiff(provided_cols, "technology")) {
      val <- user_tech[[nm]][i]
      if (!is.na(val)) {
        if (is.numeric(tech_params[[nm]])) val <- as.numeric(val)
        if (is.character(tech_params[[nm]])) val <- as.character(val)
        tech_params[[nm]][row_id] <- val
      }
    }
  }

  duration_cols <- intersect(
    names(user_tech),
    c("storage_duration_h", "duration_h", "storage_hours", "energy_to_power_h")
  )

  if (length(duration_cols) > 0) {
    storage_row <- user_tech |> filter(technology == "ThermalStorage") |> slice(1)
    if (nrow(storage_row) == 1) {
      duration_value <- as.numeric(storage_row[[duration_cols[1]]])
      if (!is.na(duration_value) && duration_value > 0) {
        storage_duration_h <- duration_value
      }
    }
  }
}

gen_params <- tech_params |> filter(fuel_type != "storage")
storage_params <- tech_params |> filter(technology == "ThermalStorage") |> slice(1)

storage_roundtrip_eff_used <- ifelse(
  is.na(storage_params$efficiency),
  storage_roundtrip_eff,
  storage_params$efficiency
)

if (is.na(storage_duration_h) || storage_duration_h <= 0) {
  stop("storage_duration_h must be a positive number.")
}
if (is.na(storage_roundtrip_eff_used) || storage_roundtrip_eff_used <= 0 || storage_roundtrip_eff_used > 1) {
  stop("Storage round-trip efficiency must be between 0 and 1.")
}

if (use_excel_current_config) {
  config_data <- read_optional_sheet(input_file, sheet_config)
  if (is.null(config_data) || !("technology" %in% names(config_data))) {
    stop("Excel current configuration requested, but sheet/technology column is missing.")
  }
  current_config <- config_data
  if (!("capacity_mw" %in% names(current_config))) stop("Current configuration must contain capacity_MW.")
  if (!("age_years" %in% names(current_config))) current_config$age_years <- 0
  if (!("efficiency" %in% names(current_config))) current_config$efficiency <- NA_real_
} else {
  current_config <- tibble(
    technology = c("fossil_CHP", "Biomass_boiler"),
    capacity_mw = c(130.00, 40.00),
    age_years = c(15.00, 10.00),
    efficiency = c(0.85, 0.90)
  )
}

current_config <- current_config |>
  mutate(
    technology = as.character(technology),
    capacity_mw = as.numeric(capacity_mw),
    age_years = as.numeric(age_years),
    efficiency = as.numeric(efficiency)
  ) |>
  filter(technology %in% tech_params$technology)

capacity_blocks_initial <- current_config |>
  left_join(tech_params |> select(technology, lifetime_yr), by = "technology") |>
  mutate(
    install_year = base_year - age_years,
    lifetime_yr = ifelse(is.na(lifetime_yr), 25, lifetime_yr)
  ) |>
  select(technology, install_year, capacity_mw, lifetime_yr)

# ============================================================
# 6) SCENARIOS
# ============================================================

make_scenario_table <- function(years) {
  anchors <- list(
  "Scenario I" = tibble(
    year = c(2023, 2025, 2030, 2040, 2050),
    demand_mult = c(1.00, 1.00, 0.99, 0.97, 0.95),
    elec_price_mult = c(1.00, 1.00, 1.35, 2.00, 2.70),
    gas_price_mult = c(1.00, 1.00, 1.18, 1.45, 1.90),
    biomass_price_mult = c(1.00, 1.00, 1.10, 1.30, 1.55),
    carbon_price_mult = c(1.00, 1.05, 1.15, 1.35, 1.60),
    grid_mult = c(1.00, 1.00, 1.01, 1.02, 1.03),
    ci_mult = c(1.00, 0.95, 0.88, 0.78, 0.68),
    ren_share_mult = c(1.00, 1.02, 1.06, 1.12, 1.20),
    cop_mult = c(1.00, 1.01, 1.02, 1.04, 1.06),
    capex_mult = c(1.00, 1.00, 0.99, 0.98, 0.97),
    emission_cap_mult = c(1.00, 0.99, 0.95, 0.90, 0.85),
    biomass_fuel_cap_share = c(0.35, 0.35, 0.34, 0.33, 0.32),
    biomass_heat_share_cap = c(0.38, 0.38, 0.37, 0.36, 0.35),
    enforce_emission_cap = TRUE,
    allow_new_fossil = TRUE
  ),

  "Scenario II" = tibble(
    year = c(2023, 2025, 2030, 2040, 2050),
    demand_mult = c(1.00, 1.00, 0.95, 0.82, 0.77),
    elec_price_mult = c(1.00, 1.00, 1.08, 1.28, 1.60),
    gas_price_mult = c(1.00, 1.00, 1.25, 1.70, 2.40),
    biomass_price_mult = c(1.00, 1.00, 1.18, 1.45, 1.80),
    carbon_price_mult = c(1.00, 1.15, 1.60, 2.60, 3.80),
    grid_mult = c(1.00, 1.00, 1.04, 1.10, 1.18),
    ci_mult = c(1.00, 0.92, 0.78, 0.50, 0.25),
    ren_share_mult = c(1.00, 1.05, 1.15, 1.40, 1.65),
    cop_mult = c(1.00, 1.02, 1.05, 1.08, 1.12),
    capex_mult = c(1.00, 1.00, 0.99, 0.96, 0.93),
    emission_cap_mult = c(1.00, 0.95, 0.80, 0.50, 0.20),
    biomass_fuel_cap_share = c(0.32, 0.32, 0.29, 0.24, 0.18),
    biomass_heat_share_cap = c(0.34, 0.34, 0.30, 0.24, 0.18),
    enforce_emission_cap = TRUE,
    allow_new_fossil = TRUE
  ),

  "Scenario III" = tibble(
    year = c(2023, 2025, 2030, 2040, 2050),
    demand_mult = c(1.00, 1.00, 0.95, 0.82, 0.77),
    elec_price_mult = c(1.00, 1.00, 1.05, 1.22, 1.45),
    gas_price_mult = c(1.00, 1.00, 1.25, 1.75, 2.45),
    biomass_price_mult = c(1.00, 1.00, 1.18, 1.45, 1.78),
    carbon_price_mult = c(1.00, 1.20, 1.80, 3.40, 5.20),
    grid_mult = c(1.00, 1.02, 1.08, 1.20, 1.32),
    ci_mult = c(1.00, 0.90, 0.68, 0.32, 0.00),
    ren_share_mult = c(1.00, 1.06, 1.20, 1.50, 1.85),
    cop_mult = c(1.00, 1.03, 1.06, 1.10, 1.14),
    capex_mult = c(1.00, 0.99, 0.97, 0.94, 0.90),
    emission_cap_mult = c(1.00, 0.90, 0.65, 0.25, 0.00),
    biomass_fuel_cap_share = c(0.30, 0.30, 0.25, 0.16, 0.08),
    biomass_heat_share_cap = c(0.32, 0.32, 0.26, 0.16, 0.08),
    enforce_emission_cap = TRUE,
    allow_new_fossil = TRUE
  )
)

  bind_rows(lapply(names(anchors), function(scn) {
    a <- anchors[[scn]]
    tibble(
      scenario = scn,
      planning_year = years,
      demand_mult = interp_series(a$year, a$demand_mult, years),
      elec_price_mult = interp_series(a$year, a$elec_price_mult, years),
      gas_price_mult = interp_series(a$year, a$gas_price_mult, years),
      biomass_price_mult = interp_series(a$year, a$biomass_price_mult, years),
      carbon_price_mult = interp_series(a$year, a$carbon_price_mult, years),
      grid_mult = interp_series(a$year, a$grid_mult, years),
      ci_mult = interp_series(a$year, a$ci_mult, years),
      ren_share_mult = interp_series(a$year, a$ren_share_mult, years),
      cop_mult = interp_series(a$year, a$cop_mult, years),
      capex_mult = interp_series(a$year, a$capex_mult, years),
      emission_cap_tco2 = reference_emissions_tco2 * interp_series(a$year, a$emission_cap_mult, years),
      biomass_fuel_cap_mwh = reference_heat_mwh * interp_series(a$year, a$biomass_fuel_cap_share, years),
      biomass_heat_share_cap = interp_series(a$year, a$biomass_heat_share_cap, years),
      enforce_emission_cap = as.logical(a$enforce_emission_cap[1]),
      allow_new_fossil = as.logical(a$allow_new_fossil[1]),
      investment_budget_eur = default_annual_investment_budget_eur
    )
  }))
}

scenario_table <- make_scenario_table(planning_years)
scenario_plot_table <- make_scenario_table(scenario_reference_year:2050)

write.csv(scenario_table, file.path(results_dir, "scenario_table.csv"), row.names = FALSE)

scenario_plot_values <- scenario_plot_table |>
  mutate(
    year = planning_year,
    heat_demand_gwh = reference_row$heat_demand_gwh[1] * demand_mult,
    electricity_price = reference_row$electricity_price[1] * elec_price_mult,
    gas_price = reference_row$gas_price[1] * gas_price_mult,
    biomass_price = reference_row$biomass_price[1] * biomass_price_mult,
    carbon_price = reference_row$carbon_price[1] * carbon_price_mult,
    grid_limit_mw = reference_row$grid_limit_mw[1] * grid_mult,
    renewable_share = pmin(1, reference_row$renewable_share[1] * ren_share_mult),
    carbon_intensity = reference_row$carbon_intensity[1] * ci_mult
  )

write.csv(scenario_plot_values, file.path(results_dir, "scenario_plot_values.csv"), row.names = FALSE)

make_scenario_plot <- function(var, ylab, outfile) {
  hist_df <- historical_annual |>
    transmute(year = source_year, value = .data[[var]])

  last_hist_year <- max(hist_df$year, na.rm = TRUE)

  anchor <- hist_df |>
    filter(year == last_hist_year) |>
    select(year, value) |>
    crossing(scenario = names(scenario_colors))

  scen_df <- scenario_plot_values |>
    transmute(year, scenario, value = .data[[var]]) |>
    filter(year > last_hist_year) |>
    bind_rows(anchor) |>
    arrange(scenario, year)

  p <- ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.55) +
    geom_line(data = hist_df, aes(year, value), color = "black", linewidth = 1.1) +
    geom_point(data = hist_df, aes(year, value), color = "black", size = 2.0) +
    geom_line(data = scen_df, aes(year, value, color = scenario, linetype = scenario), linewidth = 1.1) +
    geom_point(
      data = scen_df |> filter(year %in% c(last_hist_year, 2025, 2030, 2040, 2050)),
      aes(year, value, color = scenario),
      size = 1.8,
      show.legend = FALSE
    ) +
    geom_point(
      data = scen_df |> filter(abs(value) < 1e-9),
      aes(year, value),
      shape = 21,
      fill = "white",
      color = "black",
      size = 3,
      stroke = 1,
      show.legend = FALSE
    ) +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +
    scale_x_continuous(breaks = seq(2018, 2050, by = 4)) +
    zero_y() +
    coord_cartesian(clip = "off") +
    labs(x = "Year", y = ylab, color = "Scenario", linetype = "Scenario") +
    theme_pub()

  ggsave(file.path(plot_dir, outfile), p, width = 9, height = 5.5, dpi = 300)
  p
}

scenario_plots <- list(
  make_scenario_plot("heat_demand_gwh", "Heat demand (GWh)", "scenario_01_heat_demand.png"),
  make_scenario_plot("electricity_price", "Electricity price (EUR/MWh)", "scenario_02_electricity_price.png"),
  make_scenario_plot("gas_price", "Gas price (EUR/MWh)", "scenario_03_gas_price.png"),
  make_scenario_plot("biomass_price", "Biomass price (EUR/MWh)", "scenario_04_biomass_price.png"),
  make_scenario_plot("carbon_price", "Carbon price (EUR/tCO2)", "scenario_05_carbon_price.png"),
  make_scenario_plot("grid_limit_mw", "Available electricity capacity (MW)", "scenario_06_grid_limit.png"),
  make_scenario_plot("renewable_share", "Renewable share (-)", "scenario_07_renewable_share.png"),
  make_scenario_plot("carbon_intensity", "Electricity carbon intensity (tCO2/MWh)", "scenario_08_carbon_intensity.png")
)

scenario_panel_data <- scenario_plot_values |>
  select(
    year, scenario,
    heat_demand_gwh,
    electricity_price,
    gas_price,
    biomass_price,
    carbon_price,
    grid_limit_mw,
    renewable_share,
    carbon_intensity
  ) |>
  pivot_longer(-c(year, scenario), names_to = "variable", values_to = "value")

historical_panel_data <- historical_annual |>
  rename(year = source_year) |>
  select(
    year,
    heat_demand_gwh,
    electricity_price,
    gas_price,
    biomass_price,
    carbon_price,
    grid_limit_mw,
    renewable_share,
    carbon_intensity
  ) |>
  pivot_longer(-year, names_to = "variable", values_to = "value")

last_hist_year <- max(historical_panel_data$year, na.rm = TRUE)

scenario_anchor <- historical_panel_data |>
  filter(year == last_hist_year) |>
  select(year, variable, value) |>
  crossing(scenario = names(scenario_colors))

scenario_panel_data_connected <- scenario_panel_data |>
  filter(year > last_hist_year) |>
  bind_rows(scenario_anchor) |>
  arrange(variable, scenario, year)

p_scenario_panel <- ggplot() +
  geom_hline(
    data = historical_panel_data |> distinct(variable),
    aes(yintercept = 0),
    color = "black",
    linewidth = 0.55
  ) +
  geom_line(data = historical_panel_data, aes(year, value), color = "black", linewidth = 0.9) +
  geom_point(data = historical_panel_data, aes(year, value), color = "black", size = 1.4) +
  geom_line(
    data = scenario_panel_data_connected,
    aes(year, value, color = scenario, linetype = scenario),
    linewidth = 0.9
  ) +
  geom_point(
    data = scenario_panel_data_connected |>
      filter(year %in% c(last_hist_year, 2025, 2030, 2040, 2050)),
    aes(year, value, color = scenario),
    size = 1.4,
    show.legend = FALSE
  ) +
  geom_point(
    data = scenario_panel_data_connected |> filter(abs(value) < 1e-9),
    aes(year, value),
    shape = 21,
    fill = "white",
    color = "black",
    size = 2.4,
    stroke = 0.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = scenario_panel_data_connected |> filter(abs(value) < 1e-9, year == 2050),
    aes(year, value, label = "0"),
    color = "black",
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  scale_x_continuous(breaks = seq(2018, 2050, by = 8)) +
  zero_y() +
  coord_cartesian(clip = "off") +
  labs(x = "Year", y = NULL, color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "scenario_09_all_pathways_panel.png"),
       p_scenario_panel, width = 12, height = 9, dpi = 300)

# ============================================================
# 7) ANNUAL LP SOLVER
# ============================================================

solve_one_year <- function(year_value, scenario_row, capacity_blocks, scenario_name) {

  active_blocks <- capacity_blocks |>
    filter(year_value < install_year + lifetime_yr, capacity_mw > 1e-8)

  G <- nrow(gen_params)
  D <- length(unique(rep_hourly$rep_id))
  H <- 24

  tech_names <- gen_params$technology
  electric_ids <- which(gen_params$fuel_type == "electricity")
  biomass_ids <- which(gen_params$fuel_type == "biomass")

  active_capacity <- sapply(tech_names, function(tn) {
    sum(active_blocks$capacity_mw[active_blocks$technology == tn], na.rm = TRUE)
  })
  active_capacity[is.na(active_capacity)] <- 0

  active_storage_mw <- sum(active_blocks$capacity_mw[active_blocks$technology == "ThermalStorage"], na.rm = TRUE)

  make_mat <- function(col) matrix(rep_hourly[[col]], nrow = D, ncol = H, byrow = TRUE)

  demand_mat <- make_mat("demand_mw") * scenario_row$demand_mult
  elec_price_mat <- make_mat("elec_price") * scenario_row$elec_price_mult
  gas_price_mat <- make_mat("gas_price") * scenario_row$gas_price_mult
  biomass_price_mat <- make_mat("biomass_price") * scenario_row$biomass_price_mult
  carbon_price_mat <- make_mat("carbon_price") * scenario_row$carbon_price_mult
  grid_limit_mat <- make_mat("max_power_mw") * scenario_row$grid_mult
  ci_mat <- make_mat("carbon_intensity_t_per_mwh") * scenario_row$ci_mult

  season_mat <- matrix(as.character(rep_hourly$season), nrow = D, ncol = H, byrow = TRUE)
  limited_source_cap_mat <- ifelse(
    season_mat == "Heating", limited_source_heating_mwth,
    ifelse(season_mat == "Summer", limited_source_summer_mwth, limited_source_shoulder_mwth)
  )

  weight_vec <- rep_hourly |>
    distinct(rep_id, weight_days) |>
    arrange(rep_id) |>
    pull(weight_days)

  annual_heat_required_mwh <- sum(sapply(seq_len(D), function(d) {
    sum(demand_mat[d, ]) * weight_vec[d]
  }))

  cop_arr <- array(1, dim = c(G, D, H))
  source_cap_arr <- array(Inf, dim = c(G, D, H))

  for (g in seq_len(G)) {
    tn <- tech_names[g]
    source_col <- gen_params$source_col[g]

    if (tn == "Electric_Boiler") {
      base_cop <- matrix(1, nrow = D, ncol = H)
    } else if (tn == "ExcessHeat_HP") {
      base_cop <- matrix(4, nrow = D, ncol = H)
    } else if (!is.na(source_col) && source_col %in% names(rep_hourly)) {
      base_cop <- make_mat(source_col)
    } else {
      base_cop <- matrix(1, nrow = D, ncol = H)
    }

    cop_arr[g, , ] <- pmin(8, pmax(1, base_cop * scenario_row$cop_mult))

    if (tn %in% c("WasteWater_HP", "ExcessHeat_HP")) {
      source_cap_arr[g, , ] <- limited_source_cap_mat
    }
  }

  capex <- gen_params$capex_eur_per_mw * scenario_row$capex_mult
  fixed_om <- gen_params$fixed_opex_eur_per_mw_yr
  var_om <- gen_params$var_opex_eur_per_mwh
  eff <- gen_params$efficiency
  life <- gen_params$lifetime_yr
  co2_kg <- gen_params$co2_kg_per_mwh_fuel
  max_load <- gen_params$max_load
  max_total_cap <- gen_params$max_total_capacity_mw
  max_annual_inv <- gen_params$max_annual_invest_mw
  ann <- sapply(life, function(n) annuity_factor(discount_rate, n))

  storage_capex <- storage_params$capex_eur_per_mw * scenario_row$capex_mult
  storage_fixed <- storage_params$fixed_opex_eur_per_mw_yr
  storage_var <- storage_params$var_opex_eur_per_mwh
  storage_life <- storage_params$lifetime_yr
  storage_ann <- annuity_factor(discount_rate, storage_life)
  eta_ch <- sqrt(storage_roundtrip_eff_used)
  eta_dis <- sqrt(storage_roundtrip_eff_used)

  heat_cost <- elec_cost <- heat_emis <- elec_emis <- heat_obj <- elec_obj <- array(0, dim = c(G, D, H))

  for (g in seq_len(G)) {
    for (d in seq_len(D)) {
      for (h in seq_len(H)) {
        fuel <- gen_params$fuel_type[g]

        if (fuel == "gas") {
          heat_emis[g, d, h] <- co2_kg[g] / 1000 / eff[g]
          heat_cost[g, d, h] <- var_om[g] +
            gas_price_mat[d, h] / eff[g] +
            carbon_price_mat[d, h] * heat_emis[g, d, h]

        } else if (fuel == "biomass") {
          chp_credit <- ifelse(
            tech_names[g] == "Biomass_CHP",
            biomass_chp_coproduction_credit_eur_per_mwh_heat,
            0
          )

          heat_emis[g, d, h] <- co2_kg[g] / 1000 / eff[g]
          heat_cost[g, d, h] <- max(
            0,
            var_om[g] +
              biomass_price_mat[d, h] / eff[g] +
              biomass_sustainability_cost_eur_per_mwh_fuel / eff[g] -
              chp_credit
          )

        } else if (fuel == "electricity") {
          heat_cost[g, d, h] <- var_om[g]
          elec_cost[g, d, h] <- elec_price_mat[d, h]
          elec_emis[g, d, h] <- ci_mat[d, h]
        }

        heat_obj[g, d, h] <- weight_vec[d] *
          (heat_cost[g, d, h] + emissions_penalty_eur_per_tco2 * heat_emis[g, d, h])

        elec_obj[g, d, h] <- weight_vec[d] *
          (elec_cost[g, d, h] + emissions_penalty_eur_per_tco2 * elec_emis[g, d, h])

        if (fuel == "electricity") {
          heat_obj[g, d, h] <- heat_obj[g, d, h] -
            weight_vec[d] * electrification_reward_eur_per_mwh
        }
      }
    }
  }

  idx_gdh <- function(g, d, h) as.integer(g + (d - 1L) * G + (h - 1L) * G * D)

  heat_cost_v <- as.vector(heat_cost)
  elec_cost_v <- as.vector(elec_cost)
  heat_emis_v <- as.vector(heat_emis)
  elec_emis_v <- as.vector(elec_emis)
  heat_obj_v <- as.vector(heat_obj)
  elec_obj_v <- as.vector(elec_obj)
  cop_v <- as.vector(cop_arr)
  source_cap_v <- as.vector(source_cap_arr)

  model <- MIPModel() |>
    add_variable(invest[g], g = 1:G, lb = 0) |>
    add_variable(cap[g], g = 1:G, lb = 0) |>
    add_variable(heat[g, d, h], g = 1:G, d = 1:D, h = 1:H, lb = 0) |>
    add_variable(elec[g, d, h], g = 1:G, d = 1:D, h = 1:H, lb = 0) |>
    add_variable(storage_invest, lb = 0) |>
    add_variable(storage_cap, lb = 0) |>
    add_variable(ch[d, h], d = 1:D, h = 1:H, lb = 0) |>
    add_variable(dis[d, h], d = 1:D, h = 1:H, lb = 0) |>
    add_variable(soc[d, h], d = 1:D, h = 1:H, lb = 0)

  for (g in seq_len(G)) {
    model <- model |>
      add_constraint(cap[g] == active_capacity[g] + invest[g]) |>
      add_constraint(invest[g] <= max_annual_inv[g]) |>
      add_constraint(cap[g] <= max_total_cap[g])

    if (year_value < gen_params$available_from[g] || year_value > gen_params$available_until[g]) {
      model <- model |> add_constraint(invest[g] == 0)
    }

    if (gen_params$fuel_type[g] == "gas" && !isTRUE(scenario_row$allow_new_fossil[[1]])) {
      model <- model |> add_constraint(invest[g] == 0)
    }
  }

  model <- model |>
    add_constraint(storage_cap == active_storage_mw + storage_invest) |>
    add_constraint(storage_cap <= storage_params$max_total_capacity_mw) |>
    add_constraint(storage_invest <= storage_params$max_annual_invest_mw) |>
    add_constraint(
      sum_expr(capex[g] * invest[g], g = 1:G) +
        storage_capex * storage_invest <= scenario_row$investment_budget_eur
    )
    model <- model |>
    add_constraint(storage_cap == active_storage_mw + storage_invest) |>
    add_constraint(storage_cap <= storage_params$max_total_capacity_mw) |>
    add_constraint(storage_invest <= storage_params$max_annual_invest_mw) |>
    add_constraint(
      sum_expr(capex[g] * invest[g], g = 1:G) +
        storage_capex * storage_invest <= scenario_row$investment_budget_eur
    )
      # Hard adequacy constraint:
  # generation capacity plus thermal-storage power capacity must be at least 170 MW.
  model <- model |>
    add_constraint(
      sum_expr(cap[g], g = 1:G) + storage_cap >= minimum_total_available_capacity_mw
    )

  for (g in seq_len(G)) {
    for (d in seq_len(D)) {
      for (h in seq_len(H)) {
        model <- model |>
          add_constraint(heat[g, d, h] <= max_load[g] * cap[g])

        if (gen_params$fuel_type[g] == "electricity") {
          model <- model |>
            add_constraint(heat[g, d, h] == cop_v[idx_gdh(g, d, h)] * elec[g, d, h])
        } else {
          model <- model |>
            add_constraint(elec[g, d, h] == 0)
        }

        if (tech_names[g] %in% c("WasteWater_HP", "ExcessHeat_HP")) {
          model <- model |>
            add_constraint(heat[g, d, h] - elec[g, d, h] <= source_cap_v[idx_gdh(g, d, h)])
        }
      }
    }
  }

  for (d in seq_len(D)) {
    for (h in seq_len(H)) {
      model <- model |>
        add_constraint(
          sum_expr(heat[g, d, h], g = 1:G) + dis[d, h] ==
            demand_mat[d, h] * (1 + network_loss_rate) + ch[d, h]
        ) |>
        add_constraint(
          sum_expr(elec[g, d, h], g = electric_ids) <= grid_limit_mat[d, h]
        ) |>
        add_constraint(
          demand_mat[d, h] * (1 + network_loss_rate) <= base_network_capacity_mw
        ) |>
        add_constraint(ch[d, h] <= storage_cap) |>
        add_constraint(dis[d, h] <= storage_cap) |>
        add_constraint(soc[d, h] <= storage_cap * storage_duration_h)
    }

    model <- model |>
      add_constraint(soc[d, 1] == soc[d, H] + eta_ch * ch[d, 1] - dis[d, 1] / eta_dis)

    for (h in 2:H) {
      model <- model |>
        add_constraint(soc[d, h] == soc[d, h - 1] + eta_ch * ch[d, h] - dis[d, h] / eta_dis)
    }
  }

  if (isTRUE(scenario_row$enforce_emission_cap[[1]])) {
    model <- model |>
      add_constraint(
        sum_expr(
          heat_emis_v[idx_gdh(g, d, h)] * heat[g, d, h] * weight_vec[d] +
            elec_emis_v[idx_gdh(g, d, h)] * elec[g, d, h] * weight_vec[d],
          g = 1:G, d = 1:D, h = 1:H
        ) <= scenario_row$emission_cap_tco2
      )
  }

  if (use_biomass_fuel_cap) {
    model <- model |>
      add_constraint(
        sum_expr(heat[g, d, h] / eff[g] * weight_vec[d],
                 g = biomass_ids, d = 1:D, h = 1:H) <= scenario_row$biomass_fuel_cap_mwh
      )
  }

  if (use_biomass_share_cap) {
    model <- model |>
      add_constraint(
        sum_expr(heat[g, d, h] * weight_vec[d],
                 g = biomass_ids, d = 1:D, h = 1:H) <=
          scenario_row$biomass_heat_share_cap * annual_heat_required_mwh
      )
  }

  model <- model |>
    set_objective(
      sum_expr(ann[g] * capex[g] * invest[g], g = 1:G) +
        sum_expr(fixed_om[g] * cap[g], g = 1:G) +
        storage_ann * storage_capex * storage_invest +
        storage_fixed * storage_cap +
        sum_expr(heat_obj_v[idx_gdh(g, d, h)] * heat[g, d, h],
                 g = 1:G, d = 1:D, h = 1:H) +
        sum_expr(elec_obj_v[idx_gdh(g, d, h)] * elec[g, d, h],
                 g = 1:G, d = 1:D, h = 1:H) +
        sum_expr(storage_var * weight_vec[d] * (ch[d, h] + dis[d, h]),
                 d = 1:D, h = 1:H),
      "min"
    )

  result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE, presolve = TRUE))

  status <- tryCatch(solver_status(result), error = function(e) NA_character_)
  if (!is.na(status) && !(status %in% c("optimal", "success"))) {
    stop("Optimization failed for ", scenario_name, " in ", year_value, ". Solver status: ", status)
  }

  cap_sol <- get_solution(result, cap[g]) |>
    mutate(technology = tech_names[g], scenario = scenario_name, planning_year = year_value) |>
    rename(available_capacity_mw = value)

  invest_sol <- get_solution(result, invest[g]) |>
    mutate(technology = tech_names[g], scenario = scenario_name, planning_year = year_value) |>
    rename(new_capacity_mw = value)

  heat_sol <- get_solution(result, heat[g, d, h]) |>
    mutate(
      technology = tech_names[g],
      scenario = scenario_name,
      planning_year = year_value,
      weight_days = weight_vec[d],
      coef_idx = idx_gdh(g, d, h),
      dispatch_mw = value,
      heat_mwh = dispatch_mw * weight_days,
      fuel_varom_direct_carbon_cost_eur = dispatch_mw * weight_days * heat_cost_v[coef_idx],
      direct_emissions_tco2 = dispatch_mw * weight_days * heat_emis_v[coef_idx]
    )

  # Preserve the raw hourly dispatch by representative day for later hourly analysis.
  # This is not used by the optimization; it is only an output for validation/visualization.
  heat_hourly_sol <- heat_sol |>
    mutate(
      rep_id = as.integer(d),
      hour = as.integer(h)
    ) |>
    select(scenario, planning_year, rep_id, d, hour, technology, dispatch_mw)

  elec_sol <- get_solution(result, elec[g, d, h]) |>
    mutate(
      technology = tech_names[g],
      scenario = scenario_name,
      planning_year = year_value,
      weight_days = weight_vec[d],
      coef_idx = idx_gdh(g, d, h),
      electricity_mw = value,
      electricity_mwh = electricity_mw * weight_days,
      electricity_purchase_cost_eur = electricity_mw * weight_days * elec_cost_v[coef_idx],
      electricity_emissions_tco2 = electricity_mw * weight_days * elec_emis_v[coef_idx]
    )

  storage_sol <- tibble(d = rep(1:D, each = H), h = rep(1:H, times = D)) |>
    left_join(get_solution(result, ch[d, h]) |> rename(charge_mw = value), by = c("d", "h")) |>
    left_join(get_solution(result, dis[d, h]) |> rename(discharge_mw = value), by = c("d", "h")) |>
    left_join(get_solution(result, soc[d, h]) |> rename(soc_mwh = value), by = c("d", "h")) |>
    mutate(
      scenario = scenario_name,
      planning_year = year_value,
      weight_days = weight_vec[d],
      storage_throughput_mwh = (charge_mw + discharge_mw) * weight_days,
      storage_cycling_cost_eur = storage_var * storage_throughput_mwh
    )

  storage_cap_value <- scalar_value(get_solution(result, storage_cap))
  storage_invest_value <- scalar_value(get_solution(result, storage_invest))
    total_available_capacity_value <- sum(cap_sol$available_capacity_mw, na.rm = TRUE) +
    storage_cap_value

  if (total_available_capacity_value < minimum_total_available_capacity_mw - 1e-5) {
    stop(
      "Capacity adequacy constraint failed in ",
      scenario_name, " ", year_value,
      ": total available capacity = ",
      round(total_available_capacity_value, 3),
      " MW."
    )
  }

  grid_lookup <- tibble(
    d = rep(1:D, each = H),
    h = rep(1:H, times = D),
    weight_days = rep(weight_vec, each = H),
    grid_capacity_mw = as.vector(t(grid_limit_mat))
  )

  grid_utilization <- elec_sol |>
    group_by(scenario, planning_year, d, h) |>
    summarise(electric_load_mw = sum(electricity_mw, na.rm = TRUE), .groups = "drop") |>
    left_join(grid_lookup, by = c("d", "h")) |>
    mutate(grid_utilization = electric_load_mw / pmax(1e-9, grid_capacity_mw))

  grid_total <- grid_utilization |>
    summarise(
      avg_grid_utilization = weighted.mean(grid_utilization, weight_days, na.rm = TRUE),
      peak_grid_utilization = max(grid_utilization, na.rm = TRUE)
    )

  heat_by_tech <- heat_sol |>
    group_by(scenario, planning_year, technology) |>
    summarise(
      annual_heat_gwh = sum(heat_mwh, na.rm = TRUE) / 1000,
      fuel_varom_direct_carbon_cost_eur = sum(fuel_varom_direct_carbon_cost_eur, na.rm = TRUE),
      direct_emissions_tco2 = sum(direct_emissions_tco2, na.rm = TRUE),
      .groups = "drop"
    )

  elec_by_tech <- elec_sol |>
    group_by(scenario, planning_year, technology) |>
    summarise(
      electricity_use_gwh = sum(electricity_mwh, na.rm = TRUE) / 1000,
      electricity_purchase_cost_eur = sum(electricity_purchase_cost_eur, na.rm = TRUE),
      electricity_emissions_tco2 = sum(electricity_emissions_tco2, na.rm = TRUE),
      .groups = "drop"
    )

  investment_cost_eur <- sum(invest_sol$new_capacity_mw * capex, na.rm = TRUE) +
    storage_invest_value * storage_capex

  annualized_capital_cost_eur <- sum(invest_sol$new_capacity_mw * capex * ann, na.rm = TRUE) +
    storage_invest_value * storage_capex * storage_ann

  fixed_om_cost_eur <- sum(cap_sol$available_capacity_mw * fixed_om, na.rm = TRUE) +
    storage_cap_value * storage_fixed

  storage_total <- storage_sol |>
    summarise(
      storage_throughput_gwh = sum(storage_throughput_mwh, na.rm = TRUE) / 1000,
      storage_cycling_cost_eur = sum(storage_cycling_cost_eur, na.rm = TRUE)
    )

  electric_techs <- gen_params |> filter(fuel_type == "electricity") |> pull(technology)
  biomass_techs <- gen_params |> filter(fuel_type == "biomass") |> pull(technology)
  fossil_techs <- gen_params |> filter(fuel_type == "gas") |> pull(technology)

  annual_summary <- heat_by_tech |>
    group_by(scenario, planning_year) |>
    summarise(
      heat_generation_gwh = sum(annual_heat_gwh, na.rm = TRUE),
      fuel_varom_direct_carbon_cost_eur = sum(fuel_varom_direct_carbon_cost_eur, na.rm = TRUE),
      direct_emissions_tco2 = sum(direct_emissions_tco2, na.rm = TRUE),
      electric_heat_gwh = sum(annual_heat_gwh[technology %in% electric_techs], na.rm = TRUE),
      biomass_heat_gwh = sum(annual_heat_gwh[technology %in% biomass_techs], na.rm = TRUE),
      biomass_chp_heat_gwh = sum(annual_heat_gwh[technology == "Biomass_CHP"], na.rm = TRUE),
      biomass_boiler_heat_gwh = sum(annual_heat_gwh[technology == "Biomass_boiler"], na.rm = TRUE),
      fossil_heat_gwh = sum(annual_heat_gwh[technology %in% fossil_techs], na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      elec_by_tech |>
        group_by(scenario, planning_year) |>
        summarise(
          electricity_use_gwh = sum(electricity_use_gwh, na.rm = TRUE),
          electricity_purchase_cost_eur = sum(electricity_purchase_cost_eur, na.rm = TRUE),
          electricity_emissions_tco2 = sum(electricity_emissions_tco2, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("scenario", "planning_year")
    ) |>
    mutate(
      across(c(electricity_use_gwh, electricity_purchase_cost_eur,
               electricity_emissions_tco2), ~ replace_na(.x, 0)),
      storage_throughput_gwh = storage_total$storage_throughput_gwh,
      storage_cycling_cost_eur = storage_total$storage_cycling_cost_eur,
      storage_capacity_mw = storage_cap_value,
      storage_energy_capacity_mwh = storage_cap_value * storage_duration_h,
      storage_investment_mw = storage_invest_value,
            total_available_capacity_including_storage_mw =
        sum(cap_sol$available_capacity_mw, na.rm = TRUE) + storage_cap_value,
      capacity_margin_above_minimum_mw =
        sum(cap_sol$available_capacity_mw, na.rm = TRUE) + storage_cap_value -
        minimum_total_available_capacity_mw,
      avg_grid_utilization = grid_total$avg_grid_utilization,
      peak_grid_utilization = grid_total$peak_grid_utilization,
      investment_cost_eur = investment_cost_eur,
      annualized_capital_cost_eur = annualized_capital_cost_eur,
      fixed_om_cost_eur = fixed_om_cost_eur,
      total_emissions_tco2 = direct_emissions_tco2 + electricity_emissions_tco2,
      total_system_cost_eur =
        fuel_varom_direct_carbon_cost_eur +
        electricity_purchase_cost_eur +
        storage_cycling_cost_eur +
        fixed_om_cost_eur +
        annualized_capital_cost_eur,
      lcoh_eur_per_mwh = total_system_cost_eur / pmax(1, heat_generation_gwh * 1000),
      electrification_share = electric_heat_gwh / pmax(1e-9, heat_generation_gwh),
      biomass_heat_share = biomass_heat_gwh / pmax(1e-9, heat_generation_gwh),
      biomass_chp_share = biomass_chp_heat_gwh / pmax(1e-9, biomass_heat_gwh),
      biomass_boiler_share = biomass_boiler_heat_gwh / pmax(1e-9, biomass_heat_gwh),
      fossil_heat_share = fossil_heat_gwh / pmax(1e-9, heat_generation_gwh)
    )

  updated_blocks <- active_blocks

  for (i in seq_len(nrow(invest_sol))) {
    if (invest_sol$new_capacity_mw[i] > 1e-8) {
      tn <- invest_sol$technology[i]
      updated_blocks <- bind_rows(
        updated_blocks,
        tibble(
          technology = tn,
          install_year = year_value,
          capacity_mw = invest_sol$new_capacity_mw[i],
          lifetime_yr = gen_params$lifetime_yr[gen_params$technology == tn]
        )
      )
    }
  }

  if (storage_invest_value > 1e-8) {
    updated_blocks <- bind_rows(
      updated_blocks,
      tibble(
        technology = "ThermalStorage",
        install_year = year_value,
        capacity_mw = storage_invest_value,
        lifetime_yr = storage_life
      )
    )
  }

  list(
    annual_summary = annual_summary,
    capacity = cap_sol,
    investment = invest_sol,
    heat = heat_by_tech,
    heat_hourly = heat_hourly_sol,
    electricity = elec_by_tech,
    storage = storage_sol,
    grid_utilization = grid_utilization,
    updated_blocks = updated_blocks
  )
}

# ============================================================
# 8) RUN SCENARIOS
# ============================================================

all_annual <- list()
all_capacity <- list()
all_investment <- list()
all_heat <- list()
all_heat_hourly <- list()
all_electricity <- list()
all_storage <- list()
all_grid_utilization <- list()

scenario_names <- unique(scenario_table$scenario)

for (sc in scenario_names) {
  message("Running scenario: ", sc)

  blocks_sc <- capacity_blocks_initial

  for (yr in planning_years) {
    message("  Year: ", yr)

    sr <- scenario_table |>
      filter(scenario == sc, planning_year == yr) |>
      slice(1)

    out <- solve_one_year(
      year_value = yr,
      scenario_row = sr,
      capacity_blocks = blocks_sc,
      scenario_name = sc
    )

    blocks_sc <- out$updated_blocks

    all_annual[[length(all_annual) + 1]] <- out$annual_summary
    all_capacity[[length(all_capacity) + 1]] <- out$capacity
    all_investment[[length(all_investment) + 1]] <- out$investment
    all_heat[[length(all_heat) + 1]] <- out$heat
    all_heat_hourly[[length(all_heat_hourly) + 1]] <- out$heat_hourly
    all_electricity[[length(all_electricity) + 1]] <- out$electricity
    all_storage[[length(all_storage) + 1]] <- out$storage
    all_grid_utilization[[length(all_grid_utilization) + 1]] <- out$grid_utilization
  }
}

annual_summary <- bind_rows(all_annual) |> clean_small_values()
capacity_results <- bind_rows(all_capacity) |> clean_small_values()
investment_results <- bind_rows(all_investment) |> clean_small_values()
heat_results <- bind_rows(all_heat) |> clean_small_values()
heat_hourly_results <- bind_rows(all_heat_hourly) |> clean_small_values()
electricity_results <- bind_rows(all_electricity) |> clean_small_values()
storage_results <- bind_rows(all_storage) |> clean_small_values()
grid_utilization_results <- bind_rows(all_grid_utilization) |> clean_small_values()

# ============================================================
# 9) EXPORT TABLES
# ============================================================

write.csv(annual_summary, file.path(results_dir, "annual_summary.csv"), row.names = FALSE)
write.csv(capacity_results, file.path(results_dir, "capacity_results.csv"), row.names = FALSE)
write.csv(investment_results, file.path(results_dir, "investment_results.csv"), row.names = FALSE)
write.csv(heat_results, file.path(results_dir, "heat_generation_by_technology.csv"), row.names = FALSE)
write.csv(heat_hourly_results, file.path(results_dir, "heat_dispatch_hourly_by_representative_day.csv"), row.names = FALSE)
write.csv(electricity_results, file.path(results_dir, "electricity_use_by_technology.csv"), row.names = FALSE)
write.csv(storage_results, file.path(results_dir, "storage_operation.csv"), row.names = FALSE)
write.csv(grid_utilization_results, file.path(results_dir, "grid_utilization.csv"), row.names = FALSE)
write.csv(tech_params, file.path(results_dir, "technology_parameters_used.csv"), row.names = FALSE)
write.csv(current_config, file.path(results_dir, "current_configuration_used.csv"), row.names = FALSE)

wb <- createWorkbook()
export_tables <- list(
  Annual_Summary = annual_summary,
  Capacity = capacity_results,
  Investment = investment_results,
  Heat_Generation = heat_results,
  Electricity_Use = electricity_results,
  Storage_Operation = storage_results,
  Grid_Utilization = grid_utilization_results,
  Representative_Days = rep_days,
  Cluster_Map = cluster_map,
  Silhouette = silhouette_tbl,
  Scenario_Table = scenario_table,
  Scenario_Plot_Values = scenario_plot_values,
  Technology_Params = tech_params,
  Current_Config = current_config
)

for (nm in names(export_tables)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, export_tables[[nm]])
}

saveWorkbook(wb, file.path(results_dir, "dh_transition_model_results.xlsx"), overwrite = TRUE)

# ============================================================
# 10) PUBLICATION FIGURES
# ============================================================

tech_colors <- c(
  fossil_CHP = "#8B1A1A",
  fossil_boiler = "#CD5C5C",
  Biomass_CHP = "#2E7D32",
  Biomass_boiler = "#8BC34A",
  Electric_Boiler = "#4169E1",
  Air_HP = "#00ACC1",
  Ground_HP = "#6A994E",
  WasteWater_HP = "#7E57C2",
  SeaWater_HP = "#1E88E5",
  ExcessHeat_HP = "#F57C00",
  ThermalStorage = "#708090"
)

capacity_plot_data <- capacity_results |>
  select(scenario, planning_year, technology, available_capacity_mw) |>
  bind_rows(
    annual_summary |>
      transmute(
        scenario,
        planning_year,
        technology = "ThermalStorage",
        available_capacity_mw = storage_capacity_mw
      )
  )

p_capacity <- capacity_plot_data |>
  filter(available_capacity_mw > 1e-4) |>
  ggplot(aes(planning_year, available_capacity_mw, fill = technology)) +
  geom_col() +
  facet_wrap(~ scenario, ncol = 1) +
  scale_fill_manual(values = tech_colors) +
  zero_y() +
  labs(x = "Year", y = "Available capacity (MW)", fill = "Technology") +
  theme_pub()

ggsave(file.path(plot_dir, "01_capacity_evolution.png"), p_capacity, width = 10, height = 11, dpi = 300)

p_heat <- heat_results |>
  filter(annual_heat_gwh > 1e-4) |>
  ggplot(aes(planning_year, annual_heat_gwh, fill = technology)) +
  geom_col() +
  facet_wrap(~ scenario, ncol = 1) +
  scale_fill_manual(values = tech_colors) +
  zero_y() +
  labs(x = "Year", y = "Annual heat generation (GWh)", fill = "Technology") +
  theme_pub()

ggsave(file.path(plot_dir, "02_heat_generation_mix.png"), p_heat, width = 10, height = 11, dpi = 300)

p_invest <- investment_results |>
  filter(new_capacity_mw > 1e-4) |>
  ggplot(aes(planning_year, new_capacity_mw, fill = technology)) +
  geom_col() +
  facet_wrap(~ scenario, ncol = 1) +
  scale_fill_manual(values = tech_colors) +
  zero_y() +
  labs(x = "Year", y = "New capacity investment (MW)", fill = "Technology") +
  theme_pub()

ggsave(file.path(plot_dir, "03_investment_by_year.png"), p_invest, width = 10, height = 11, dpi = 300)

zero_emission_points <- annual_summary |>
  filter(abs(total_emissions_tco2) < 1e-6)

p_emissions <- annual_summary |>
  ggplot(aes(planning_year, total_emissions_tco2, color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_point(
    data = zero_emission_points,
    aes(planning_year, total_emissions_tco2),
    shape = 21,
    fill = "white",
    color = "black",
    size = 3.0,
    stroke = 1.0,
    show.legend = FALSE
  ) +
  geom_text(
    data = zero_emission_points |> filter(planning_year == max(planning_year)),
    aes(label = "0"),
    color = "black",
    vjust = -0.9,
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  zero_y(comma) +
  labs(x = "Year", y = "Annual emissions (tCO2e)", color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "04_emissions_trajectory.png"), p_emissions, width = 9, height = 5.5, dpi = 300)

cost_plot_data <- annual_summary |>
  select(
    scenario, planning_year,
    annualized_capital_cost_eur,
    fixed_om_cost_eur,
    fuel_varom_direct_carbon_cost_eur,
    electricity_purchase_cost_eur,
    storage_cycling_cost_eur
  ) |>
  pivot_longer(-c(scenario, planning_year), names_to = "cost_component", values_to = "value") |>
  mutate(
    cost_component = recode(
      cost_component,
      annualized_capital_cost_eur = "Annualized\ninvestment cost",
      fixed_om_cost_eur = "Fixed O&M\ncost",
      fuel_varom_direct_carbon_cost_eur = "Fuel, variable O&M,\nand direct CO2 cost",
      electricity_purchase_cost_eur = "Electricity\npurchase cost",
      storage_cycling_cost_eur = "Storage\ncycling cost"
    )
  )

p_cost <- cost_plot_data |>
  ggplot(aes(planning_year, value / 1e6, fill = cost_component)) +
  geom_col() +
  facet_wrap(~ scenario, ncol = 1) +
  zero_y() +
  labs(x = "Year", y = "Annual cost (million EUR)", fill = "Cost component") +
  theme_pub() +
  theme(legend.text = element_text(size = 10))

ggsave(file.path(plot_dir, "05_cost_components.png"), p_cost, width = 12, height = 11, dpi = 300)

p_electrification <- annual_summary |>
  ggplot(aes(planning_year, electrification_share, color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  zero_y(percent_format(accuracy = 1)) +
  labs(x = "Year", y = "Electrification share of heat generation", color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "06_electrification_share.png"), p_electrification, width = 9, height = 5.5, dpi = 300)

p_biomass <- annual_summary |>
  ggplot(aes(planning_year, biomass_heat_share, color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  zero_y(percent_format(accuracy = 1)) +
  labs(x = "Year", y = "Biomass heat share", color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "07_biomass_share.png"), p_biomass, width = 9, height = 5.5, dpi = 300)

p_biomass_chp <- annual_summary |>
  select(scenario, planning_year, biomass_chp_share, biomass_boiler_share) |>
  pivot_longer(-c(scenario, planning_year), names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric,
                         biomass_chp_share = "Biomass CHP",
                         biomass_boiler_share = "Biomass boiler")) |>
  ggplot(aes(planning_year, value, color = metric)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  facet_wrap(~ scenario, ncol = 1) +
  zero_y(percent_format(accuracy = 1)) +
  labs(x = "Year", y = "Share within biomass heat", color = "Biomass option") +
  theme_pub()

ggsave(file.path(plot_dir, "08_biomass_chp_vs_boiler.png"), p_biomass_chp, width = 9, height = 8, dpi = 300)

p_compare <- annual_summary |>
  select(scenario, planning_year, lcoh_eur_per_mwh, total_emissions_tco2,
         electrification_share, avg_grid_utilization, peak_grid_utilization,
         storage_energy_capacity_mwh) |>
  pivot_longer(-c(scenario, planning_year), names_to = "metric", values_to = "value") |>
  ggplot(aes(planning_year, value, color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  zero_y() +
  labs(x = "Year", y = NULL, color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "09_scenario_comparison.png"), p_compare, width = 10, height = 11, dpi = 300)

p_grid <- annual_summary |>
  select(scenario, planning_year, avg_grid_utilization, peak_grid_utilization) |>
  pivot_longer(-c(scenario, planning_year), names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric,
                         avg_grid_utilization = "Average grid utilization",
                         peak_grid_utilization = "Peak grid utilization")) |>
  ggplot(aes(planning_year, value, color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  zero_y(percent_format(accuracy = 1)) +
  facet_wrap(~ metric, ncol = 1) +
  labs(x = "Year", y = "Grid utilization", color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "10_grid_utilization.png"), p_grid, width = 9, height = 7, dpi = 300)

p_storage <- annual_summary |>
  select(scenario, planning_year, storage_capacity_mw, storage_energy_capacity_mwh, storage_throughput_gwh) |>
  pivot_longer(-c(scenario, planning_year), names_to = "metric", values_to = "value") |>
  ggplot(aes(planning_year, value, color = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_linetypes) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  zero_y() +
  labs(x = "Year", y = NULL, color = "Scenario", linetype = "Scenario") +
  theme_pub()

ggsave(file.path(plot_dir, "11_storage_results.png"), p_storage, width = 9, height = 8, dpi = 300)

pdf(file.path(plot_dir, "all_publication_figures.pdf"), width = 10, height = 6)
for (p in scenario_plots) print(p)
print(p_scenario_panel)
print(p_silhouette)
print(p_pca)
print(p_cluster_weights)
print(p_capacity)
print(p_heat)
print(p_invest)
print(p_emissions)
print(p_cost)
print(p_electrification)
print(p_biomass)
print(p_biomass_chp)
print(p_compare)
print(p_grid)
print(p_storage)
dev.off()

message("Done.")
message("Storage duration used: ", storage_duration_h, " h")
message("Results saved to: ", results_dir)
message("Plots saved to: ", plot_dir)