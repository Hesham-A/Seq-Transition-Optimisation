# ============================================================
# REPRESENTATIVE-DAY VALIDATION ADD-ON
# ============================================================
# Run this section AFTER the main script has created:
#   dyn, daily, rep_days, cluster_map, results_dir, plot_dir
#
# IMPORTANT:
# - This code does NOT rerun the optimisation.
# - It does NOT modify the LP model.
# - It evaluates whether the representative-day reduction preserves
#   hourly variability and important operating conditions.
#
# Main outputs:
#   1) weighted historical-vs-representative hourly statistics
#   2) day-by-day reconstruction errors
#   3) daily clustering-feature reconstruction fidelity
#   4) extreme-condition retention
#   5) joint operating-condition retention
#   6) pairwise correlation preservation
#   7) duration-curve plots
#   8) one Excel workbook with all validation results
# ============================================================

# ------------------------------------------------------------
# 0. Check required objects
# ------------------------------------------------------------
required_objects <- c(
  "dyn", "daily", "rep_days", "cluster_map", "results_dir", "plot_dir"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]

if (length(missing_objects) > 0) {
  stop(
    "Validation cannot start. Missing objects: ",
    paste(missing_objects, collapse = ", "),
    ". Run the main script through the representative-day section first."
  )
}

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------
# 1. Basic structural checks
# ------------------------------------------------------------
if (nrow(dyn) == 0) stop("dyn is empty.")
if (nrow(daily) == 0) stop("daily is empty.")
if (nrow(cluster_map) == 0) stop("cluster_map is empty.")
if (nrow(rep_days) == 0) stop("rep_days is empty.")

required_dyn_cols <- c("source_date", "source_year", "season", "hour")
required_cluster_cols <- c("day_id", "source_year", "season", "cluster_id")
required_rep_cols <- c("day_id", "season", "cluster_id", "rep_id", "cluster_size_hist")

missing_dyn_cols <- setdiff(required_dyn_cols, names(dyn))
missing_cluster_cols <- setdiff(required_cluster_cols, names(cluster_map))
missing_rep_cols <- setdiff(required_rep_cols, names(rep_days))

if (length(missing_dyn_cols) > 0) {
  stop("dyn is missing: ", paste(missing_dyn_cols, collapse = ", "))
}
if (length(missing_cluster_cols) > 0) {
  stop("cluster_map is missing: ", paste(missing_cluster_cols, collapse = ", "))
}
if (length(missing_rep_cols) > 0) {
  stop("rep_days is missing: ", paste(missing_rep_cols, collapse = ", "))
}

# ------------------------------------------------------------
# 2. Select hourly variables that actually exist in dyn
# ------------------------------------------------------------
# COP: use the existing COP variable if present. Otherwise calculate an
# air-source COP only if the helper function and air temperature exist.
if (!("cop_air_validation" %in% names(dyn))) {
  if ("cop_air" %in% names(dyn)) {
    dyn$cop_air_validation <- dyn$cop_air
  } else if (exists("compute_cop", mode = "function") && "temp_air" %in% names(dyn)) {
    dyn$cop_air_validation <- compute_cop(dyn$temp_air)
  } else {
    dyn$cop_air_validation <- NA_real_
  }
}

hourly_validation_vars <- c(
  "demand_mw",
  "temp_air",
  "elec_price",
  "gas_price",
  "biomass_price",
  "carbon_price",
  "max_power_mw",
  "renewable_share",
  "carbon_intensity_t_per_mwh",
  "cop_air_validation"
)

hourly_validation_vars <- hourly_validation_vars[
  hourly_validation_vars %in% names(dyn)
]

hourly_validation_vars <- hourly_validation_vars[
  vapply(
    dyn[hourly_validation_vars],
    function(x) any(is.finite(as.numeric(x))),
    logical(1)
  )
]

hourly_validation_labels <- c(
  demand_mw = "Heat demand",
  temp_air = "Air temperature",
  elec_price = "Electricity price",
  gas_price = "Natural gas price",
  biomass_price = "Biomass price",
  carbon_price = "Carbon price",
  max_power_mw = "Available grid capacity",
  renewable_share = "Renewable electricity share",
  carbon_intensity_t_per_mwh = "Electricity carbon intensity",
  cop_air_validation = "Air-source heat-pump COP"
)

if (length(hourly_validation_vars) < 2) {
  stop("Fewer than two usable hourly variables are available for validation.")
}

# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------
weighted_mean_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

weighted_sd_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (sum(keep) < 2) return(NA_real_)
  mu <- weighted_mean_safe(x[keep], w[keep])
  sqrt(sum(w[keep] * (x[keep] - mu)^2) / sum(w[keep]))
}

weighted_quantile_safe <- function(x, w, probs = c(0.05, 0.50, 0.95)) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(rep(NA_real_, length(probs)))

  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)

  vapply(probs, function(p) {
    idx <- which(cw >= p)[1]
    if (is.na(idx)) x[length(x)] else x[idx]
  }, numeric(1))
}

summary_unweighted <- function(x) {
  x <- as.numeric(x)
  keep <- is.finite(x)
  if (!any(keep)) {
    return(c(
      mean = NA_real_, sd = NA_real_, min = NA_real_,
      p05 = NA_real_, p50 = NA_real_, p95 = NA_real_, max = NA_real_
    ))
  }

  q <- quantile(
    x[keep], probs = c(0.05, 0.50, 0.95),
    names = FALSE, type = 7
  )

  c(
    mean = mean(x[keep]),
    sd = if (sum(keep) > 1) sd(x[keep]) else 0,
    min = min(x[keep]),
    p05 = q[1],
    p50 = q[2],
    p95 = q[3],
    max = max(x[keep])
  )
}

summary_weighted <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  q <- weighted_quantile_safe(x, w)
  finite_x <- is.finite(x)

  c(
    mean = weighted_mean_safe(x, w),
    sd = weighted_sd_safe(x, w),
    min = if (any(finite_x)) min(x[finite_x]) else NA_real_,
    p05 = q[1],
    p50 = q[2],
    p95 = q[3],
    max = if (any(finite_x)) max(x[finite_x]) else NA_real_
  )
}

paired_metrics <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  keep <- is.finite(x) & is.finite(y)

  if (sum(keep) < 2) {
    return(tibble(
      n = sum(keep),
      bias = NA_real_,
      mae = NA_real_,
      rmse = NA_real_,
      nrmse = NA_real_,
      correlation = NA_real_,
      r_squared = NA_real_
    ))
  }

  x <- x[keep]
  y <- y[keep]
  err <- y - x
  rmse <- sqrt(mean(err^2))
  correlation <- suppressWarnings(cor(x, y))

  tibble(
    n = length(x),
    bias = mean(err),
    mae = mean(abs(err)),
    rmse = rmse,
    nrmse = if (is.finite(sd(x)) && sd(x) > 0) rmse / sd(x) else NA_real_,
    correlation = correlation,
    r_squared = if (is.finite(correlation)) correlation^2 else NA_real_
  )
}

percent_error <- function(reference, estimate) {
  if (!is.finite(reference) || abs(reference) < 1e-12 || !is.finite(estimate)) {
    return(NA_real_)
  }
  100 * (estimate - reference) / reference
}

# ------------------------------------------------------------
# 4. Robust day-to-medoid mapping
# ------------------------------------------------------------
# Cluster IDs are meaningful only within a season. We therefore create an
# explicit character key and match on season + cluster_id.

cluster_map_clean <- cluster_map |>
  mutate(
    day_id = as.Date(day_id),
    season_key = trimws(as.character(season)),
    cluster_key = as.character(as.integer(cluster_id)),
    map_key = paste(season_key, cluster_key, sep = "__")
  ) |>
  select(day_id, source_year, season, cluster_id, map_key)

rep_days_clean <- rep_days |>
  mutate(
    day_id = as.Date(day_id),
    season_key = trimws(as.character(season)),
    cluster_key = as.character(as.integer(cluster_id)),
    map_key = paste(season_key, cluster_key, sep = "__")
  )

# One medoid per season/cluster is required.
rep_key_counts <- rep_days_clean |>
  count(map_key, name = "n_rep")

if (any(rep_key_counts$n_rep != 1)) {
  bad_keys <- rep_key_counts |> filter(n_rep != 1)
  print(bad_keys)
  stop("rep_days contains duplicate season/cluster keys.")
}

# Match instead of a dplyr join. This avoids factor/integer join surprises.
rep_index <- match(cluster_map_clean$map_key, rep_days_clean$map_key)

if (anyNA(rep_index)) {
  missing_keys <- unique(cluster_map_clean$map_key[is.na(rep_index)])
  cat("Missing representative-day keys:\n")
  print(missing_keys)
  cat("Available representative-day keys:\n")
  print(rep_days_clean$map_key)
  stop("Some historical days could not be mapped to a representative day.")
}

day_assignment <- cluster_map_clean |>
  mutate(
    rep_day_id = rep_days_clean$day_id[rep_index],
    rep_id = rep_days_clean$rep_id[rep_index]
  ) |>
  select(day_id, source_year, season, cluster_id, rep_day_id, rep_id)

if (nrow(day_assignment) != nrow(cluster_map_clean)) {
  stop("Day-to-medoid mapping changed the number of historical days.")
}

if (anyDuplicated(day_assignment$day_id) > 0) {
  stop("day_assignment contains duplicated historical day IDs.")
}

message(
  "Successfully mapped ", nrow(day_assignment),
  " historical days to ", nrow(rep_days_clean),
  " representative days."
)

# ------------------------------------------------------------
# 5. Build representative-day hourly lookup
# ------------------------------------------------------------
# Each medoid is an actual 24-hour historical day.

rep_profile <- dyn |>
  mutate(source_date = as.Date(source_date)) |>
  inner_join(
    rep_days_clean |>
      select(day_id, rep_id),
    by = c("source_date" = "day_id")
  ) |>
  select(source_date, hour, season, rep_id, all_of(hourly_validation_vars)) |>
  arrange(rep_id, hour)

rep_profile_check <- rep_profile |>
  count(rep_id, name = "n_hours")

expected_rep_ids <- sort(unique(as.integer(rep_days_clean$rep_id)))
actual_rep_ids <- sort(unique(as.integer(rep_profile$rep_id)))

if (!identical(expected_rep_ids, actual_rep_ids)) {
  stop("Not all representative days could be found in dyn.")
}

if (any(rep_profile_check$n_hours != 24)) {
  print(rep_profile_check |> filter(n_hours != 24))
  stop("At least one representative day does not contain exactly 24 hourly observations.")
}

# ------------------------------------------------------------
# 6. Construct a full historical reconstruction using medoid profiles
# ------------------------------------------------------------
# Every historical day is replaced by the 24-hour profile of its selected
# medoid. This is a data-representation test only. No optimisation is run.

orig_hourly <- dyn |>
  mutate(source_date = as.Date(source_date)) |>
  select(source_date, source_year, season, hour, all_of(hourly_validation_vars))

orig_hourly <- orig_hourly |>
  rename_with(
    function(x) paste0(x, "_original"),
    .cols = all_of(hourly_validation_vars)
  )

reconstructed_hourly <- orig_hourly |>
  left_join(
    day_assignment |>
      select(day_id, rep_day_id, rep_id),
    by = c("source_date" = "day_id")
  )

if (anyNA(reconstructed_hourly$rep_id)) {
  stop("Some historical hourly rows have no representative-day ID after day mapping.")
}

rep_profile_values <- rep_profile |>
  select(rep_id, hour, all_of(hourly_validation_vars)) |>
  rename_with(
    function(x) paste0(x, "_representative"),
    .cols = all_of(hourly_validation_vars)
  )

# rep_id + hour uniquely identifies the representative hour.
reconstructed_hourly <- reconstructed_hourly |>
  left_join(
    rep_profile_values,
    by = c("rep_id", "hour")
  )

if (nrow(reconstructed_hourly) != nrow(orig_hourly)) {
  stop("Reconstructed hourly data changed the number of historical observations.")
}

for (v in hourly_validation_vars) {
  nm <- paste0(v, "_representative")
  if (!(nm %in% names(reconstructed_hourly))) {
    stop("Representative series was not created for variable: ", v)
  }
  if (all(is.na(reconstructed_hourly[[nm]]))) {
    stop("Representative series is entirely missing for variable: ", v)
  }
}

# ------------------------------------------------------------
# 7. Pooled historical hourly distribution fidelity
# ------------------------------------------------------------
# Historical cluster_size_hist is used here because the reference population
# is the complete 2018-2023 historical dataset. The optimisation's weight_days
# is an annualised seasonal weight and is NOT used for this comparison.

rep_hourly_historical_weighted <- dyn |>
  mutate(source_date = as.Date(source_date)) |>
  filter(source_date %in% rep_days_clean$day_id) |>
  inner_join(
    rep_days_clean |>
      select(day_id, rep_id, cluster_size_hist),
    by = c("source_date" = "day_id")
  ) |>
  arrange(rep_id, hour)

if (nrow(rep_hourly_historical_weighted) == 0) {
  stop("No representative-day observations were found in dyn.")
}

weighted_hourly_stats <- purrr::map_dfr(hourly_validation_vars, function(v) {
  original_stats <- summary_unweighted(dyn[[v]])
  representative_stats <- summary_weighted(
    rep_hourly_historical_weighted[[v]],
    rep_hourly_historical_weighted$cluster_size_hist
  )

  tibble(
    variable = v,
    label = hourly_validation_labels[[v]],
    original_mean = original_stats["mean"],
    representative_mean = representative_stats["mean"],
    mean_error_pct = percent_error(original_stats["mean"], representative_stats["mean"]),
    original_sd = original_stats["sd"],
    representative_sd = representative_stats["sd"],
    sd_error_pct = percent_error(original_stats["sd"], representative_stats["sd"]),
    original_min = original_stats["min"],
    representative_min = representative_stats["min"],
    original_p05 = original_stats["p05"],
    representative_p05 = representative_stats["p05"],
    original_p50 = original_stats["p50"],
    representative_p50 = representative_stats["p50"],
    original_p95 = original_stats["p95"],
    representative_p95 = representative_stats["p95"],
    original_max = original_stats["max"],
    representative_max = representative_stats["max"],
    min_difference = representative_stats["min"] - original_stats["min"],
    max_difference = representative_stats["max"] - original_stats["max"]
  )
})

write.csv(
  weighted_hourly_stats,
  file.path(results_dir, "validation_weighted_hourly_statistics.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Paired hourly reconstruction error
# ------------------------------------------------------------
# This asks: when an actual historical day is represented by its medoid, how
# close are the 24 hourly values to the actual values on that day?

overall_paired_hourly <- purrr::map_dfr(hourly_validation_vars, function(v) {
  paired_metrics(
    reconstructed_hourly[[paste0(v, "_original")]],
    reconstructed_hourly[[paste0(v, "_representative")]]
  ) |>
    mutate(
      variable = v,
      label = hourly_validation_labels[[v]],
      .before = 1
    )
})

by_year_paired_hourly <- purrr::map_dfr(hourly_validation_vars, function(v) {
  out <- reconstructed_hourly |>
    group_by(source_year) |>
    group_modify(function(.x, .y) {
      paired_metrics(
        .x[[paste0(v, "_original")]],
        .x[[paste0(v, "_representative")]]
      )
    }) |>
    ungroup()

  out |>
    mutate(
      variable = v,
      label = hourly_validation_labels[[v]],
      .before = 1
    )
})

write.csv(
  overall_paired_hourly,
  file.path(results_dir, "validation_paired_hourly_overall.csv"),
  row.names = FALSE
)
write.csv(
  by_year_paired_hourly,
  file.path(results_dir, "validation_paired_hourly_by_year.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. Daily-feature fidelity
# ------------------------------------------------------------
# Recalculate the same daily features used for clustering from the reconstructed
# hourly data, then compare those daily values against the original daily table.

reconstructed_daily <- reconstructed_hourly |>
  group_by(source_date, source_year, season) |>
  summarise(
    demand_sum_mwh = sum(demand_mw_representative, na.rm = TRUE),
    demand_peak_mw = max(demand_mw_representative, na.rm = TRUE),
    demand_mean_mw = mean(demand_mw_representative, na.rm = TRUE),
    temp_air_mean = mean(temp_air_representative, na.rm = TRUE),
    temp_air_min = min(temp_air_representative, na.rm = TRUE),
    hdh18 = sum(pmax(0, 18 - temp_air_representative), na.rm = TRUE),
    elec_price_mean = mean(elec_price_representative, na.rm = TRUE),
    gas_price_mean = mean(gas_price_representative, na.rm = TRUE),
    biomass_price_mean = mean(biomass_price_representative, na.rm = TRUE),
    carbon_price_mean = mean(carbon_price_representative, na.rm = TRUE),
    renewable_share_mean = mean(renewable_share_representative, na.rm = TRUE),
    carbon_intensity_mean = mean(carbon_intensity_t_per_mwh_representative, na.rm = TRUE),
    grid_limit_mean = mean(max_power_mw_representative, na.rm = TRUE),
    grid_limit_min = min(max_power_mw_representative, na.rm = TRUE),
    .groups = "drop"
  )

# Only evaluate features that are present in the actual daily table.
daily_feature_validation_vars <- intersect(
  c(
    "demand_sum_mwh", "demand_peak_mw", "demand_mean_mw",
    "temp_air_mean", "temp_air_min", "hdh18",
    "elec_price_mean", "gas_price_mean", "biomass_price_mean",
    "carbon_price_mean", "renewable_share_mean", "carbon_intensity_mean",
    "grid_limit_mean", "grid_limit_min"
  ),
  intersect(feature_cols, names(daily))
)

if (length(daily_feature_validation_vars) > 0) {
  daily_feature_validation <- purrr::map_dfr(daily_feature_validation_vars, function(v) {
    original_daily <- daily |>
      mutate(day_id = as.Date(day_id)) |>
      select(day_id, source_year, all_of(v)) |>
      rename(original = all_of(v))

    reconstructed_feature <- reconstructed_daily |>
      select(source_date, source_year, all_of(v)) |>
      rename(reconstructed = all_of(v))

    joined <- original_daily |>
      left_join(
        reconstructed_feature,
        by = c("day_id" = "source_date", "source_year")
      )

    paired_metrics(joined$original, joined$reconstructed) |>
      mutate(feature = v, .before = 1)
  })
} else {
  daily_feature_validation <- tibble(
    feature = character(), n = integer(), bias = numeric(), mae = numeric(),
    rmse = numeric(), nrmse = numeric(), correlation = numeric(),
    r_squared = numeric()
  )
}

write.csv(
  daily_feature_validation,
  file.path(results_dir, "validation_daily_features.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 10. Extreme-condition retention
# ------------------------------------------------------------
extreme_specs <- tibble(
  variable = c("demand_mw", "temp_air", "elec_price", "max_power_mw", "cop_air_validation"),
  direction = c("maximum", "minimum", "maximum", "minimum", "minimum")
) |>
  filter(variable %in% hourly_validation_vars)

if (nrow(extreme_specs) > 0) {
  extreme_validation <- purrr::map_dfr(seq_len(nrow(extreme_specs)), function(i) {
    v <- extreme_specs$variable[i]
    direction <- extreme_specs$direction[i]
    original_name <- paste0(v, "_original")
    representative_name <- paste0(v, "_representative")

    out <- reconstructed_hourly |>
      group_by(source_year) |>
      summarise(
        original_extreme = if (
          direction == "maximum"
        ) max(.data[[original_name]], na.rm = TRUE) else min(.data[[original_name]], na.rm = TRUE),
        representative_extreme = if (
          direction == "maximum"
        ) max(.data[[representative_name]], na.rm = TRUE) else min(.data[[representative_name]], na.rm = TRUE),
        .groups = "drop"
      )

    out |>
      mutate(
        variable = v,
        label = hourly_validation_labels[[v]],
        direction = direction,
        absolute_difference = representative_extreme - original_extreme,
        relative_difference_pct = ifelse(
          abs(original_extreme) > 1e-12,
          100 * (representative_extreme - original_extreme) / original_extreme,
          NA_real_
        ),
        .before = 1
      )
  })
} else {
  extreme_validation <- tibble(
    variable = character(), source_year = integer(), label = character(),
    direction = character(), original_extreme = numeric(),
    representative_extreme = numeric(), absolute_difference = numeric(),
    relative_difference_pct = numeric()
  )
}

write.csv(
  extreme_validation,
  file.path(results_dir, "validation_extreme_conditions.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. Joint operating-condition retention
# ------------------------------------------------------------
# Conditions are defined using thresholds calculated from the ORIGINAL data.
# This makes the comparison meaningful: both representations are tested
# against the same physical operating thresholds.

joint_needed <- c(
  "demand_mw", "temp_air", "elec_price", "max_power_mw", "cop_air_validation"
)

if (all(joint_needed %in% hourly_validation_vars)) {
  joint_condition_validation <- reconstructed_hourly |>
    group_by(source_year) |>
    group_modify(function(.x, .y) {
      q_demand <- quantile(.x$demand_mw_original, c(0.75), na.rm = TRUE, names = FALSE)
      q_temp <- quantile(.x$temp_air_original, c(0.25), na.rm = TRUE, names = FALSE)
      q_price <- quantile(.x$elec_price_original, c(0.75), na.rm = TRUE, names = FALSE)
      q_grid <- quantile(.x$max_power_mw_original, c(0.25), na.rm = TRUE, names = FALSE)
      q_cop <- quantile(.x$cop_air_validation_original, c(0.25), na.rm = TRUE, names = FALSE)

      original_flags <- list(
        high_demand_low_temperature = (
          .x$demand_mw_original >= q_demand[1] &
            .x$temp_air_original <= q_temp[1]
        ),
        high_demand_high_electricity_price = (
          .x$demand_mw_original >= q_demand[1] &
            .x$elec_price_original >= q_price[1]
        ),
        high_demand_low_grid_capacity = (
          .x$demand_mw_original >= q_demand[1] &
            .x$max_power_mw_original <= q_grid[1]
        ),
        high_demand_low_temperature_high_price = (
          .x$demand_mw_original >= q_demand[1] &
            .x$temp_air_original <= q_temp[1] &
            .x$elec_price_original >= q_price[1]
        ),
        high_demand_low_temperature_low_cop_low_grid = (
          .x$demand_mw_original >= q_demand[1] &
            .x$temp_air_original <= q_temp[1] &
            .x$cop_air_validation_original <= q_cop[1] &
            .x$max_power_mw_original <= q_grid[1]
        )
      )

      reconstructed_flags <- list(
        high_demand_low_temperature = (
          .x$demand_mw_representative >= q_demand[1] &
            .x$temp_air_representative <= q_temp[1]
        ),
        high_demand_high_electricity_price = (
          .x$demand_mw_representative >= q_demand[1] &
            .x$elec_price_representative >= q_price[1]
        ),
        high_demand_low_grid_capacity = (
          .x$demand_mw_representative >= q_demand[1] &
            .x$max_power_mw_representative <= q_grid[1]
        ),
        high_demand_low_temperature_high_price = (
          .x$demand_mw_representative >= q_demand[1] &
            .x$temp_air_representative <= q_temp[1] &
            .x$elec_price_representative >= q_price[1]
        ),
        high_demand_low_temperature_low_cop_low_grid = (
          .x$demand_mw_representative >= q_demand[1] &
            .x$temp_air_representative <= q_temp[1] &
            .x$cop_air_validation_representative <= q_cop[1] &
            .x$max_power_mw_representative <= q_grid[1]
        )
      )

      purrr::map_dfr(names(original_flags), function(condition_name) {
        tibble(
          condition = condition_name,
          original_fraction = mean(original_flags[[condition_name]], na.rm = TRUE),
          representative_fraction = mean(reconstructed_flags[[condition_name]], na.rm = TRUE)
        ) |>
          mutate(
            absolute_difference = representative_fraction - original_fraction
          )
      })
    }) |>
    ungroup()
} else {
  joint_condition_validation <- tibble(
    source_year = integer(), condition = character(),
    original_fraction = numeric(), representative_fraction = numeric(),
    absolute_difference = numeric()
  )
}

write.csv(
  joint_condition_validation,
  file.path(results_dir, "validation_joint_operating_conditions.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 12. Pairwise correlation preservation
# ------------------------------------------------------------
correlation_vars <- intersect(
  c(
    "demand_mw", "temp_air", "elec_price", "max_power_mw",
    "carbon_intensity_t_per_mwh", "cop_air_validation"
  ),
  hourly_validation_vars
)

if (length(correlation_vars) >= 2) {
  correlation_pairs <- combn(correlation_vars, 2, simplify = FALSE)

  correlation_validation <- purrr::map_dfr(correlation_pairs, function(pair) {
    x <- reconstructed_hourly[[paste0(pair[1], "_original")]]
    y <- reconstructed_hourly[[paste0(pair[2], "_original")]]
    xr <- reconstructed_hourly[[paste0(pair[1], "_representative")]]
    yr <- reconstructed_hourly[[paste0(pair[2], "_representative")]]

    keep_original <- is.finite(x) & is.finite(y)
    keep_reconstructed <- is.finite(xr) & is.finite(yr)

    original_cor <- if (sum(keep_original) >= 2) suppressWarnings(cor(x[keep_original], y[keep_original])) else NA_real_
    representative_cor <- if (sum(keep_reconstructed) >= 2) suppressWarnings(cor(xr[keep_reconstructed], yr[keep_reconstructed])) else NA_real_

    tibble(
      variable_1 = pair[1],
      label_1 = hourly_validation_labels[[pair[1]]],
      variable_2 = pair[2],
      label_2 = hourly_validation_labels[[pair[2]]],
      original_correlation = original_cor,
      representative_correlation = representative_cor,
      absolute_difference = representative_cor - original_cor
    )
  })
} else {
  correlation_validation <- tibble(
    variable_1 = character(), label_1 = character(),
    variable_2 = character(), label_2 = character(),
    original_correlation = numeric(), representative_correlation = numeric(),
    absolute_difference = numeric()
  )
}

write.csv(
  correlation_validation,
  file.path(results_dir, "validation_pairwise_correlations.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 13. Duration curves for key hourly variables
# ------------------------------------------------------------
plot_vars <- intersect(
  c("demand_mw", "temp_air", "elec_price", "max_power_mw", "carbon_intensity_t_per_mwh", "cop_air_validation"),
  hourly_validation_vars
)

plot_percentiles <- seq(0, 1, by = 0.01)

duration_curve_data <- purrr::map_dfr(plot_vars, function(v) {
  original_q <- quantile(
    dyn[[v]],
    probs = 1 - plot_percentiles,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  representative_q <- weighted_quantile_safe(
    rep_hourly_historical_weighted[[v]],
    rep_hourly_historical_weighted$cluster_size_hist,
    probs = 1 - plot_percentiles
  )

  bind_rows(
    tibble(
      variable = v,
      label = hourly_validation_labels[[v]],
      percentile = plot_percentiles * 100,
      representation = "Original hourly data",
      value = original_q
    ),
    tibble(
      variable = v,
      label = hourly_validation_labels[[v]],
      percentile = plot_percentiles * 100,
      representation = "Cluster-weighted representative days",
      value = representative_q
    )
  )
})

write.csv(
  duration_curve_data,
  file.path(results_dir, "validation_duration_curve_data.csv"),
  row.names = FALSE
)

if (nrow(duration_curve_data) > 0) {
  p_validation_duration <- ggplot(
    duration_curve_data,
    aes(percentile, value, color = representation, linetype = representation)
  ) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ label, scales = "free_y", ncol = 2) +
    labs(
      x = "Percentage of hours at or above value (%)",
      y = NULL,
      color = "Representation",
      linetype = "Representation"
    ) +
    theme_pub()

  ggsave(
    file.path(plot_dir, "validation_duration_curves.png"),
    p_validation_duration,
    width = 11,
    height = 10,
    dpi = 300
  )
}

# ------------------------------------------------------------
# 14. Representative-day summary table
# ------------------------------------------------------------
representative_day_summary <- rep_days_clean |>
  transmute(
    representative_day = rep_id,
    season = as.character(season),
    historical_medoid_date = day_id,
    cluster_id = as.integer(cluster_id),
    historical_days_in_cluster = cluster_size_hist,
    annual_weight_days = if ("weight_days" %in% names(rep_days_clean)) weight_days else NA_real_
  ) |>
  arrange(season, cluster_id)

# ------------------------------------------------------------
# 15. Validation overview
# ------------------------------------------------------------
validation_overview <- tibble(
  item = c(
    "Historical hourly observations",
    "Historical complete days",
    "Historical years",
    "Representative days",
    "Heating representative days",
    "Mid-season representative days",
    "Summer representative days",
    "Hourly variables evaluated",
    "Clustering features represented",
    "Historical days represented by cluster weights"
  ),
  value = c(
    nrow(dyn),
    dplyr::n_distinct(as.Date(dyn$source_date)),
    dplyr::n_distinct(dyn$source_year),
    nrow(rep_days_clean),
    sum(as.character(rep_days_clean$season) == "Heating"),
    sum(as.character(rep_days_clean$season) %in% c("Mid-season", "Shoulder")),
    sum(as.character(rep_days_clean$season) == "Summer"),
    length(hourly_validation_vars),
    if (exists("feature_cols")) length(feature_cols) else NA_integer_,
    sum(rep_days_clean$cluster_size_hist, na.rm = TRUE)
  )
)

write.csv(
  validation_overview,
  file.path(results_dir, "validation_overview.csv"),
  row.names = FALSE
)

write.csv(
  representative_day_summary,
  file.path(results_dir, "validation_representative_day_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 16. Excel export
# ------------------------------------------------------------
validation_tables <- list(
  Overview = validation_overview,
  Representative_Days = representative_day_summary,
  Hourly_Weighted = weighted_hourly_stats,
  Hourly_Paired_Overall = overall_paired_hourly,
  Hourly_Paired_By_Year = by_year_paired_hourly,
  Daily_Features = daily_feature_validation,
  Extreme_Conditions = extreme_validation,
  Joint_Conditions = joint_condition_validation,
  Pairwise_Correlations = correlation_validation,
  Duration_Curve_Data = duration_curve_data
)

if ("openxlsx" %in% rownames(installed.packages())) {
  openxlsx::write.xlsx(
    validation_tables,
    file = file.path(results_dir, "representative_day_validation_results.xlsx"),
    overwrite = TRUE
  )
} else {
  warning("Package 'openxlsx' is not installed. CSV files were written, but Excel export was skipped.")
}

# ------------------------------------------------------------
# 17. Final message
# ------------------------------------------------------------
message("------------------------------------------------------------")
message("Representative-day validation completed.")
message("Historical observations: ", nrow(dyn))
message("Historical days: ", dplyr::n_distinct(as.Date(dyn$source_date)))
message("Representative days: ", nrow(rep_days_clean))
message("Results folder: ", results_dir)
if (file.exists(file.path(plot_dir, "validation_duration_curves.png"))) {
  message("Duration curves: ", file.path(plot_dir, "validation_duration_curves.png"))
}
if (file.exists(file.path(results_dir, "representative_day_validation_results.xlsx"))) {
  message("Excel workbook: ", file.path(results_dir, "representative_day_validation_results.xlsx"))
}
message("------------------------------------------------------------")
