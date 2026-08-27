# Input data

Place the input workbook here as:

`Data2.xlsx`

The model expects three worksheets.

## 1. `Dynamic data`

Required columns after the model's name cleaning step:

- `year` or `date`: date for each hourly observation
- `hour`: integer 1 to 24
- `demand_mw`: hourly district-heating demand

The model can also use these columns:

- `temp_air`
- `temp_ground`
- `temp_seawater`
- `temp_wastewater`
- `elec_price`
- `gas_price`
- `biomass_price`
- `carbon_price`
- `max_power_mw`
- `renewable_share`
- `carbon_intensity`
- `cop_air`
- `cop_ground`
- `cop_seawater`
- `cop_wastewater`

If some optional columns are absent, the current model supplies defaults for several of them, or assume the specific source does not exist in heat sources temperatures. For reproducibility of published results, it is preferable to provide the actual input values rather than rely on defaults.

The dynamic dataset should contain complete 24-hour days for the historical period used for representative-day selection. The current study uses 2018-2023 historical data for clustering.

## 2. `Technologies parameters`

This sheet must contain at least a `technology` column. The model's default technology table contains these fields:

- `technology`
- `fuel_type`
- `capex_eur_per_mw`
- `fixed_opex_eur_per_mw_yr`
- `var_opex_eur_per_mwh`
- `efficiency`
- `lifetime_yr`
- `co2_kg_per_mwh_fuel`
- `min_load`
- `max_load`
- `available_from`
- `available_until`
- `max_total_capacity_mw`
- `max_annual_invest_mw`
- `source_col`

A row in this sheet overrides the corresponding default technology parameter when the technology name matches.

## 3. `Current configuration`

This sheet is used when `use_excel_current_config = TRUE` in the main script. It should contain:

- `technology`
- `capacity_mw`
- `age_years`
- `efficiency`

The current public version of the script uses its built-in current configuration by default (`use_excel_current_config = FALSE`).

## Reproducibility note

Do not publish data unless you have the right to redistribute it. 
