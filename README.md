# Long-Term District Heating Transition Optimization

R implementation of a scenario-based, multi-year district-heating transition optimization framework using seasonal representative-day clustering and hourly operational representation.

## What the model does

The framework combines:

- scenario-based evolution of heat demand, energy prices, carbon prices and electricity-system conditions;
- seasonal multi-signal clustering of historical days using PAM and silhouette-based selection of the number of clusters;
- representative days that retain their original 24-hour profiles;
- temperature-dependent heat-pump performance;
- sequential annual capacity-expansion optimization from 2025 to 2050;
- thermal storage and hourly grid constraints;
- technology, biomass and emissions constraints;
- hourly dispatch outputs for representative days.

The Estonian district-heating system used in the associated study is an application of the framework.

## Repository structure

```text
.
├── R/
│   ├── dh_transition_model.R
│   ├── representative_day_validation.R
│   └── hourly_dispatch_validation.R
├── data/
│   └── README.md
├── results/
│   └── .gitkeep
├── README.md
├── LICENSE
├── CITATION.cff
└── .gitignore
```

## Input data

The main model expects `data/Data2.xlsx`.

The workbook should contain the sheets `Dynamic data`, `Technologies parameters`, and `Current configuration`. The required and optional fields are described in [`data/README.md`](data/README.md).

For the published case study, the exact input data should be supplied only when their redistribution is permitted. If the data cannot be openly redistributed, the repository should provide a clear description of how an eligible researcher can obtain or reproduce them.

## How to run

Open this repository as an RStudio Project so that the repository root is the working directory.

Place the input workbook at:

```text
 data/Data2.xlsx
```

Then run:

```r
source("R/dh_transition_model.R")
```

The model writes its results to:

```text
results/dh_transition_model/
```

The main script creates the output directory automatically.

After the main model has completed, the representative-day validation can be run in the same R session:

```r
source("R/representative_day_validation.R")
```

The hourly dispatch visualization can then be generated with:

```r
source("R/hourly_dispatch_validation.R")
```

## R packages

The main model uses:

`readxl`, `dplyr`, `tidyr`, `lubridate`, `cluster`, `purrr`, `tibble`, `stringr`, `ggplot2`, `openxlsx`, `ompr`, `ompr.roi`, `ROI`, `ROI.plugin.glpk`, and `scales`.

The validation/visualization scripts additionally use `readr`.

## Important implementation note

The repository version removes machine-specific Windows paths. Input and output locations are relative to the repository, so the code does not depend on the original author's local computer.

The optimization formulation and parameterization should be read together with the associated manuscript. The validation scripts are post-processing analyses and do not modify the optimization model.

## Citation

Please cite the associated paper when using this code. A `CITATION.cff` file is provided as a template and should be completed with the final paper DOI and repository DOI when available.
