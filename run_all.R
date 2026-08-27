# Run the full model and the post-processing/validation scripts in sequence.
# Run this file from the repository root.

source("R/dh_transition_model.R")
source("R/representative_day_validation.R")
source("R/hourly_dispatch_validation.R")
