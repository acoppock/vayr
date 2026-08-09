# Datasets for the seven worked examples in Coppock (2021), "Visualize As You
# Randomize", chapter 17 of Advances in Experimental Political Science.
#
# The files in data-raw/replication_archive/ are the chapter's replication
# archive, downloaded unmodified from https://doi.org/10.7910/DVN/VE6VSR. All
# seven are simulated, and make_datasets.R in that folder is the code that
# simulated them.
#
# Run this script to rebuild the .rda files in data/.

library(readr)
library(tibble)
library(usethis)

archive <- "data-raw/replication_archive"

# as_tibble() drops readr's spec_tbl_df class and its column-spec attribute, so
# the shipped objects are plain tibbles like patriot_act.
two_arm_trial <- as_tibble(read_csv(file.path(archive, "two_arm_simulated_data.csv"), show_col_types = FALSE))
blocked_experiment <- as_tibble(read_csv(file.path(archive, "blocked_simulated_data.csv"), show_col_types = FALSE))
clustered_experiment <- as_tibble(read_csv(file.path(archive, "clustered_simulated_data.csv"), show_col_types = FALSE))
covariate_adjustment <- as_tibble(read_csv(file.path(archive, "covariate_simulated_data.csv"), show_col_types = FALSE))
continuous_interaction <- as_tibble(read_csv(file.path(archive, "interaction_simulated_data.csv"), show_col_types = FALSE))
noncompliance_experiment <- as_tibble(read_csv(file.path(archive, "noncompliance_simulated_data.csv"), show_col_types = FALSE))
attrition_experiment <- as_tibble(read_csv(file.path(archive, "attrition_simulated_data.csv"), show_col_types = FALSE))

# The only edit to the archive's contents: two column names in the
# noncompliance data carry a space and a capital, so they need backticks at
# every use.
names(noncompliance_experiment)[names(noncompliance_experiment) == "Treatment Receipt"] <- "D"
names(noncompliance_experiment)[names(noncompliance_experiment) == "Turnout"] <- "Y"

use_data(two_arm_trial, overwrite = TRUE, compress = "xz")
use_data(blocked_experiment, overwrite = TRUE, compress = "xz")
use_data(clustered_experiment, overwrite = TRUE, compress = "xz")
use_data(covariate_adjustment, overwrite = TRUE, compress = "xz")
use_data(continuous_interaction, overwrite = TRUE, compress = "xz")
use_data(noncompliance_experiment, overwrite = TRUE, compress = "xz")
use_data(attrition_experiment, overwrite = TRUE, compress = "xz")
