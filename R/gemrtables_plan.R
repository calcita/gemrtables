# gemrtables plan

# source("pkgs.R")
source("R/aggregates_function.R")
source("R/cedar_import.R")
source("R/clean_functions.R")
source("R/import_functions.R")
source("R/merge_files.R")
source("R/other_import.R")
source("R/uis_alternative_access.R")
source("R/uis_import.R")
source("R/gemrtables_function.R")

# drake_plan() stores the plan as targets and commands in a dataframe.
param_plan <-
  drake::drake_plan(
    gem_params = gemrtables()
  )

# import/clean functions
import_clean_plan <-
  drake::drake_plan(
    uis_data = uis(),
    cedar_data = cedar(),
    other_data = other()
  )

# Summarize
summary_plan <-
  drake::drake_plan(
    # other_data = dplyr::bind_rows(wb_data, eurostat_data, oecd_data, un_aids_data, gcpea_data, unicef_wash_data,
    #                               unicef_ecce_learn_data, unicef_ecce_books_data, bullying_data, ict_skills_data, chores_data),
    all_data = dplyr::bind_rows(uis_data, other_data),#, cedar_data
    long_data = long_data()
  )

# Create a workflow plan data frame for the whole plan
gemrtables_plan <- dplyr::bind_rows(param_plan, import_clean_plan, summary_plan)

# Run the plan
drake::make(gemrtables_plan) #prework = "devtools::load_all()"

# Configuration list object
config <- drake::drake_config(gemrtables_plan)

# Interactive dependency graph of all inputs and outputs
drake::vis_drake_graph(config)

# Clean: drake::which_clean(), clean(), file_out()
# Explore chache: drake::cached()
