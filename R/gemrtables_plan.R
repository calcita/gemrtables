# gemrtables plan

# source("pkgs.R")
library(gemrtables)

# drake_plan() stores the plan as targets and commands in a dataframe.

param_plan <-
  drake::drake_plan(
    gem_params = gem_params()
  )

# ....
import_clean_plan <-
  drake::drake_plan(
    uis_data = gemrtables::uis(),
    cedar_data = gemrtables::cedar(),
    wb_data = gemrtables::wb_import(),
    eurostat_data = gemrtables::eurostat_import(),
    oecd_data = gemrtables::oecd_import(),
    un_aids_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1S6E4WwosJHjpu-d557tUjlJ6awMAwLsa") %>% un_aids_clean(),
    gcpea_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1tlya_2MkfFuAZC_fXw8Q3BZqZ3Qq08Q1")  %>% gcpea_clean(),
    unicef_wash_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1I4XbP4fCzlkE-e-98BVgh6UPjM0oEHDI") %>% unicef_wash_clean(),
    unicef_ecce_learn_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1JzOo7rt8O3ZE9eSAL3v1jKexLwbICtM3") %>% unicef_ecce_clean(ind = "home.lrn.env.3t7", source = "UNICEF"),
    unicef_ecce_books_data = readr::read_csv("https://drive.google.com/uc?export=download&id=15oDSTYqYm4Z4lHx7nQi31k_G1tjHfcGz") %>% unicef_ecce_clean(ind = "home.book.u5", source = "UNICEF"),
    bullying_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1QgrTGKWuhlgd_oYtCKAiUw4SSgdj4xts") %>% bullying_clean(),
    ict_skills_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1dV4T7RSDbJmYvhy7ydUzLCkLCTcUkZzl") %>% ict_skills_clean(),
    chores_data = readr::read_csv("https://drive.google.com/uc?export=download&id=1tTQB6CtgGkBwj4SBsjLdfR7l_4mVlFfs") %>% chores_clean()
  )

# ....
summary_plan <-
  drake::drake_plan(
    other_data = dplyr::bind_rows(wb_data, eurostat_data, oecd_data, un_aids_data, gcpea_data, unicef_wash_data,
                                  unicef_ecce_learn_data, unicef_ecce_books_data, bullying_data, ict_skills_data, chores_data),
    all_data = dplyr::bind_rows(uis_data, cedar_data, other_data),
    weights_data = gemrtables::weights(), #%>% ...,
    country_data = gemrtables::country(),
    uis_aggregates = uis_aggregates(),
    computed_aggregates = computed_aggregates(),
    long_data = long_data(country_data, uis_aggregates, computed_aggregates),
    wide_data = format_wide(long_data)
  )

# Create a workflow plan data frame for the whole plan
gemrtables_plan <- dplyr::bind_rows(param_plan, import_plan, summary_plan)

# Run the plan
make(gemrtables_plan, prework = "devtools::load_all()")

# Configuration list object
config <- drake::drake_config(gemrtables_plan)

# Interactive dependency graph of all inputs and outputs
drake::vis_drake_graph(config)

