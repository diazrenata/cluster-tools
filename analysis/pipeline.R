## ----setup, echo = F-----------------------------------------------------
library(drake)
library(dplyr)
R.utils::sourceDirectory(path = here::here("R"))

## ----get data------------------------------------------------------------
get_data <- drake_plan(
  dat  = target(get_toy_portal_data()),
  dat_e = target(add_energy_sizeclass(dat)),
  isd  = target(make_isd(dat_e))
)


## ----draw sim------------------------------------------------------------
draw_one_sim <- drake_plan(
  communitypars = target(get_community_pars(isd)),
  sim = target(draw_sim(community_pars = communitypars)),
  sim_e = target(add_energy_sizeclass(sim)),
  sim_isd = target(make_isd(sim_e))
)



## ----fit gmms-----------------------------------------------------------
fit_gmms <- drake_plan(
  isds_to_analyze= target(list(emp = isd,
                               sim = sim_isd)),
  
  gmms = target(lapply(isds_to_analyze, fit_gmm)),
  integrated_densities = target(lapply(gmms, get_integrated_density))
)

## ----try many thresholds-------------------------------------------------

try_thresholds <- drake_plan(
  thresholds_to_try = target(seq(0, .25, by = 0.01)),
  
  emp_thresholds= target(lapply(thresholds_to_try, find_gaps, integrated_density = integrated_densities[[1]])),
  
  
  empirical_result =target(get_result(emp_thresholds) %>%
                             dplyr::mutate(source = "empirical")),
  
  sim_thresholds = target(lapply(thresholds_to_try, find_gaps, integrated_density = integrated_densities[[2]])),
  
  sim_result =target(get_result(sim_thresholds) %>%
                       dplyr::mutate(source = "sim")),
  
  
  results = target(dplyr::bind_rows(empirical_result, sim_result))
)

pipeline <- bind_rows(get_data, draw_one_sim, fit_gmms, try_thresholds)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
  config <- drake_config(pipeline, cache = cache)
  sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
  vis_drake_graph(config, build_times = "none")     # requires "visNetwork" package
}

make(pipeline, cache = cache, cache_log_file = here::here("drake", "cache_log.txt"))

