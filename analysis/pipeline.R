## ----setup, echo = F-----------------------------------------------------
library(drake)
library(dplyr)
R.utils::sourceDirectory(path = here::here("R"))

## ----empirical channel-----------------------------------------------------------

sim_indices = as.numeric(c(1:2))

empirical_pipeline <- drake_plan(
  empirical_data  = target(get_toy_portal_data()),
  empirical_e = target(add_energy_sizeclass(empirical_data)),
  empirical_isd  = target(make_isd(empirical_e)),
  empirical_gmm = target(fit_gmm(empirical_isd)),
  empirical_id = target(get_integrated_density(empirical_gmm))
)


## ----sim channel------------------------------------------------------------
stdevs <- c(0.1, 0.2)

sim_pipeline <- drake_plan(
  cp = target(get_community_pars(empirical_isd)),
  draw = target(draw_sim(community_pars, sdev, sim_index),
                transform = map(community_pars = cp,
                                sdev = !!stdevs,
                                sim_index = !!sim_indices)
  ),
  e = target(add_energy_sizeclass(draw),
             transform = map(draw)),
  isd = target(make_isd(e),
               transform = map(e)),
  gmm = target(fit_gmm(isd),
               transform = map(isd)),
  id = target(get_integrated_density(gmm),
              transform = map(gmm))
)

sim_ids <- vector()
for(i in 1:nrow(sim_pipeline)) {
  if(rlang::call_name(unlist(sim_pipeline$command)[[i]]) == "get_integrated_density") {
    sim_ids <- c(sim_ids, sim_pipeline$target[[i]])
  } 
}

ids <- c("empirical_id", sim_ids)

thresholds_to_try <- seq(.01, .26, by = 0.01)
thresholds_pipeline <- drake_plan(
  t = target(find_gaps(threshold, id),
             transform = cross(threshold = !!thresholds_to_try,
                               id = !!rlang::syms(ids))
  ),
  r = target(get_result(t),
             transform = map(t)),
  result = target(
    bind_rows(r),
    transform = combine(r)
  )
  )


pipeline <- bind_rows(empirical_pipeline, sim_pipeline, thresholds_pipeline) 

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

result <- readd(result, cache = cache)

result$source <- unlist(thresholds_pipeline[ which(substr(as.character(thresholds_pipeline$target), 0, 2) == "r_"), "target"])
                              
