library(drake)
library(dplyr)
R.utils::sourceDirectory(path = here::here("R"))

sim_indices = as.numeric(c(1:2))
stdevs = c(0.1, 0.2)
thresholds_to_try = seq(.01, .26, by = 0.05)

dats <- drake_plan(
  dat1  = target(get_toy_portal_data()),
  dat2 = target(get_toy_portal_data())
)

cp_pipeline <- make_cp_pipeline(dats)

sp_pipeline <- make_sp_pipeline(cp_pipeline = cp_pipeline,
                                stdevs = stdevs)

draw_pipeline <- make_draw_pipeline(sp_pipeline = sp_pipeline,
                                    sim_indices = sim_indices)
sim_id_pipeline <- make_id_pipeline(draw_pipeline, "sim")

empirical_id_pipeline <- make_id_pipeline(dats, "emp")

id_pipelines <- rbind(sim_id_pipeline, empirical_id_pipeline)

thresholds_pipeline <- make_thresholds_pipeline(id_pipelines,
                                                thresholds_to_try)

full_pipeline <- rbind(dats, cp_pipeline, sp_pipeline, draw_pipeline, id_pipelines, thresholds_pipeline)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
  config <- drake_config(full_pipeline, cache = cache)
  sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
  vis_drake_graph(config, build_times = "none")     # requires "visNetwork" package
}

make(full_pipeline, cache = cache, cache_log_file = here::here("drake", "cache_log.txt"))
