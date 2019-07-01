# Get S and N
get_community_pars <- function(isd) {
  S <- length(unique(isd$individual_species_ids))
  N <- nrow(isd)
  min_size <- min(isd$individual_sizes)
  max_size <- max(isd$individual_sizes)
  community_pars <- list(S = S, N = N, min_size = min_size, max_size = max_size)
  return(community_pars)
}

get_sim_pars <- function(community_pars, stdev) {
  sim_pars <- append(community_pars, list(stdev = stdev))
  return(sim_pars)
}

# Draw SAD from logseries given S and N
draw_sad <- function(community_pars) {
  
  maxentESF <- meteR::meteESF(S0 = community_pars$S, N0 = community_pars$N)
  
  maxentSAD <- meteR::sad(maxentESF)
  
  simsad <- NULL
  sum_to_n <- FALSE
    while(!sum_to_n) {
      simsad <- maxentSAD$r(community_pars$S)
      if(sum(simsad) == community_pars$N) {
       sum_to_n <- TRUE
      }
    }
  
  return(simsad)
}

# Draw BSD from uniform
draw_bsd <- function(community_pars) {
  means <- runif(n = community_pars$S, min = .8 * (community_pars$min_size), max = 1.2* (community_pars$max_size))
  return(means)
}

# Add standard deviations to BSD
## Current: stdev scales as .1*(mean) on a linear scale. RMD is basing this on her general sense from fieldwork at Portal.
## Future: Get stdev from rodent lit. Requires some thought wrt avoiding circularity. 
add_sd <- function(bsd_means, stdev) {
  sds <- stdev * bsd_means
  bsd <- data.frame(
    means = bsd_means,
    sd = sds
  )
  return(bsd)
}

# Assign sizes to individuals given abundance, mean, and sd
## Current: assumes normal distribution
assign_ind_sizes <- function(bsd_index, bsd){
  this_species <- data.frame(
    individual_species_ids = rep(x = bsd_index, times = bsd$abundance[bsd_index]),
    individual_sizes = rnorm(n = bsd$abundance[bsd_index],
                             mean = bsd$means[bsd_index],
                             sd = bsd$sd[bsd_index])
  )
  
  return(this_species)
}

# Combine BSD and SAD to produce ISD
## Current: randomly combine one time
## Possible future: incoporate size-abundance relationship
## Possible future: use all possible combinations
combine_abds <- function(sad, bsd) {
  
  bsd$abundance <- sample(sad, size = length(sad), replace = FALSE)
  
  return(bsd)
}


# Draw sim(s)
## Wrapper for above functions
draw_sim <- function(community_pars, sim_index = 1){
  
  sad <- draw_sad(community_pars = community_pars)
  bsd <- draw_bsd(community_pars = community_pars)
  bsd <- add_sd(bsd, stdev = community_pars$stdev)
  bsd <- combine_abds(sad = sad, bsd = bsd)
  
  sim <- lapply(1:nrow(bsd),FUN = assign_ind_sizes, bsd = bsd)
  sim <- dplyr::bind_rows(sim)
  return(sim)
}
