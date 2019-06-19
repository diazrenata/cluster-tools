quantile_walk <- function(dat, toy_comm, pars = list(mean = NULL, sd = NULL)) {
  quantile_min = rep(0.49, times = 50)
  quantile_max = rep(0.51, times = 50)
  
  for(i in 1:49) {
    quantile_min[i + 1] <- 0.49 - (i / 100)
    quantile_max[i + 1] <- 0.51 + (i/100)
  }
  
  toy_df <- data.frame(
    quantile_min = quantile_min,
    quantile_max = quantile_max,
    quantile_range = quantile_max - quantile_min
  )
  
  for(i in 1:50) {
    quantile_boundaries <- qnorm(c(toy_df$quantile_min[i], 
                                   toy_df$quantile_max[i]),
                                 mean = pars$mean, sd = 
                                   pars$sd)
    these_ind <- dplyr::filter(toy_comm, dplyr::between(value, quantile_boundaries[1], quantile_boundaries[2]))
    
    if(nrow(these_ind) == 0) {
      toy_df$nbind[i] <- 0
      toy_df$nbspp[i] <- 0
      toy_df$spp_per_ind[i] <- 0
      next
    } else {
      toy_df$nbind[i] <- nrow(these_ind)
      toy_df$nbspp[i] <- length(unique(these_ind$species_ID))
      if(i == 1) {
        toy_df$spp_per_ind <- NA
        next
      }
      if(length(unique(these_ind$species_ID)) == toy_df$nbspp[i - 1]) {
        toy_df$spp_per_ind[i] = 0
      } else {
        toy_df$spp_per_ind[i] <-  (length(unique(these_ind$species_ID)) - toy_df$nbspp[i - 1]) / (nrow(these_ind) - toy_df$nbind[i - 1])
      }
    }
  }
  
  return(toy_df)
}