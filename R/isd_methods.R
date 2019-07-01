# Make an ISD from a community data table

add_energy_sizeclass <- replicatebecs::add_energy_sizeclass

make_isd <- neonbecs::make_isd

# Fit GMM

fit_gmm <- neonbecs::fit_gmm

# Integrate density of GMM
get_integrated_density <- function(gmm, interval_size = 0.0001, min_size = 0, max_size = 8, type = "sim", dat_name, stdev) {
  
  density_evalpoints <- seq(min_size, max_size, by = interval_size)
  
  density_estimates <- predict(gmm, newdata = density_evalpoints)
  
  integrated_density <- data.frame(start = density_evalpoints[1:length(density_evalpoints) -1],
                                   stop = density_evalpoints[2:length(density_evalpoints)],
                                   start_density = density_estimates[1:length(density_evalpoints) -1]) %>%
    dplyr::mutate(density = start_density * (stop - start),
                  by_max = density / max(density))
  
  p_turnpoints <- pastecs::turnpoints(integrated_density$by_max)
  
  pits <- c()
  peaks <- c()
  
  tppos <- p_turnpoints$tppos
  if(length(tppos) > 0) {
    if(p_turnpoints$firstispeak) {
      peaks <- tppos[ seq(1, length(tppos), by = 2)]
      if(length(tppos) > 1) {
        pits <- tppos[ seq(2, length(tppos), by =2)]
      } 
    } else {
      pits <- tppos[ seq(1, length(tppos), by =2)]
      if(length(tppos) > 1) {
        peaks <- tppos[ seq(2, length(tppos), by =2)]
      }
    }
  }
  integrated_density$start_is_peak <- c(1:(length(density_evalpoints) - 1)) %in% peaks
  integrated_density$start_is_pit <- c(1:(length(density_evalpoints) - 1)) %in% pits
  integrated_density$start_is_turnpoint <- (integrated_density$start_is_peak | integrated_density$start_is_pit)
  integrated_density$type = type
  integrated_density$dat_name = dat_name
  integrated_density$stdev = stdev
  
  return(integrated_density)
}


# Find troughs in density to a given threshold
find_pit_sections <- function(pit, integrated_density, threshold) {
  
  turns <- integrated_density %>%
    dplyr::filter(start_is_turnpoint) %>%
    dplyr::mutate(start_is_peak = by_max >= threshold)
  
  lower_peak <- turns %>%
    dplyr::filter(start_is_peak, start <= pit) %>%
    dplyr::filter(start == max(start))
  
  upper_peak <- turns %>%
    dplyr::filter(start_is_peak, start >= pit) %>%
    dplyr::filter(start == min(start))
  
  pit_subset <- integrated_density %>%
    dplyr::filter(dplyr::between(start, lower_peak$start[1], upper_peak$start[1]))
  
  pit_below_threshold <- pit_subset %>%
    dplyr::filter(by_max <= threshold)
  
  return(pit_below_threshold)
}

find_all_pits <- function(integrated_density, threshold = 0.05) {
  
  peaks_above_thresh <- dplyr::filter(integrated_density,
                                      start_is_peak, by_max >= threshold)
  
  pits <- integrated_density %>%
    dplyr::filter(start_is_pit, by_max <= threshold,
                  start > min(peaks_above_thresh$start))
  
  if(!any(pits$by_max <= threshold)) {
    return(NULL)
  }
  
  all_pits <- lapply(pits$start, FUN = find_pit_sections, 
                     integrated_density = integrated_density,
                     threshold = threshold)
  return(all_pits)
}

find_all_pit_boundaries <- function(integrated_density, threshold = 0.05) {
  all_pits <- find_all_pits(integrated_density = integrated_density,
                            threshold = threshold) 
  if(is.null(all_pits)) {
    warning("No troughs below threshold")
    return(NULL)
  }
  
  pit_boundaries <- data.frame(
    pit_index = 1:length(all_pits),
    pit_start = NA,
    pit_stop = NA,
    pit_length = NA
  )
  
  for(i in 1:nrow(pit_boundaries)) {
    pit_boundaries$pit_start[i] <- min(all_pits[[i]]$start)
    pit_boundaries$pit_stop[i] <- max(all_pits[[i]]$stop)
    pit_boundaries$pit_length[i] <- pit_boundaries$pit_stop[i] - pit_boundaries$pit_start[i]
  }
  return(pit_boundaries) 
}

add_all_pit_boundaries <- function(integrated_density, threshold = 0.05) {
  peaks_above_thresh <- dplyr::filter(integrated_density,
                                      start_is_peak == TRUE,
                                      by_max >= threshold)
  
  if(nrow(peaks_above_thresh) < 2) {
    integrated_density$start_is_trough_start <- FALSE
    integrated_density$start_is_trough_stop <- FALSE
    integrated_density$is_in_trough <- FALSE
    warning("Fewer than 2 peaks above threshold")
    return(integrated_density)
  }
  pit_boundaries <- find_all_pit_boundaries(integrated_density = integrated_density,
                                            threshold = threshold)
  if(is.null(pit_boundaries)) {
    integrated_density$start_is_trough_start <- FALSE
    integrated_density$start_is_trough_stop <- FALSE
    integrated_density$is_in_trough <- FALSE
    return(integrated_density)
  }
  
  integrated_density$start_is_trough_start <- integrated_density$start %in% pit_boundaries$pit_start
  integrated_density$start_is_trough_stop <- integrated_density$start %in% pit_boundaries$pit_stop
  integrated_density$is_in_trough <- FALSE
  for(i in 1:nrow(pit_boundaries)) {
    integrated_density$is_in_trough[ which(dplyr::between(integrated_density$start, 
                                                          pit_boundaries$pit_start[i],
                                                          pit_boundaries$pit_stop[i]))] <- TRUE
  }
  
  return(integrated_density)
}

# Count modes and troughs in density

isd_methods_summary <- function(integrated_density, threshold, empirical = F) {
  nmodes  <- sum(integrated_density$start_is_peak)
  
  nmodes_above_threshold <- nrow(dplyr::filter(integrated_density,
                                               start_is_peak == TRUE,
                                               by_max >= threshold))
  
  ntroughs <- sum(integrated_density$start_is_trough_start)
  
  ntroughs_possible <- nmodes_above_threshold - 1
  
  summary_variables <- list(nmodes = nmodes, 
                            nmodes_above_threshold = nmodes_above_threshold,
                            ntroughs = ntroughs,
                            ntroughs_possible = ntroughs_possible,
                            threshold = threshold,
                            empirical = empirical)
  
  
  return(summary_variables)
}