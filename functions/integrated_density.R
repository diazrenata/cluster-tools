
get_integrated_density <- function(dat_gmm, interval_size = 0.001, min_size = 0, max_size = 8) {
  
  density_evalpoints <- seq(min_size, max_size, by = interval_size)
  
  density_estimates <- predict(dat_gmm, newdata = density_evalpoints)
  
  integrated_density <- data.frame(start = density_evalpoints[1:length(density_evalpoints) -1],
                                   stop = density_evalpoints[2:length(density_evalpoints)],
                                   start_density = density_estimates[1:length(density_evalpoints) -1]) %>%
    dplyr::mutate(density = start_density * (stop - start),
                  by_max = density / max(density))
  
  p_turnpoints <- pastecs::turnpoints(integrated_density$by_max)
  
  integrated_density$start_is_peak <- p_turnpoints$peaks[1:length(density_evalpoints) -1 ]
  integrated_density$start_is_pit <- p_turnpoints$pits[1:length(density_evalpoints) -1 ]
  integrated_density$start_is_turnpoint <- (integrated_density$start_is_peak | integrated_density$start_is_pit)
  
  return(integrated_density)
}

plot_integrated_density <- function(integrated_density, threshold_lines = TRUE, pit_boundaries = FALSE) {
  
  integrated_plot <- ggplot2::ggplot(data = integrated_density, ggplot2::aes(x = start, y = by_max)) +
    ggplot2::geom_point(inherit.aes = TRUE, size = .1) +
    ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_peak), c("start", "by_max")], inherit.aes = TRUE, color = "green", size = 2) +
    ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_pit), c("start", "by_max")], inherit.aes = TRUE, color = "blue", size = 2) +
    ggplot2::theme_bw()
  
  if(threshold_lines) {
    integrated_plot <- integrated_plot +
      ggplot2::geom_hline(yintercept = 0.08, color = "yellow") +
      ggplot2::geom_hline(yintercept = 0.05, color = "orange") +
      ggplot2::geom_hline(yintercept = 0.01, color = "red")
  }
  
  if(pit_boundaries) {
    
    if(!("start_is_trough_start" %in% colnames(integrated_density))) {
      warning("Missing trough boundaries")
      return(integrated_plot)
    }
    
    integrated_plot <- integrated_plot + 
      ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_trough_start), c("start", "by_max")], inherit.aes = TRUE, color = "red", size = 2, shape = 4) +
      ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_trough_stop), c("start", "by_max")], inherit.aes = TRUE, color = "red", size = 2, shape = 4) 
  }
  
  return(integrated_plot)
}


find_pit_sections <- function(pit, integrated_density, threshold) {
  
  turns <- integrated_density %>%
    dplyr::filter(start_is_turnpoint)
  
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
  
  pits <- integrated_density %>%
    dplyr::filter(start_is_pit) 
  
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
    return(integrated_density)
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
  pit_boundaries <- find_all_pit_boundaries(integrated_density = integrated_density,
                                            threshold = threshold)
  
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