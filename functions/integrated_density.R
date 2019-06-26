
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
    dplyr::filter(start_is_pit, by_max <= threshold) 
  
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

get_dat_p <- function(dat_p_args = list(integrated_density, isd)) {
  
  integrated_density <- dat_p_args$integrated_density
  dat_isd <- dat_p_args$isd
  
  intd_tojoin <- integrated_density %>%
    dplyr::rename(ln_size2 = start) %>%
    dplyr::mutate(ln_size2 = as.character(ln_size2))
  
  dat_p <- dat_isd %>%
    dplyr::mutate(ln_size2 = as.character(signif(ln_size, digits = 3))) %>%
    dplyr::left_join(intd_tojoin, by = "ln_size2") %>%
    dplyr::select(-ln_size2)
  
  return(dat_p)
}


get_species_summaries <- function(dat_p) {
  
  species_summaries <- dat_p %>%
    dplyr::group_by(individual_species_ids) %>%
    dplyr::summarise(mean_size = mean(ln_size),
                     mean_p = mean(by_max),
                     sd_p = sd(by_max),
                     prop_in_trough = mean(is_in_trough),
                     n_ind = dplyr::n()) %>%
    dplyr::ungroup()
  species_summaries <- species_summaries %>%
    dplyr::mutate(sd_p = tidyr::replace_na(data = species_summaries$sd_p, replace = 0))
  return(species_summaries)
}

plot_individuals <- function(dat_p, species_summary) {
  
  individuals_plot <- ggplot2::ggplot(data = dat_p, ggplot2::aes(x = ln_size, y = by_max, color = individual_species_ids)) + 
    ggplot2::geom_jitter(size = .5) +
    ggplot2::geom_point(data = species_summary, ggplot2::aes(x = mean_size, y = mean_p, color = individual_species_ids), size = 3) +
    ggplot2::geom_hline(yintercept = 0.05) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  
  return(individuals_plot) 
}


get_trough_individuals <- function(integrated_density, dat_isd, species_summary) {
  
  trough_sizes <- integrated_density %>%
    dplyr::filter(is_in_trough) %>%
    dplyr::select(start) %>%
    dplyr::mutate(ln_size = signif(start, digits = 4)) %>%
    dplyr::select(ln_size) %>%
    dplyr::distinct()
  
  trough_ind <- dat_isd %>%
    dplyr::mutate(ln_size = signif(ln_size, digits = 4)) %>%
    dplyr::filter(ln_size %in% trough_sizes$ln_size) %>%
    dplyr::left_join(species_summary, by = "individual_species_ids") %>%
    dplyr::select(-individual_sizes, -individual_energy, -size_class, -size_class_g, -ln_energy) %>%
    dplyr::arrange(dplyr::desc(individual_species_ids)) %>%
    dplyr::mutate(relative_size = ln_size/mean_size)
  
  return(trough_ind)
  
}


plot_trough_individuals <- function(trough_ind) {
  
  if(nrow(trough_ind) == 0) {
    trough_plot <- ggplot2::ggplot(data = trough_ind, ggplot2::aes(x = prop_in_trough, y = relative_size, color = individual_species_ids, size = n_ind))
    return(trough_plot)
    }
  
  max_rel_size = max(trough_ind$relative_size, na.rm =T) + 1
  
  trough_plot <- ggplot2::ggplot(data = trough_ind, ggplot2::aes(x = prop_in_trough, y = relative_size, color = individual_species_ids)) +
    ggplot2::xlim(0, 1) + 
    ggplot2::ylim(0, max_rel_size) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::geom_jitter() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  
  return(trough_plot)
}


get_mode_dom <- function(threshold, dat_p) {
  
  N <- as.numeric(nrow(dat_p))
  S <- as.numeric(length(unique(dat_p$individual_species_ids)))
  
  modes <- dat_p %>%
    dplyr::filter(by_max > threshold)
  
  mode_richness <- as.numeric(length(unique(modes$individual_species_ids)))
  mode_abund <- as.numeric(nrow(modes))
  
  mode_dom <- (mode_richness / S) / (mode_abund / N)
  
  return(mode_dom)
}

get_mode_thresholded <- function(dat_p) {
  mode_thresholded <- data.frame(
    threshold = seq(.01, 1, by = 0.01),
    mode_dom = vapply(seq(0.01, 1, by = 0.01), FUN = get_mode_dom, 
                      dat_p = dat_p, FUN.VALUE = .75)
  )
  return(mode_thresholded)
}

plot_mode_dom <- function(mode_thresholded) {
  
  thresh_plot <- ggplot2::ggplot(data = mode_thresholded, 
                                 ggplot2::aes(x = threshold, y = mode_dom)) +
    ggplot2::geom_point() + 
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::theme_bw()
  
  return(thresh_plot)
  
}
