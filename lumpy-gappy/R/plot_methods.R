# Plot an ISD

plot_isd <- neonbecs::plot_isd

# Plot integrated density

## Options:
### With maxima/minima
### With threshold lines
### With trough boundaries demarcated

plot_integrated_density <- function(integrated_density, threshold_lines = NULL, pit_boundaries = FALSE) {
  
  integrated_plot <- ggplot2::ggplot(data = integrated_density, ggplot2::aes(x = start, y = by_max)) +
    ggplot2::geom_point(inherit.aes = TRUE, size = .1) +
    ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_peak), c("start", "by_max")], inherit.aes = TRUE, color = "green", size = 2) +
    ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_pit), c("start", "by_max")], inherit.aes = TRUE, color = "blue", size = 2) +
    ggplot2::theme_bw()
  
  if(!is.null(threshold_lines)) {
    integrated_plot <- integrated_plot +
      ggplot2::geom_hline(yintercept = threshold_lines, color = "red")
  }
  
  if(pit_boundaries) {
    
    if(!("is_gap_start" %in% colnames(integrated_density))) {
      warning("Missing trough boundaries")
      return(integrated_plot)
    }
    
    integrated_plot <- integrated_plot + 
      ggplot2::geom_point(data = integrated_density[ which(integrated_density$is_gap_start), c("start", "by_max")], inherit.aes = TRUE, color = "red", size = 2, shape = 4) +
      ggplot2::geom_point(data = integrated_density[ which(integrated_density$is_gap_end), c("start", "by_max")], inherit.aes = TRUE, color = "red", size = 2, shape = 4) 
  }
  
  return(integrated_plot)
}

# Plot ISD methods summary variables

plot_isd_methods_summary <- function(summary_dataframe) {
  
  baseplot <- ggplot2::ggplot(data = summary_dataframe) +
    ggplot2::theme_bw()
  
  if(any(summary_dataframe$empirical)) {
    baseplot <- baseplot +
      ggplot2::aes(color = empirical)
}
  modes_plot <- baseplot + 
    ggplot2::geom_bar(ggplot2::aes(x = as.factor(nmodes))) +
    ggplot2::labs(x = "Number of modes", y = "Count", title = "Number of modes (all)")
  
  modes_abovet_plot <- baseplot + 
    ggplot2::geom_bar(ggplot2::aes(x = as.factor(nmodes_above_threshold))) +
    ggplot2::labs(x = "Number of modes above threshold", y = "Count", title = "Number of modes above threshold") +
    ggplot2::facet_grid(. ~ threshold)
  
  ntroughs_plot <- baseplot + 
    ggplot2::geom_jitter(ggplot2::aes(x = ntroughs_possible, y = ntroughs), width = .2, height = 0) +
    ggplot2::xlim(-.5, max(summary_dataframe$ntroughs_possible) + .5) + 
    ggplot2::ylim(0, max(summary_dataframe$ntroughs_possible) + 1) +
    ggplot2::labs(x = "Number of troughs possible", y = "Number of troughs", title = "Number of modes above threshold") +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::facet_grid(. ~ threshold)
  
  summary_plots <- list(modes_plot = modes_plot,
                        modes_abovet_plot = modes_abovet_plot,
                        ntroughs_plot = ntroughs_plot)
  
  return(summary_plots)
}
