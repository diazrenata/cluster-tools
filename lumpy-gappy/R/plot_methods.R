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
