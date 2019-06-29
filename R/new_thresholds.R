find_gaps <- function(threshold, integrated_density) {
  first_peak <- min(integrated_density[ which(integrated_density$start_is_peak), "start"])
  
  within_peaks <- dplyr::filter(integrated_density, start >= first_peak, by_max >= threshold) %>%
    dplyr::mutate(start_diff = (start - dplyr::lag(start, n = 1, default = NA)) > 0.000100000001) %>%
    dplyr::mutate(is_gap_start = dplyr::lead(start_diff, n = 1, default = F),
                  is_gap_end = start_diff) %>%
    tidyr::replace_na(replace = list(start_diff = FALSE,
                                     is_gap_start = FALSE,
                                     is_gap_end = FALSE))
  
  integrated_density <- dplyr::left_join(integrated_density,
                                         within_peaks,
                                         by = colnames(integrated_density)) %>%
    tidyr::replace_na(replace = list(start_diff = FALSE,
                                     is_gap_start = FALSE,
                                     is_gap_end = FALSE)) %>%
    dplyr::mutate(threshold = threshold)
  
  return(integrated_density)
  
}

count_gaps <- function(integrated_density) {
  return(length(which(integrated_density$is_gap_start)))
}

count_peaks <- function(integrated_density) {
  return(length(which(integrated_density$start_is_peak)))
}

get_threshold <- function(integrated_density) {
  return(integrated_density$threshold[1])
}

get_result <- function(integrated_density) {
  result <- data.frame(ngaps = vapply(integrated_density, FUN = count_gaps, FUN.VALUE = 1),
                       npeaks = vapply(integrated_density, FUN = count_peaks, FUN.VALUE = 1),
                       threshold = vapply(integrated_density, FUN = get_threshold, FUN.VALUE = .5))
  return(result)
}