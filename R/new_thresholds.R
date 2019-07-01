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

get_type <- function(integrated_density) {
  return(integrated_density$type[1])
}

get_datname <- function(integrated_density) {
  return(integrated_density$dat_name[1])
}

get_stdev <- function(integrated_density) {
  return(integrated_density$stdev[1])
}

get_result <- function(integrated_density) {
  result <- data.frame(ngaps = count_gaps(integrated_density),
                       npeaks = count_peaks(integrated_density),
                       threshold = get_threshold(integrated_density),
                       dat_type = get_type(integrated_density),
                       datname = get_datname(integrated_density),
                       stdev = get_stdev(integrated_density))
  return(result)
}
