

collection <- ggplot2::ggplot(data = toy_df, ggplot2::aes(x = quantile_range, y = nbind)) +
#  ggplot2::geom_point(data = toy_df, ggplot2::aes(x = quantile_range, y = nbind), color = "red") + 
  ggplot2::geom_point(data = toy_df, ggplot2::aes(x = quantile_range, y = nbspp), color = "blue") +
  ggplot2::geom_point(data = toy_df, ggplot2::aes(x = quantile_range, y = spp_per_ind), color = "purple")

collection
