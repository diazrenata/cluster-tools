norm <- rnorm(50, mean = 10, sd = 1)
norm2 <- rnorm(50, mean = 20, sd = 1)

dat <- c(norm, norm2)
library(mclust)
clust <- Mclust(dat,modelNames = "V" )


toy_comm <- data.frame(
  value = dat,
  # species_ID = sample.int(10, size = 100, replace = TRUE)
  species_ID = ifelse(dat > 15, 1, 2)
)


collection <- ggplot2::ggplot(data = toy_df, ggplot2::aes(x = quantile_range, y = nbind)) +
#  ggplot2::geom_point(data = toy_df, ggplot2::aes(x = quantile_range, y = nbind), color = "red") + 
  ggplot2::geom_point(data = toy_df, ggplot2::aes(x = quantile_range, y = nbspp), color = "blue") +
  ggplot2::geom_point(data = toy_df, ggplot2::aes(x = quantile_range, y = spp_per_ind), color = "purple")

collection
