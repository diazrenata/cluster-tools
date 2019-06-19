make_dat <- function(input){
  norm <- rnorm(floor(input$nind / 2), mean = input$mean1, sd = input$sd1)
  norm2 <- rnorm(floor(input$nind / 2), mean = input$mean2, sd = input$sd2)
  
  dat <- c(norm, norm2)
  
  return(dat)
}

assign_species <- function(dat, input) {
  
  toy_comm <- data.frame(
    value = dat,
    speciesID = NA)
  
  if(input$method == 1) {
    toy_comm$speciesID = sample.int(n =input$nspp, size = length(dat), replace = TRUE)
  } 
  if(input$method == 2) {
    toy_comm$value <- sort(toy_comm$value) 
    for(i in 1:input$nspp) {
      these_ind <- ((i-1)*floor(input$nind/input$nspp)) + (1:floor(input$nind/input$nspp))
      if(i == input$nspp) {
        these_ind <- c(min(these_ind):input$nind)
      }
      toy_comm[these_ind, "speciesID"] <- i
    }
  }
  return(toy_comm)
}

fit_gmm <- function(dat){
  clust <- Mclust(dat,modelNames = "V" )
  return(clust)
}


