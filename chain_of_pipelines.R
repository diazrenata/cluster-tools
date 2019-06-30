library(drake)
library(dplyr)
firsts <- c("she", "been")

add_second_line <- function(first_word) {
  if(first_word == "she") {
    return(paste0(first_word, " was"))
  } else if (first_word == "been") {
    return(paste0(first_word, " beat"))
  }
}

add_third_line <- function(first2) {
  if(first2 == "she was") {
    return(c(first2, "an"))
  } else if (first2 == "been beat") {
    return(first2, "up")
  }
}

make_pipeline2 <- function(pipeline1) {
  targets2 <- list()
  for(i in 1:length(pipeline1$target)) {
    targets2 <- c(targets2, as.name(pipeline1$target[i]))
  }
  pipeline2 <- drake_plan(
    third = target(add_third_line(first2),
                   transform = map(first2 = !!targets2))
  )
  
    return(pipeline2)
}

pipeline1 <- drake_plan(
  first_two = target(add_second_line(first),
                     transform = map(first = !!firsts))
)

pipeline2 <- make_pipeline2(pipeline1)

full_pipeline <- rbind(pipeline1, pipeline2)

make(full_pipeline)
