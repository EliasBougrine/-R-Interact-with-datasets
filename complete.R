complete <- function(directory, id = 1:332) {
      
      ## Change the dir
      old_dir <- getwd()
      setwd(directory)
      
      ## Create a vector with the complete cases
      complete_cases <- vector('numeric', length = length(id))
      k <- 1
      for (i in id) {
            my_sum <- 0
            a <- dir()[i]
            b <- read.csv(a)
            c <- data.frame(b)
            for (j in 1:nrow(c)) {
                  d <- c[j,]
                  e <- sum(is.na(d))
                  if (e == 0) {
                        my_sum <- my_sum + 1
                  }
            }
            complete_cases[k] <- my_sum
            k <- k+1
      }
      
      ## Set the old dir
      setwd(old_dir)
      
      ## construct the final dataframe
      my_data <- data.frame(id = id, nobs = complete_cases)
      return(my_data)
}