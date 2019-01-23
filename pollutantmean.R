pollutantmean <- function(directory, polluant, id = 1:332) {
      
      ##Change the dir
      old_dir <- getwd()
      setwd(directory)
      
      ## Create a vector with the means and a vector with the lengths
      my_mean <- vector('numeric',length = length(id))
      my_length <- vector('numeric',length = length(id))
      j <- 1
      for (i in id) {
            a <- dir()[i]
            b <- read.csv(a)
            c <- data.frame(b)
            if (polluant == 'sulfate') {
                  num <- 2
            } else {
                  num <- 3
            }
            d <- c[,num]
            e <- d[!is.na(d)]
            my_length[j] <- length(e)
            my_mean[j] <- mean(e)
            j <- j+1
      }
      
      ## Compute the final mean
      my_sum <- sum(my_length)
      top <- 0
      for (j in 1:length(my_mean)) {
            f <- my_mean[j] * my_length[j]
            top <- top + f
      }
      final_mean <- top/my_sum
      
      ## Set the directory
      setwd(old_dir)
      
      ## Return the answer
      return(final_mean)
}