corr <- function(directory, threshold = 0) {
      
      ## Change the dir
      old_dir <- getwd()
      setwd(directory)
      
      ## Take the length of the new dir
      length_dir <- length(dir())
      
      ##Change back the dir
      setwd(old_dir)
      
      ## Main code
      my_vect <- rep(NA, times = length_dir)
      for (i in 1:length_dir) {
            a <- complete(directory,i)
            b <- a[,2]
            if (b > threshold) {
                  setwd(directory)
                  c <- dir()[i]
                  d <- read.csv(c)
                  e <- data.frame(d)
                  f <- e[,2]
                  g <- e[,3]
                  good <- complete.cases(f,g)
                  h <- f[good]
                  l <- g[good]
                  m <- cor(h,l)
                  my_vect[i] <- m
                  setwd(old_dir)
            }
      }
      my_vect <- my_vect[!is.na(my_vect)]
      return(my_vect)
}