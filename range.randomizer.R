tempvector[is.na(tempvector)] <- rnorm(1, mean = 3, sd = 1)
tempvector <- as.numeric(tempvector)
tempterm <- sd(tempvector)
tempterm[is.na(tempterm)] <- 0
if (tempterm == 0 && tempvector[6] == 0) {tempvector <- c(0,10)} else {tempvector <- tempvector}
if (tempterm == 0) {
  tempvector2 <- abs(rnorm(n = as.numeric(length(tempvector)), mean = 1, sd = 2))
  tempvector <- tempvector*tempvector2
} else {tempvector <- tempvector} 
range <- tempvector
range <- append(range, runif(s, min = (min(tempvector)*0.2), max = (max(tempvector)*1.2)))
range[!is.na(range)]