tempvector <- prettyNum(tempvector, digits=4)
tempvector[is.na(tempvector)] <- 0   
tempvector <- as.numeric(tempvector)

tempterm <- sd(tempvector)
tempterm[is.na(tempterm)] <- 0
if (tempterm == 0 && max(tempvector) == 0) {tempvector <- c(0,10)} else {tempvector <- tempvector}
if (tempterm == 0) {tempvector2 <- c(1,2,3,4,5,6)
  tempvector <- tempvector / tempvector2} else {tempvector <- tempvector}
tempterm <- (max(tempvector)*1.2*1/s)
tempvector2 <- seq(from = min(tempvector)*0.2, to = max(tempvector)*1.8, by = tempterm)
range <- append(tempvector, tempvector2)
range[is.na(range)] <- 0

range <- prettyNum(range, digits=4)
#print(paste(range))