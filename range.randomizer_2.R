range.input <- prettyNum(range.input, digits=4)
range.input[is.na(range.input)] <- 0   
range.input <- as.numeric(range.input)

range.sd <- sd(range.input)
range.sd[is.na(range.sd)] <- 0
if (max(range.input) == 0) {range.input <- rnorm(2,10,3)} else {}
if (range.sd == 0) {vector.multiplier <- c(1,2,3,4,5,6)
    range.input <- range.input / vector.multiplier} else {}
range.expanded<- range.input * rnorm(1,1,0.1)
randomizer.value <- (max(range.expanded)*1.2*1/s)
range.expanded <- seq(from = min(range.expanded)*0.2, to = max(range.expanded)*1.8, by = randomizer.value)
range <- append(range.input, range.expanded)
range[is.na(range)] <- 0

range <- prettyNum(range, digits=4)
#print(paste(range))