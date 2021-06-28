# Generate Starting Sequences from Ranges
all.runs <- as.data.frame(transpose(as.data.frame(1:6)))
colnames(all.runs) <- c("activename", "a","b","c","RMSE","Iterations")

# start of looping madness
for (u in z) {
  u <- as.numeric(u)
  {  
    range.input <- a
    source(file = paste(randomizer), local = TRUE)
    #a can not equal 0 nor 1
    a_list <- as.numeric(range[range > 0])
    a_list <- a_list[a_list < 1]
    
    range.input <- b
    source(file = paste(randomizer), local = TRUE)
    #b can not equal 0
    b_list <- as.numeric(range[range > 0])

    range.input <- c
    source(file = paste(randomizer), local = TRUE)
    c_list <- as.numeric(range[range > 0]) 
    c_list <- as.vector(c_list)
    
  }
  results <- as.data.frame(transpose(as.data.frame(1:6)))
  colnames(results) <- c("activename","a","b","c","RMSE","Iterations")
  for (a in a_list){"range.randomizer.R"
    if (a >= max(x)) {a <- rnorm(1, mean = mean(x/10), 1)} else {}
    if (a <= min(x)/10) {a <- rnorm(1, mean = mean(x/10), 1)} else {}
    a <- as.numeric(prettyNum(a, digits=4))
    for (b in b_list){
      if (b >= max(x)) {b <- rnorm(1, mean = mean(x), 1)} else {}
      if (b <= min(x)/10) {b <- rnorm(1, mean = mean(x), 1)} else {}
      b <- as.numeric(prettyNum(b, digits=4))
      for (c in c_list){
        if (c >= max(x)) {c <- rnorm(1, mean = mean(x), 1)} else {}
        if (c <= min(x)/10) {c <- rnorm(1, mean = mean(x), 1)} else {}
        c <- as.numeric(prettyNum(c, digits=4))
        predicted <- c() 
        predicted <- activefunction(x)
        predicted[is.na(predicted)] <- 0
        RMSE <- prettyNum(rmse(y, predicted), digits = 4)
        tempvector3 <- c(ActiveName, a, b, c, RMSE, u)
        results <- rbind(results, tempvector3)
        results <- results[order(results$RMSE), ]
        results <- results[!duplicated(results[,c("activename", "a")]),]
      } 
    }
  }
  {
    results <- subset(results, !is.na(results$RMSE))
    results <- subset(results, !is.infinite(results$RMSE))
    results <- distinct(results, .keep_all = FALSE)
    all.runs <- rbind(all.runs, results)
    
    range.best.six <- results[order(results$RMSE), ][1:6,]
    a <- as.vector(range.best.six$a)
    b <- as.vector(range.best.six$b)
    c <- as.vector(range.best.six$c)
    RMSE <- as.vector(range.best.six$RMSE)
  }
}

all.runs <- rbind(all.runs, results)