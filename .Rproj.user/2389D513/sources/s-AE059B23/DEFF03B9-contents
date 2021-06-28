# Generate Starting Sequences from Ranges
a <- append(a, runif(s, min(a), max(a)))
b <- append(b, runif(s, min(b), max(b)))
c <- append(c, runif(s, min(c), max(c)))
#d <- append(d, runif(s, min(d), max(d)))
all.runs <- transpose(as.data.frame(1:7))
colnames(all.runs) <- c("activename", "a","b","c","d","RMSEscore","Iterations")

# start of looping madness
for ( u in z) {
  u <- as.numeric(u)
  {  
  tempvector <- a
    source(file = "range.randomizer.R", local = TRUE)
  #a can not equal 0 nor 1
  a_list <- range[range > 0]
  a_list <- a_list[a_list < 1]
  
  tempvector <- b
    source(file = "range.randomizer.R", local = TRUE)
  #b can not equal 0
  b_list <- range[range > 0]
  b_list <- b_list[!b_list > 1]
  
  tempvector <- c
    source(file = "range.randomizer.R", local = TRUE)
  c_list <- range
  c_list[c_list < 0] <- c_list[c_list < 0]*-1 
#  tempvector <- d 
#    source(file = "range.randomizer.R", local = TRUE)
#  d <- sample(range, 12, replace = TRUE)
  }
  tempvector <- c()
  results <- transpose(as.data.frame(1:7))
  colnames (results) <- c("activename","a","b","c","d","RMSEscore","Iterations")
  for (a in a_list){
    for (b in b_list){
      for (c in c_list){
        #for (d in l){
          predict_y <- c()
          predict_y <- activefunction(x)
          predict_y[is.na(predict_y)] <- 0
          tempterm <- abs(RMSE(y, predict_y))
          RMSEscore <- abs(tempterm)
          tempvector <- c(ActiveName, a, b, c, d, RMSEscore, u)
          results <- rbind(results, tempvector)
          #}
        } 
     }
  }
  {
  results <- subset(results, !is.na(results$RMSEscore))
  results <- subset(results, !is.infinite(results$RMSEscore))
  all.runs <- rbind(all.runs, results)
  
  results <- results[order(results$RMSEscore), ] [1:6,]
  a <- as.vector(results$a)
  b <- as.vector(results$b)
  c <- as.vector(results$c)
  #d <- as.vector(results$d)
  RMSE <- as.vector(results$RMSEscore)
  }
}

all.runs <- rbind(all.runs, results)
results <- results[order(results$RMSEscore), ] [1:6,]

