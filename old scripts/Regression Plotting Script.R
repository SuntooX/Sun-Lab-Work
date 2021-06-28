#start of looping madness
for (y in z) {
  
  tempvector <- a
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(.9*tempvector[1], 1.1*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/4)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  #a can not equal 0
  a <- range[range >= 0]
  
  tempvector <- b
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(.9*tempvector[1], 1.1*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/4)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  b <- range
  #b can not equal 0
  b <- range[range >= 0]
  
  tempvector <- c
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(.9*tempvector[1], 1.1*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/4)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  c <- range
  
  tempvector <- d
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(.9*tempvector[1], 1.1*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/4)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  d <- range
  
  tempvector <- c()
  results <- transpose(as.data.frame(1:6))
  colnames(results) <- c("a","b","c","d","RMSEscore","Iterations")
  for (i in a){
    for (j in b){
      for (k in c){
        for (l in d){
          predict_y <- c()
          predict_y <- i*(x-l)/(j+(x-l))+(1+i)*(x-l)/(k+(x-l))
          predict_y[predict_y <= 0] <- 0
          RMSEscore <- RMSE(predict_y, testdata$y, na.rm=TRUE)
          tempvector <- c(i, j, k, l, RMSEscore, y)
          results <- rbind(results, tempvector)
        }
      }
    }
  }
  
  results <- results[order(results$RMSEscore), ] [1:2,]
  a <- as.vector(results$a)
  b <- as.vector(results$b)
  c <- as.vector(results$c)
  d <- as.vector(results$d)
  RMSE <- as.vector(results$RMSEscore)
}
