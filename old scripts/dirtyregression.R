#start of looping madness
for (u in z) {
  
  tempvector <- a
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(0,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
  tempvector <- c(1/2*tempvector[1], 2*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/s)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  #a can not equal 0 nor 1
  i <- range[range > 0]
  i <- i[i < 1]
  
  tempvector <- b
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(1/2*tempvector[1], 2*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/s)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  b <- range
  #b can not equal 0
  o <- range[range > 0 ]
  
  tempvector <- c
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(1/2*tempvector[1], 2*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/s)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  p <- range
  
  tempvector <- d 
  if (tempvector[1] == tempvector[2] && tempvector[1] == 0)
  {tempvector <- c(-1,1)} else {tempvector <- tempvector}
  if (tempvector[1] == tempvector[2]){
    tempvector <- c(1/2*tempvector[1], 2*tempvector[1])
  } else {tempvector <- tempvector}
  tempvector <- tempvector[order(tempvector)]
  subset <- ((tempvector[2]-tempvector[1])/s)
  range <- seq(from = tempvector[1], to = tempvector[2], by = subset)
  l <- range
  
  tempvector <- c()
  results <- transpose(as.data.frame(1:6))
  colnames(results) <- c("a","b","c","d","RMSEscore","Iterations")
  for (a in i){
    for (b in o){
      for (c in p){
        for (d in l){
          predict_y <- c()
          predict_y <- offset.activefunction(x)
          #predict_y[predict_y <= 0] <- 0
          if (max(predict_y) > 1) next else {
          RMSEscore <- RMSE(predict_y, testdata$y)
          tempvector <- c(a, b, c, d, RMSEscore, u)
          results <- rbind(results, tempvector)}
        }
      }
    }
  }
  tempterm <- results[order(results$RMSEscore), ] [1,]
  results <- results[order(results$RMSEscore), ] [4,]
  results <- rbind(results, tempterm)
  
  a <- as.vector(results$a)
  b <- as.vector(results$b)
  c <- as.vector(results$c)
  d <- as.vector(results$d)
  RMSE <- as.vector(results$RMSEscore)
}
