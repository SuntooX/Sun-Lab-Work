library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
inputdata <- read_xlsx("inputdata.xlsx", sheet = "BMS")

randomizer = "range.randomizer_2.R"
x <- pull(inputdata,1)
y <- pull(inputdata,2)
ActiveName <- as.vector(colnames(inputdata[,2]))

testdata <- as.data.frame(cbind(x,y))

{#Biphasic Model Regression Calculations ------------------------
  #inputs
  activefunction <- function(x) {1-a*x/(b+x) -(1-a)*x/(c+x)}
  biphasic.function <- activefunction
  z <- c(0:10)  #iterations
  s <- 5       #Number of subdivisions to do during regression
  a <- c(0,1)  #F1 term
  b <- c(0,10) #Kd1
  c <- c(0,10) #Kd2
  d <- NA #currently the offset for when Imax is reached
}
source(file = "dirtyregression.R", local = TRUE)

{#outputs
  biphasic.results <- results[order(results$RMSEscore), ] [1,]
  a <- as.numeric(biphasic.results$a)
  b <- as.numeric(biphasic.results$b)
  c <- as.numeric(biphasic.results$c)
  RMSE <- as.numeric(biphasic.results$RMSEscore)
}
{#Calculate IC50
  tempterm <- 0.5*(2*a*b - b - 2*a*c + c + sqrt((-2*a*b + b + 2*a*c - c)^2 + 4*b*c))
  tempterm2 <- 0.5*(2*a*b - b - 2*a*c + c - sqrt((-2*a*b + b + 2*a*c - c)^2 + 4*b*c))
  IC50 <- max(tempterm, tempterm2)
}

n <- max(length(x), length(y), length(tempvector))
length(x) <- n                      
length(y) <- n
length(tempvector) <- n

tempdata <- tempvector
tempdata <- cbind(tempdata, x)
tempdata <- as.data.frame(cbind(tempdata, y))
tempterm <- paste("~/Desktop/plots/", ActiveName, "_Data.csv", sep = "")

write.csv(tempdata, file = tempterm)

#Graph Function
temptext <- paste(ActiveName, "Biphasic Minimalization", sep = " ")
biphasic.results <- paste("BiphasicModel: ", "F1=", round(a, digits = 4) , ", Kd1=", round(b, digits = 4), ", Kd2=", round(c, digits = 4), " Offset=", ", RMSE=", round(RMSE, digits = 4), ", IC50=", round(IC50, digits = 4), sep = "")

x <- pull(inputdata,1)
y <- pull(inputdata,2)
testdata <- as.data.frame(cbind(x,y))

tempterm <- max(max(x), IC50)
ggplot(data = testdata, aes(x = x, y = y)) +
  geom_point()+
  geom_function(fun = biphasic.function)+ 
  scale_x_continuous(trans = "log10")+
  labs(title = temptext, x = "lnCONC", y = "Relative Viability") +
  annotate(geom = "text", x = 1 ,  y=1.5, label = str_wrap(biphasic.results, 50))+
  ylim(0, 1.5)+
  ggsave(plot = last_plot(), filename = paste(ActiveName, "Biphasic_Minimalization.png", sep = ""), width = 10, height = 10, device = NULL)

all_results <- read_xlsx("all_results.xlsx")
tempvector <- c(ActiveName, "Biphasic Model", a, b, c, RMSEscore, IC50)
dirtyregressionresults <- rbind(dirtyregressionresults, tempvector)
colnames(dirtyregressionresults) <- c("ActiveName", "Model", "F#1", "Kd1", "Kd2", "RMSE", "IC50")


