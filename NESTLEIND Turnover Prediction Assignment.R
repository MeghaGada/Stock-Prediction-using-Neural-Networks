require(neuralnet)
library(neuralnet)

df <- read.csv("C:\\Users\\megha\\OneDrive\\Desktop\\NESTLEIND.csv")
str(df)
scaleddata <- scale(df)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindf <- as.data.frame(lapply(df,normalize))  

trainset<-maxmindf[1:150,]
testset<-maxmindf[151:200,]

library(neuralnet)
nn<- neuralnet(Turnover~Prev.Close+Open+High+Low+Last+Close+VWAP+Volume+Deliverable.Volume,data=trainset,hidden=c(2,1),linear.output=FALSE,threshold=0.01)
nn$result.matrix
plot(nn)

nn$result.matrix
temp_test <- subset(testset,select = 
                      c("Prev.Close","Open","High","Low","Last","Close","VWAP","Volume","Turnover","Deliverable.Volume"))
head(temp_test)
nn.results <- compute(nn,temp_test)
results <- data.frame(actual=testset$Turnover,prediction=nn.results$net.result)

results 

