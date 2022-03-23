require(neuralnet)
library(neuralnet)

df <- read.csv("C:\\Users\\megha\\OneDrive\\Desktop\\CIPLA.csv")
str(df)
scaleddata <- scale(df)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindf <- as.data.frame(lapply(df,normalize))  

trainset<-maxmindf[1:150,]
testset<-maxmindf[151:200,]

library(neuralnet)
nn<- neuralnet(Turnover~Prev.Close+Open+High+Low+Last+Close+VWAP+Volume,data=trainset,hidden=c(2,1),linear.output=FALSE,threshold=0.01)
nn$result.matrix
plot(nn)

nn$result.matrix
temp_test <- subset(testset,select = 
                      c("Prev.Close","Open","High","Low","Last","Close","VWAP","Volume","Turnover"))
head(temp_test)
nn.results <- compute(nn,temp_test)
results <- data.frame(actual=testset$Turnover,prediction=nn.results$net.result)

results 

nn7 = neuralnet(Turnover~Prev.Close+Open+High+Low+Last+Close+VWAP+Volume,data=trainset, rep = 5, hidden=c(3,1),act.fct="logistic",linear.output = FALSE)
plot(nn7)

nn_backprop2 <- neuralnet(Turnover~Prev.Close+Open+High+Low+Last+Close+VWAP+Volume,data=trainset, rep = 3,hidden = c(3,1),algorithm="backprop",learningrate=0.0001)
plot(nn_backprop2)



#Visualizations
a <- head(sort(table(df$High), decreasing=TRUE))
a
barplot(a, xlab="High", ylab = "Low", col="lightgreen" )


b <- head(sort(table(df$Open), decreasing=TRUE))
b
hist(b, xlab="Open", ylab = "Close", col="coral" )


