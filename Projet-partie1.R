library(party)
library(Metrics)

scoring <- function(N){
  set.seed(1234)
  irisSample <- iris[sample(1:N),]
  dataIndex <- sample( 2, nrow(irisSample) , replace = TRUE, prob = c( 0.7, 0.3) )
  train.data <- irisSample[dataIndex==1, ]
  test.data <- irisSample[dataIndex==2, ]
  
  formule <- Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
  irisDT <- ctree(formule, data=train.data)
  print(irisDT)
  plot(irisDT)
  
  trainPrediction <- predict(irisDT)
  print(table(trainPrediction, train.data$Species))
  testPrediction <- predict(irisDT, newdata=test.data)
  print(table(testPrediction,test.data$Species))
  
  mae<-0
  for(i in 1:9){
   mae<-table(trainPrediction, train.data$Species)[i]+mae
  }
  mae<-mae-table(trainPrediction, train.data$Species)[1]-table(trainPrediction, train.data$Species)[5]-table(trainPrediction, train.data$Species)[9]
  print(mae)
  
}


scoring(120)