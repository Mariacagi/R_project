#' Title
#'
#' @param df1 
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
generarModelo <- function(df1, config){

  library(xgboost)
  library(caret)
  
  set.seed(42)
  
  print("HOLA1")
  
  # Creating train/test data partition
  indexes <- createDataPartition(df1[,config$target], p=.85, list=FALSE)
  print("HOLA_CASI_2")
  df1Train <- df1[indexes, ]
  df1Test <- df1[-indexes, ]
  #validation_index <- createDataPartition(df1[, ncol(df1)], p=0.80, list=FALSE)
  #test_df1 <- df1[-validation_index,]
  #train_df1 <- df1[validation_index,]
  
  print("HOLA2")
  
  # Separating target/data
  xTrain <- data.matrix(df1Train[, -grep(config$target, colnames(df1))])
  yTrain <- df1Train[, grep(config$target, colnames(df1))]
    
  xTest <- data.matrix(df1Test[, -grep(config$target, colnames(df1))])
  yTest <- df1Test[, grep(config$target, colnames(df1))]
  
  print("HOLA3")
  
  # Creating Matrix Data
  xgbTrain <- xgb.DMatrix(data=xTrain, label=yTrain)
  xgbTest <- xgb.DMatrix(data=xTest, label=yTest)
  
  print("HOLA4")
  
  # Fit model
  xgbc <- xgboost(data = xgbTrain, max.depth = 2, nrounds = 50)
  
  # config$to_predict$country
  # config$to_predict$year
  
  to_predict <- df1[(df1["country"] == config$to_predict$country) & (df1["Year"] == config$to_predict$year), 1:5]
  
  my_predict <- predictValue(to_predict, xgbc, config)
  
  return(my_predict)
}

predictValue <- function(df1, model, config){
  #toPred <- data.matrix(df1[strtoi(config$country), -grep(config$target, colnames(df1))])
  
  yPred <- predict(model, df1)
  return(yPred)
}
