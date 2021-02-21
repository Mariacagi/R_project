#' Title
#'
#' @param df 
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
generateModel <- function(df, config){

  library(xgboost)
  library(caret)
  
  set.seed(42)
  
  # Creating train/test data partition
  indexes <- createDataPartition(df[[config$target]], p=.85, list=FALSE)
  dfTrain <- df[indexes, ]
  dfTest <- df[-indexes, ]
  
  # Separating target/data
  xTrain <- data.matrix(dfTrain[, -grep(config$target, colnames(df))])
  yTrain <- dfTrain[, grep(config$target, colnames(df))]
    
  xTest <- data.matrix(dfTest[, -grep(config$target, colnames(df))])
  yTest <- dfTest[, grep(config$target, colnames(df))]
  
  # Creating Matrix Data
  xgbTrain <- xgb.DMatrix(data=xTrain, label=yTrain)
  xgbTest <- xgb.DMatrix(data=xTest, label=yTest)
  
  # Fit model
  xgbc <- xgboost(data = xgbTrain, max.depth = 2, nrounds = 50)
  return(xgbc)
}



predictValue <- function(df, model, config){
  toPred <- data.matrix(dfTrain[strtoi(config$country), -grep(config$target, colnames(df))])
  
  yPred <- predict(model, xgbX)
  return(yPred)
}