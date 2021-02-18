#' modelML.R
#'
#' @param datos 
#' @param config 
#'
#' @import xgboost
#' @import caret
#' @param datosSplit 
#' @description blablabla
#'
#' @param config 
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


#' Title
#'
#' @param model 
#' @param X 
#'
#' @return
#' @export
#'
#' @examples
predictValues <- function(model, X){
  xgbX <- xgb.DMatrix(data=X)
  
  yPred <- predict(model, xgbX)
  return(yPred)
}

df <- MASS::Boston
model <- generateModel(df, df[[config$target]], config$target)

prediction(model, xTest)

df$config$target

config$target = "columna"


clumna <- config[$target
df$clumna][]