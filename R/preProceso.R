#' Title
#'
#' @param datos 
#' @param config 
#' @import reshape2
#' @return
#' @export
#'
#' @examples

library(tidyverse)
library(reshape2)

preproceso <- function(data, config){
  #select countries from dataframes
  countries <- strsplit(config$countries,",")
  print('countries')
  
  for (i in 1:length(data)){
   data[[i]] <- data[[i]][data[[i]]['country']==countries[[1]],]
   print(i)
  }
  
  #turn dataframes from wide format to long format
  print('reshape')
  csv <- strsplit(config$input$csvs,",")
  for (i in 1:length(data)){
    name <- csv[[1]][i]
    name <- gsub(".csv","",name)
    data[[i]] = melt(data[[i]], id.vars='country', variable.name = 'Year',value.name = name, factorsAsStrings = FALSE)
    print(i)
  }
  
  #merging reshaped dataframes to form final one
  print('merging')
  final_df <- Reduce(function(d1, d2) merge(d1, d2, by = c('country','Year'), all.x = TRUE),  data)
  final_df$Year <- gsub("X","",final_df$Year)
  return (final_df)
}

