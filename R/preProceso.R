library(tidyverse)

preproceso <- function(data, config){
  #select countries from dataframes
  countries <- c()
  for (c in config$countries){
    countries <- c(countries, c)
    print(c)
  }
  print('countries')
  for (i in 1:length(data)){
   data[[i]] <- data[[i]][data[[i]]['country']==countries,]
   print('i')
  }
  
  #turn dataframes from wide format to long format
  print('reshape')
  for (i in 1:length(data)){
    #data[[i]] <- reshape(data[[i]], idvar='country', varying=-1, direction="long", sep="")
    data[[i]] <- setNames(as.data.frame.table(as.matrix(data[[i]][-1])),c("Country","Year",config$input$csvs[i]))
    #names(result) <- c("Country","Year", config$input$csvs[i])
    print(i)
  }
  
  #merging reshaped dataframes to form final one
  print('merging')
  final_df <- data[[1]]
  for (i in 2:length(data)){
    final_df <- merge(final_df, data[[i]], by ="Country", all= TRUE)
    print(i)
  }
  return (final_df)
}

###### Para probar formula ########

#library(XML)

configPath <- paste0("D:/Users/Juan/Documents/TheBridge/Git/R_project/",
                     "config/config.xml")


#library("methods")
result <- xmlParse(file = configPath)
config <- xmlToList(result, addAttributes = TRUE, simplify = FALSE)

#csvs
murdered_women <- read.csv(file = 'D:/Users/Juan/Documents/TheBridge/Git/R_project/R/murdered_women_per_100000_people.csv')
population <- read.csv(file = 'D:/Users/Juan/Documents/TheBridge/Git/R_project/R/population_total.csv')

data <- list(murdered_women, population)

#Reshaping options, both work here but reshape formula doesn't in the function
# with reshape method the countries appear by name
setNames(as.data.frame.table(as.matrix(population)),c("Country","Year", 'population'))
reshape(data[[2]], idvar="country", varying=-1, direction="long", sep="")

######   Prueba  ######

x <- preproceso(data, config)
dim(x)
head(x)
