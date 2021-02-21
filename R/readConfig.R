#' Title
#'
#' @return config list
#' 
#' @import XML
#' @import methods
#'
#' @examples
leerConfig <- function(){
  
  library(XML)
  
  configPath <- paste0(getwd(), "config/config.xml")
  
  tryCatch(expr = {
                    library("methods")
                    result <- xmlParse(file = configPath)
                    config <- xmlToList(result, addAttributes = TRUE, simplify = FALSE)
    
                  }, error = function(e){
                    
                    logerror("File not found. Config file must be named 'config.xml'", logger = 'log')
                    stop()
                  }
           )
  
  loginfo("Config file readed", logger = 'log')
  
  validateConfig(config)
  
  loginfo("Config file checked", logger = 'log')
  
  config$input$csvs <- trimws(strsplit(config$input$csvs, ",")[[1]])
  config$countries <- trimws(strsplit(config$countries, ",")[[1]])
  
  return(config)
}


#' ValidateConfig
#'
#' @param config 
#'
#' @examples
validateConfig <- function(config){
  
  configNode <- identical(names(config), c("input", "countries", "years", "target", "to_predict"))
  inputNode <- identical(names(config$input), c("csvs", "sep"))
  yearsNode <- identical(names(config$columnas$fechas), c("start", "end"))
  predictNode <- identical(names(config$columnas$mails), c("country", "year"))  
  
  nodes <- c("configNode" = configNode, "inputNode" = inputNode, "yearsNode" = yearsNode, 
             "predictNode" = predictNode)
  
  check <- all(nodes)
  
  if(!check){
    
    incorrectNodes <- names(nodes)[!nodes]
    
    logerror(paste0("The following nodes:, ",paste(incorrectNodes, collapse = ",") , " are incorrect"),
             logger = 'log')
    
    stop()
  }
}