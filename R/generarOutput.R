#' @Title Generar Ouput
#'
#' @param output 
#' @param config 
#' @param path 
#'
#' @return prediccion.csv
#' @return modelo.rds
#' @export
#'
#' @examples
#' @author María


# Función para general los outputs
generarOutput <- function(output, config, path){
  
  #Ponemos una marca de tiemnpo para saber cuando se ha guardado el modelo,
  # además, la fecha y hora le da un nombre único al archivo
  marcaTmp <- Sys.time()
  
  
  # Esta parte guarda la predicción el la carpeta output en modo csv
  nombreArchivo <- paste0(path, "output/prediccion.csv")
  
  tryCatch(expr = {
    
    write.csv(output$prediccion, file = nombreArchivo, sep = config$input$sep,
              row.names = FALSE)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado de la predicción!!", logger = 'log')
  })
  
  
  # Esta parte guarda el modelo el la carpeta output en modo rds
  nombreArchivo <- paste0(path, "output/modelo.rds")
  
  tryCatch(expr = {
    
    saveRDS(output$modelo, file = nombreArchivo)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado del modelo!!", logger = 'log')
    
  })
  
}