#' @title funcionPrincipal
#' @description Funcion principal del paquete de prediccionDatos
#'
#' @param path, string
#'
#' @export
#' @import logging
#'
#' @author María


# Para que funcione nuestro path lo igualamos a getwd para que recoja la carpeta madre R_project
path <- getwd()

prediccionDatos <- function(path){

  # Hacemos un tryCatch para ver si hay error, en qué funcion.
  
  tryCatch(expr = {
    library(logging)
    
    # addHandler necesario para generar el controlador y donde guardar los errores que transmita log
      #Tipo de escritura (WriteToFile, funcion predefinida), como se llama el controlador (log) y donde lo escribe.
    addHandler(writeToFile, logger = 'log', file = paste0(path, "/log/logfile.log"))
    
    loginfo("Empezamos la app...", logger = 'log')
    
    loginfo("Leyendo el Config...", logger = 'log')
    config <- leerConfig(path)
    loginfo("Config leido.", logger = 'log')
    
    
    loginfo("Leyendo los datos...", logger = 'log')
    datos <- leerDatos(config, path)
    loginfo("Datos leidos.", logger = 'log')
    
    loginfo("Procesando los datos...", logger = 'log')
    splitDatos <- preProcesarDatos(datos, config)
    loginfo("Datos procesados.", logger = 'log')
    
    loginfo("Generando modelo...", logger = 'log')
    output <- generarModelo(splitDatos, config)
    loginfo("Modelo generado.", logger = 'log')
    
    loginfo("Generando output...", logger = 'log')
    generarOutput(output, config, path)
    loginfo("Output generado.", logger = 'log')
    
    
  }, error = function(e){
    
    logerror("La aplicación ha petado...", logger = 'log')
    
    stop()
    
  }, finally = {
    loginfo("Fin de la ejecucion.", logger = 'log')
    removeHandler(writeToFile, logger = 'log')
  })
  
}