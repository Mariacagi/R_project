#' Title
#'
#' @param config 
#' @param path 
#'
#' @return
#' @export
#'
#' @examples

#Funcion para leer csv
leerDatos <- function(config, path){

#pathDatos hasta el csv: añado "data/" al path y el nombre del csv de la etiqueta name del xml
  pathDatos <- paste0(path, "data/", config$input$name)
  
  browser()

  
  tryCatch(expr = {

#Paquete data.table. Funcion fread para leer el csv con pathDatos, sep(etiqueta del xml).
#data.table=False para que genere un dataframe. Lo guarda como 'datos'
    filenames <- c()
    
    for (i in pathDatos){
      
      filename <- strsplit(basename(i),'.csv')
      
      filenames <- c(filenames, filename)
    }
    
    list_df <- c()
    
    for (i in pathDatos){
      datos <-data.table::fread(pathDatos, sep = config$input$sep, encoding = 'UTF-8',
                                data.table = FALSE)

      list_df[[length(list_df) + 1]] <- datos}
    
    names(list_df) <- filenames
    
    
   
    
  }, error = function(e){

#log de errores   
    logerror("Datos no encontrados en su ruta. Verifica el archivo datos",
             logger = 'log')
    stop()
  })
  #si las filas o columnas del dataframe 'datos' estan vacias generar error de datos mal leidos
  if (nrow(datos) == 0 | ncol(datos) == 0){
    
    logerror("Datos mal leidos, verifica que tengan el formato correcto.",
             logger = 'log')
    stop()
    
  }
#si no hay errores, devolver el dataframe 'datos'  
  return(datos)
  
}


