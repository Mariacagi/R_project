
# Lista de librerías que importar (requiere es lo mismo que library)
# character.only = TRUE significa que lo aplica solo a los vectores que son string
lapply(c("dummies", "logging", "xgboost", "XML"), require, character.only = TRUE)

# Para saber la carpeta en la que estas en el path. 
  # En este caso ya estamos en la carpeta madre, R_project, por tanto no es necesario subir a otra carpeta.
path <- getwd()


#List files nos devuelve los archivos dentro del directorio.
lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)


#DEBUG LINEA POR LINEA
  #si no quiero ir linea por linea añadir browser()
debug(funcionPrincipal)

# Llamamos a la funcion para ejecutar el programa
funcionPrincipal(path)

#SALIR DEL DEBUG
undebug(funcionPrincipal)