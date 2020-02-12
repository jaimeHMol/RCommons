# -------------------------------------------------------------------------------------------------------------------
# Función Template
# -------------------------------------------------------------------------------------------------------------------
# Plantilla con la estructura básica para una función en R
# Creado en Septiembre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definición de función
# ===================================================================================================================

nombreFuncionXXXX <- function(paramEntrada1XXX, paramEntrada2XXX){
  
  # ----------------------------------------------------------------------------------
  # Instalar paquetes (Solo se hace la primera vez que se va a utilizar una libreria)
  # ----------------------------------------------------------------------------------
  #install.packages('caret')
  #install.packages('ggplot2')

  
  # ------------------
  # Invocar librerias
  # ------------------
  library(caret)
  library(ggplot2)
  #Se puede hacer también con require(libreria)
  
  # ----------------------------------------
  # Funciones auxiliares (si son necesarias)
  # ----------------------------------------
  nombreFuncionAUXXXX <- function(paramEntrada1XXX, paramEntrada2XXX) {

    
    return(paramSalidaXXX)
  }
  
  # -----------------
  # Proceso principal
  # -----------------  
  # Si la salida es un data frame con multiples atributos
  dfSalidaXXX <- data.frame(matrix(ncol = 3, nrow = 0))
  cols <- c("Atrib1", "Atrib2", "Atrib3")
  colnames(dfSalidaXXX) <- cols
  
  # Poblar data frame
  
  
  
  dfSalidaXXX
  return(dfSalidaXXX)
}

