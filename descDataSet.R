# -------------------------------------------------------------------------------------------------------------------
# Función dscDataSet
# -------------------------------------------------------------------------------------------------------------------
# Describe cuantitativamente un dataSet y cada una de sus variables (columnas).
# No se tienen en cuenta los valores NA (nulos, missings)
# Cálcula las siguientes métricas descriptivas para todas las columnas del data frame de entrada
#     1.1. Posición dentro de la tabla (data frame)
#     1.2. Nombre de variable
#     1.3. Tipo de variable
#     1.4. Max
#     1.5. Min
#     1.6. Valor moda (Si hay más de un valor con la máxima frecuencia de aparición muestra Multi)
#     1.7. Cantidad de repeticiones de la moda
#     1.8. Promedio
#     1.9. Desviación estandar
#     1.10 Cantidad de NA (Valores nulos, missings)
#     1.11 Cantidad de valores distintos
# Entrada: Data frame con dataset a describir
# Salida: Data frame con metricas de resultado
#
# Además de contar con la desviación estandar (sd), se podría agregar var (sd^2) 
# y/o mad la alternativa más robusta a outliers.
#
# Creado en Septiembre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definición de función
# ===================================================================================================================
descDataSet <- function(dfSourceFun){
  
  options(warn = -1) # Para no mostrar en la consola los warnings que son esperables en la ejecución de esta función
  
  # ----------------------------------------------------------------------------------
  # Instalar paquetes (Solo se hace la primera vez que se va a utilizar una libreria)
  # ----------------------------------------------------------------------------------
  
  # ------------------
  # Invocar librerias
  # ------------------
  
  # --------------------
  # Funciones auxiliares
  # --------------------
  moda <- function(listValores) {
    moda <- data.frame(matrix(ncol = 2, nrow = 1))
    colnames(moda) <- c("val","cant")
    
    # Reemplaza los valores nulos con un string para que sean tenidos en cuenta
    listValores[is.na(listValores)] <- "NA"
    
    uniqueListValores <- table(listValores)
    max1 <- which.max(uniqueListValores)


    if (max(uniqueListValores) == max(uniqueListValores[-max1])){
      moda$val  <- "Multi"
      moda$cant <- max(uniqueListValores)
    } else {
      moda$val  <- names(max1)  
      moda$cant <- max(uniqueListValores)
    }
    return(moda)
  }
  
  
  # -----------------
  # Proceso principal
  # -----------------  
  dfSourceFun <- as.data.frame(dfSourceFun)
  dfType <- lapply(dfSourceFun, class)
  dfDesc <- data.frame(matrix(ncol = 11, nrow = ncol(dfSourceFun)))
  rows <- character(ncol(dfSourceFun))
  cols <- c("Ind", "Nombre", "Tipo", "Max", "Min", "ModaVal", "ModaCant", "Prom", "DesvStd", "NACant", "DistintCant")
  colnames(dfDesc) <- cols
  
  for (i in 1:ncol(dfSourceFun)){
    dfDesc[i,1]   <- i
    dfDesc[i,2]   <- colnames(dfSourceFun)[i]
    dfDesc[i,3]   <- dfType[i]
    if (is.factor(dfSourceFun[,i])){
      dfDesc[i,4] <- "NA"
      dfDesc[i,5] <- "NA"
    }else{
      dfDesc[i,4] <- max(dfSourceFun[,i], na.rm = TRUE)
      dfDesc[i,5] <- min(dfSourceFun[,i], na.rm = TRUE)
    }
    dfDesc[i,6]   <- moda(dfSourceFun[,i])$val
    dfDesc[i,7]   <- moda(dfSourceFun[,i])$cant
    dfDesc[i,8]   <- round(mean(unlist(dfSourceFun[,i]), na.rm = TRUE),2)
    dfDesc[i,9]   <- round(sd(unlist(dfSourceFun[,i]), na.rm = TRUE),2)
    dfDesc[i,10]  <- sum(is.na(dfSourceFun[,i]))
    dfSourceFunNA <- dfSourceFun[,i]
    dfSourceFunNA[is.na(dfSourceFunNA)] <- "NA"
    dfDesc[i,11]  <- length(table(dfSourceFunNA))
    
    rows[i]       <- colnames(dfSourceFun)[i]
  }
  
  rownames(dfDesc) <- rows
  
  dfDesc[ncol(dfSourceFun) + 1, ] <- "___"
  dfDesc[ncol(dfSourceFun) + 2,1] <- " "
  dfDesc[ncol(dfSourceFun) + 2,2] <- "TotalObs: "
  dfDesc[ncol(dfSourceFun) + 2,3] <- nrow(dfSourceFun)
  dfDesc[ncol(dfSourceFun) + 2,4:ncol(dfDesc)] <- " "
  
  dfDesc[ncol(dfSourceFun) + 3,1] <- " "
  dfDesc[ncol(dfSourceFun) + 3,2] <- "TotalVar: "
  dfDesc[ncol(dfSourceFun) + 3,3] <- ncol(dfSourceFun)
  dfDesc[ncol(dfSourceFun) + 3,4:ncol(dfDesc)] <- " "
  
  dfDesc[ncol(dfSourceFun) + 4,1] <- " "
  dfDesc[ncol(dfSourceFun) + 4,2] <- "TotalNul: "
  # Calcula cuantos nulos hay por fila en el data set de entrada 
  # y luego suma cuantos registros tiene cantidad de registros > 0:
  dfDesc[ncol(dfSourceFun) + 4,3] <- sum(rowSums(is.na(dfSourceFun)) > 0) 
  dfDesc[ncol(dfSourceFun) + 4,4:ncol(dfDesc)] <- " "
  dfDesc[ncol(dfSourceFun) + 5, ] <- "___"  
  
  
  print(dfDesc)
  options(warn = 0) # Para volver a mostrar warnings en consola
  return(dfDesc)
}
