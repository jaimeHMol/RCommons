# -------------------------------------------------------------------------------------------------------------------
# Función descGrafVars
# -------------------------------------------------------------------------------------------------------------------
# Descripción gráfica de las variables que sean ingresadas.
# Genera un mosaico por cada tipo de gráfico:
#     * Boxplot
#     * Histograma (gráfico de barras para variables categóricas)
# Genera grafico con la relación entre variables de entrada (library(PerformanceAnalytics) char.Correlation)
# que incluye
#     * Matriz de correlaciones
#     * Función de densidad aproximada a partir de histograma
#     * Gráfico de correlación entre cada combinación de las variables de entrada
# Entrada: Variables (columnas) a describir
# Salida: Gráficos
#
# Restricciones: Solo permite dividir el mosaico de gráficas en una matriz de .....
# Creado en Septiembre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definición de función
# ===================================================================================================================

descGrafVars <- function(dfSourceFun, cantFil, maximizar=c("TRUE","FALSE")){
  
  # ----------------------------------------------------------------------------------
  # Instalar paquetes (Solo se hace la primera vez que se va a utilizar una libreria)
  # ----------------------------------------------------------------------------------
  #install.packages('PerformanceAnalytics')
  #install.packages('ggplot2')

  
  # ------------------
  # Invocar librerias
  # ------------------
  library(PerformanceAnalytics)
  #library(ggplot2)
  #Se puede hacer también con require(libreria)
  
  
  # -----------------
  # Proceso principal
  # -----------------
  # Valida variables de entrada opcionales
  maximizar <- paste(maximizar)
  maximizar <- match.arg(maximizar)
  if (missing(cantFil)){
    cantFil <- 2
  }
  if (cantFil <= 0 | cantFil > 7){
    stop("Valor máximo de filas para el mosaico de gráficos 8 y mínimo 1")
  }
  
  dfSourceFun <- as.data.frame(dfSourceFun)

  # Calcula el número de columnas que va a tener el mosaico de gráficos en función del número de filas
  # definido y de la cantidad de variables que reciba la función, redondeando siempre al entero mayor
  # (para los casos en que no es posible tener un número de divisiones exacto)
  cantCol <- ceiling(length(dfSourceFun) / cantFil)
  
  
  if (maximizar){

    # --Genera histogramas
    # Genera una nueva ventana para mostrar las gráficas
    windows(xpos = 1, ypos = 1)
    # Divide el lienzo donde se va a graficar
    par(mfrow=c(cantFil,cantCol))
    for(i in 1:ncol(dfSourceFun)){
      hist(dfSourceFun[,i], main = paste("Histograma de",colnames(dfSourceFun[i])), xlab = NULL )
    }
    
    # ---Genera box plots
    # Genera una nueva ventana para mostrar las gráficas
    windows(xpos = 50, ypos = 50)
    # Divide el lienzo donde se va a graficar
    par(mfrow=c(cantFil,cantCol))
    for(i in 1:ncol(dfSourceFun)){
      boxplot(dfSourceFun[,i], main = paste("Boxplot de",colnames(dfSourceFun[i])) )
    }
    
    # ---Genera gráfico de correlación
    # Genera una nueva ventana para mostrar las gráficas
    windows(xpos = -1, ypos = 1)
    chart.Correlation(dfSourceFun)
    
  }else{

    # --Genera histogramas
    # Divide el lienzo donde se va a graficar
    par(mfrow=c(cantFil,cantCol))
    for(i in 1:ncol(dfSourceFun)){
      hist(dfSourceFun[,i], main = paste("Histograma de",colnames(dfSourceFun[i])), xlab = NULL )
    }
    
    # ---Genera box plots
    # Divide el lienzo donde se va a graficar
    par(mfrow=c(cantFil,cantCol))
    for(i in 1:ncol(dfSourceFun)){
      boxplot(dfSourceFun[,i], main = paste("Boxplot de",colnames(dfSourceFun[i])) )
    }
    
    # ---Genera gráfico de correlación
    chart.Correlation(dfSourceFun)
  }
  
  
  # Finaliza dejando el layout de gráficos del tamaño normal
  par(mfrow=c(1,1))
}

