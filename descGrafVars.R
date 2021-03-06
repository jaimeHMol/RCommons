# -------------------------------------------------------------------------------------------------------------------
# Funci�n descGrafVars
# -------------------------------------------------------------------------------------------------------------------
# Descripci�n gr�fica de las variables que sean ingresadas.
# Genera un mosaico por cada tipo de gr�fico:
#     * Boxplot
#     * Histograma (gr�fico de barras para variables categ�ricas)
# Genera grafico con la relaci�n entre variables de entrada (library(PerformanceAnalytics) char.Correlation)
# que incluye
#     * Matriz de correlaciones
#     * Funci�n de densidad aproximada a partir de histograma
#     * Gr�fico de correlaci�n entre cada combinaci�n de las variables de entrada
# Entrada: Variables (columnas) a describir
# Salida: Gr�ficos
#
# Restricciones: Solo permite dividir el mosaico de gr�ficas en una matriz de .....
# Creado en Septiembre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definici�n de funci�n
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
  #Se puede hacer tambi�n con require(libreria)
  
  
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
    stop("Valor m�ximo de filas para el mosaico de gr�ficos 8 y m�nimo 1")
  }
  
  dfSourceFun <- as.data.frame(dfSourceFun)

  # Calcula el n�mero de columnas que va a tener el mosaico de gr�ficos en funci�n del n�mero de filas
  # definido y de la cantidad de variables que reciba la funci�n, redondeando siempre al entero mayor
  # (para los casos en que no es posible tener un n�mero de divisiones exacto)
  cantCol <- ceiling(length(dfSourceFun) / cantFil)
  
  
  if (maximizar){

    # --Genera histogramas
    # Genera una nueva ventana para mostrar las gr�ficas
    windows(xpos = 1, ypos = 1)
    # Divide el lienzo donde se va a graficar
    par(mfrow=c(cantFil,cantCol))
    for(i in 1:ncol(dfSourceFun)){
      hist(dfSourceFun[,i], main = paste("Histograma de",colnames(dfSourceFun[i])), xlab = NULL )
    }
    
    # ---Genera box plots
    # Genera una nueva ventana para mostrar las gr�ficas
    windows(xpos = 50, ypos = 50)
    # Divide el lienzo donde se va a graficar
    par(mfrow=c(cantFil,cantCol))
    for(i in 1:ncol(dfSourceFun)){
      boxplot(dfSourceFun[,i], main = paste("Boxplot de",colnames(dfSourceFun[i])) )
    }
    
    # ---Genera gr�fico de correlaci�n
    # Genera una nueva ventana para mostrar las gr�ficas
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
    
    # ---Genera gr�fico de correlaci�n
    chart.Correlation(dfSourceFun)
  }
  
  
  # Finaliza dejando el layout de gr�ficos del tama�o normal
  par(mfrow=c(1,1))
}

