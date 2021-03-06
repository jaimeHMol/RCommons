# -------------------------------------------------------------------------------------------------------------------
# Funci�n manejarNotificarError
# -------------------------------------------------------------------------------------------------------------------
# Funci�n llamada cuando un script de R falla. 
# Notifica del error a trav�s de pushbullet
#
# TODO: implementar manejo avanzado del error, como registro de hora de finalizaci�n, 
# cerrar sesi�n, apagar m�quina, etc
#
# Creado en Octubre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definici�n de funci�n
# ===================================================================================================================

manejarNotificarError <- function(){
  
  # -------------------------
  # Invocar funciones propias
  # -------------------------
  source('E:\\0. Dropbox\\Maestr�a Data Mining\\R\\Funciones\\enviarNotificacion.R')
  
  # -----------------
  # Proceso principal
  # -----------------  
  enviarNotificacion(titulo = "Ejecuci�n R", 
                     mensaje = paste("El programa",programa,"present� un error el d�a",Sys.time(),"\nError:", geterrmessage()), 
                     severidad = "KO")
}

