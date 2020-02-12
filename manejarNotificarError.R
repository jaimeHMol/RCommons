# -------------------------------------------------------------------------------------------------------------------
# Función manejarNotificarError
# -------------------------------------------------------------------------------------------------------------------
# Función llamada cuando un script de R falla. 
# Notifica del error a través de pushbullet
#
# TODO: implementar manejo avanzado del error, como registro de hora de finalización, 
# cerrar sesión, apagar máquina, etc
#
# Creado en Octubre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definición de función
# ===================================================================================================================

manejarNotificarError <- function(){
  
  # -------------------------
  # Invocar funciones propias
  # -------------------------
  source('E:\\0. Dropbox\\Maestría Data Mining\\R\\Funciones\\enviarNotificacion.R')
  
  # -----------------
  # Proceso principal
  # -----------------  
  enviarNotificacion(titulo = "Ejecución R", 
                     mensaje = paste("El programa",programa,"presentó un error el día",Sys.time(),"\nError:", geterrmessage()), 
                     severidad = "KO")
}

