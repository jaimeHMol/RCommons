# -------------------------------------------------------------------------------------------------------------------
# Funci�n enviarNotificacion
# -------------------------------------------------------------------------------------------------------------------
# Funci�n que env�a una notificaci�n a todos los dispositivos asociados (celular, tablet o computador externo)
# usando Push Bullet
#
# Se puede mejorar: obteniendo la lista de dispositivos asociados y seleccionar un dispositivo en particular
# para notificar
# 
# Creado en Octubre de 2017
# @kingSelta
# -------------------------------------------------------------------------------------------------------------------

# ===================================================================================================================
# Definici�n de funci�n
# ===================================================================================================================

enviarNotificacion <- function(titulo, mensaje, severidad = c("INFO","OK","KO")){
  
  # ----------------------------------------------------------------------------------
  # Instalar paquetes (Solo se hace la primera vez que se va a utilizar una libreria)
  # ----------------------------------------------------------------------------------
  #install.packages("RPushbullet")

  # Escribe el archivo inicial .rpushbullet necesario en la ruta requerida
  # el valor del key debe contener el token de acceso generado en la web de pushbullet (Settings -> Account)
  write("{\n\t\"key\":\"o.L6j15F84S0c7hc6iKyLFMLvYqIMasJZ6\",\n\t\"devices\":[]\n}", 
        paste(Sys.getenv("HOME"), ".rpushbullet.json", sep="/") )  
  
  # ------------------
  # Invocar librerias
  # ------------------
  library(RPushbullet)
  #Se puede hacer tambi�n con require(libreria)
  
  # -----------------
  # Proceso principal
  # ----------------- 
  # Valida las opciones validas para severidad
  severidad <- match.arg(severidad)
  
  # Encierra la severidad entre corchetes para darle m�s visibilidad al tipo de notificaci�n
  severidad <- paste("[",severidad,"]",sep = "")
  
  
  # Obtiene la informaci�n de los dispositivos asociados a la cuenta de pushbullet
  #dispInfo <- pbGetDevices()
  #dispInfo$devices
  
  # Env�a una notificaci�n (note) a todos los dispositivos vinculados a la cuenta, con el titulo 
  # y el cuerpo del mensaje especificado
  pbPost("note", paste(severidad,titulo), mensaje)

}

