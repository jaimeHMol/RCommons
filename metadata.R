# FUNCION DEPRECIADA!! Usar descDataSet.
# Hecha por Adrian Cal - 2017

vars.tb <- function(ds){
  ds <- ds
  vars <- colnames(ds) # nombre variables.
  lis <- list() # guarda valores unicos por variables.
  cont <- 0
  canv <- NULL # guarda # valores unicos por variable.
  clase <- NULL
  
  # guarda valores unicos por variable en lista y cuenta cantidad.
  for(i in vars){
    cont <- cont+1
    lis[cont] <- list(eval(parse(text=paste('unique(ds$`',i,'`)',sep=''))) )
    conlis <- length(lis[[cont]])
    if(is.null(canv)){canv <- conlis}else{canv <- c(canv,conlis)}
    clas <- eval(parse(text=paste('class(ds$`',i,'`)',sep='')))
    if(is.null(clase)){clase <- clas}else{clase <- c(clase,clas)}
  }
  
  # data.frame con variables y cantidad de valores únicos.
  vars.df <- data.frame(cbind(vars,canv,clase))
  names(vars.df) <- c('variable','#_unicos','clase')
  return(vars.df)
}