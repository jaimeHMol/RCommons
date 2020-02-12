discre.fn <- function(vadis,cant,estilo,redon){
  require(classInt)
  inter.0 <- classIntervals(vadis,n=cant,style = estilo
                            ,intervalClosure = "right",dataPrecision = F)
  if(redon==0){inter.0 <- round(inter.0$brks)}else{if(redon==1)
    {inter.0 <- round(inter.0$brks,1)}}
  clases_generadas <- cut(vadis,breaks = inter.0
                          ,include.lowest = T,dig.lab = 6)
  return(clases_generadas)
}