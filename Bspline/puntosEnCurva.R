#######################################################################
###
### GENERA PUNTOS SOBRE LA BSPLINE
### (por defecto, 20 PUNTOS)
### @params: bspline (data.frame)
### @params: nPuntos (integer o NULL)
###
### Devolvemos puntos (data.frame) sobre la bspline: coordenadas (x, y)
###
#######################################################################
puntosEnCurva <- function(bspline, nPuntos=NULL) {
  puntosControl <- bspline$puntosControl
  nudos <- bspline$nudos
  p <- bspline$p
  
  ### CODIGO A REALIZAR:
  ### Generamos las coordenadas (x, y) y las almacenamos en la variable puntos
  puntos <- data.frame(x=double(), y=double())

  nPuntos <- dim(puntosControl)[1]
  
  for(u in seq(from=0.0,to=0.999,by=1/60)){
    suma <- c(0,0)
    for (i in 1:nPuntos){
      suma <- rbind(suma, (N(i, p, u, nudos) * puntosControl[i,]))
    }
    puntos<- rbind(puntos, colSums(suma))
  }
  puntos <- data.frame(puntos)
  rownames(puntos) <- NULL
  colnames(puntos) <- c("x", "y")
  return(puntos)
}
