#######################################################################
###
### CALCULA LOS NUDOS UNIFORMES DE UNA BSPLINE
### @params: nDatos (numero de datos)
### @params: p (grado de la bspline)
###
### Devolvemos un vector de nudos
###
#######################################################################
calculaNudosUniforme <- function(nDatos, p) {
  
  ### CODIGO A REALIZAR:
  ### Calcular los nudos uniformes de una bspline
  nudos <- c(rep(0.0,p), seq(0,1,by=(1/(nDatos-1))), rep(1.0,p))
  
  return(nudos)
}