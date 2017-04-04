#######################################################################
###
### CALCULA X (PASO BACKWARD)
### @params: delta (data.frame)
### @params: lambda (vector)
###
### Devolvemos X (data.frame)
###
#######################################################################
dame_X <- function (delta, lambda) {
  
  ### CODIGO A REALIZAR:
  ### Calcular X
  X <- data.frame(x=double(), y=double())
  
  #numero de elementos que debe de tener X
  n <- length(lambda) + 1
  
  #Ultimo elemento
  X[n,]<-delta[n,]
  
  #Demas elementos
  for (i in (n-1):1){
    X[i,] <- delta[i,] - (lambda[i] * X[i+1,])
  }
  
  return(X)
}