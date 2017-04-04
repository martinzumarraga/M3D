#######################################################################
###
### CALCULA LAMBDA-DELTA (PASO FORDWARD 2)
### @params: datos (data.frame)
### @params: alfa, beta, gamma
###
### Devolvemos lambda (vector) y beta (data.frame)
###
#######################################################################
dame_LambdaDelta <- function (datos, alfa, beta, gamma) {
  n <- length(alfa)
  lambda <- array(0, dim = c(n-1,1))
  delta  <- data.frame(x=double(), y=double())
  epsilon <- 1.0e-32
  ### CODIGO A REALIZAR:
  ### Calcular lambda y delta lambda 1 menos
  
  lambda[1] <- gamma[1]
  delta[1,] <- datos[1,]
  
  #Calculo lambda
  for(i in 2:(n-1)){
    zatLambda <- beta[i] - (alfa[i] * lambda[i-1])
    if(zatLambda > epsilon){
      lambda[i] <- gamma[i] / zatLambda
    }else {
      lambda[i] <- 0.0
    }
  }
  
  #Calculo delta
  for(i in 2:(n)){
    zatDelta <- beta[i] - (alfa[i] * lambda[i-1])
    if(zatDelta > epsilon){
      delta[i,] <- (datos[i,] - (alfa[i] * delta[i-1,])) / zatDelta
    }else{
      delta[i,] <- 0.0
    }
  }
  res        <- NULL
  res$lambda <- lambda
  res$delta  <- delta
  
  return(res)
}