#######################################################################
###
### CALCULA ALFA, BETA, GAMMA (PASO FORDWARD 1)
### @params: n (integer)
### @params: nudos (vector)
###
### Devolvemos los vectores alfa, beta, gamma
###
#######################################################################
dame_AlfaBetaGamma <- function (n, nudos) {
  alfa  <- array(0, dim = n)
  beta  <- array(0, dim = n)
  gamma <- array(0, dim = n)
  
  # para comprobar si un numero es cero (FLOAT)
  # if (A < epsilon) donde epsilon == 1.0e-8, 1.0e-16, 1.0e-32
  
  ### alfa[1] == alfa[n] == 0
  ### gamma[1] == gamma[n] == 0
  ### beta[1] == beta[n] == 1
  
  ### CODIGO A REALIZAR:
  ### Calcular los vectores alfa, beta, gamma
  
  epsilon <- (1.0e-32)
  
  alfa[1] <- 0.0
  gamma[1] <- 0.0
  beta[1] <- 1.0
  
  for(i in 2:(n-1)){
    uk1 <- nudos[i+1]
    uk2 <- nudos[i+2]
    uk3 <- nudos[i+3]
    uk4 <- nudos[i+4]
    uk5 <- nudos[i+5]
    
    #Calculo de alfa i
    zatAlfa <-(uk4-uk1)*(uk4-uk2)
    if(zatAlfa > epsilon){
      alfa[i] <- ((uk4-uk3)^2)/zatAlfa
    }else{
      alfa[i] <- c(alfa,0.0)
    }
    
    #Calculo de beta i
    zatBeta1 <- (uk4-uk1)*(uk4-uk2)
    zatBeta2 <- (uk5-uk2)*(uk4-uk2)
    if(zatBeta1 > epsilon || zatBeta2 > epsilon){
      beta[i] <- (((uk3-uk1)*(uk4-uk3))/zatBeta1) + (((uk5-uk3)*(uk3-uk2))/zatBeta2)
    }else{
      beta[i] <- 0.0
    }
    
    #Calculo de gamma i
    zatGamma <- (uk4-uk2)*(uk5-uk2)
    if(zatGamma > epsilon){
      gamma[i] <- (uk3-uk2)^2/zatGamma
    }else{
      gamma[i] <- 0.0
    }
  }
  
  alfa[n] <- 0.0
  gamma[n] <- 0.0
  beta[n] <- 1.0
  
  res       <- NULL
  res$alfa  <- alfa
  res$beta  <- beta
  res$gamma <- gamma
  
  return(res)
  
}