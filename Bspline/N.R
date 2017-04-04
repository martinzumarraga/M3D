#######################################################################
###
### FUNCION COX-DE-BOOR
### @params: i (indice)
### @params: p (grado de la curva)
### @params: u (parametro)
### @params: nudos (vector)
###
### Devolvemos el valor de la funcion base N(i,p,u)
###
#######################################################################
N <- function (i, p, u, nudos) {
	### CODIGO A REALIZAR
    ### Calcular el valor de la funcion base N(i,p,u)
    nip <- 0
    
  	return(nip)
}#######################################################################
###
### FUNCION COX-DE-BOOR
### @params: i (indice)
### @params: p (grado de la curva)
### @params: u (parametro)
### @params: nudos (vector)
###
### Devolvemos el valor de la funcion base N(i,p,u)
###
#######################################################################
N <- function (i, p, u, nudos) {
  ### CODIGO A REALIZAR
  ### Calcular el valor de la funcion base N(i,p,u)
  
  nip <- 0
  
  
  #Si el grado es 0
  if(p == 0){
    if((nudos[i]<= u) && (u < nudos[i+1])) { #Si p es 0 y u est? en el intervalo [Ui,Ui+1) n0i = 1.
      nip <- 1
    }
    else{ # u no se encuentra en [Ui,Ui+1)
      nip <- 0
    }
    
  }else{
    
    #Si el grado es mayor a 0 se utilizar? el algoritmo de Cox-de boor
    
    if((nudos[i+p]-nudos[i]) > 1.0e-8){ #Se comprueba si el denominador es mayor a 0 para evitar problemas.
      left_nip <- ((u-nudos[i])/(nudos[i+p]-nudos[i]))*N(i, p-1, u, nudos)
    }
    
    else{
      left_nip <- 0
    }
    
    if((nudos[i+p+1]-nudos[i+1]) > 1.0e-8){
      right_nip <- ((nudos[i+p+1]-u)/(nudos[i+p+1]-nudos[i+1]))*N(i+1,p-1,u,nudos)
    }
    
    else{
      right_nip <- 0
    }
    
    
    #Sumando ambas partes conseguimos el valor total de nip
    nip <- left_nip + right_nip
    
  }
  
  return(nip)
}#######################################################################
###
### FUNCION COX-DE-BOOR
### @params: i (indice)
### @params: p (grado de la curva)
### @params: u (parametro)
### @params: nudos (vector)
###
### Devolvemos el valor de la funcion base N(i,p,u)
###
#######################################################################
N <- function (i, p, u, nudos) {
  ### CODIGO A REALIZAR
  ### Calcular el valor de la funcion base N(i,p,u)
  
  nip <- 0
  
  
  #Si el grado es 0
  if(p == 0){
    if((nudos[i]<= u) && (u < nudos[i+1])) { #Si p es 0 y u est? en el intervalo [Ui,Ui+1) n0i = 1.
      nip <- 1
    }
    else{ # u no se encuentra en [Ui,Ui+1)
      nip <- 0
    }
    
  }else{
    
    #Si el grado es mayor a 0 se utilizar? el algoritmo de Cox-de boor
    
    if((nudos[i+p]-nudos[i]) > 1.0e-8){ #Se comprueba si el denominador es mayor a 0 para evitar problemas.
      left_nip <- ((u-nudos[i])/(nudos[i+p]-nudos[i]))*N(i, p-1, u, nudos)
    }
    
    else{
      left_nip <- 0
    }
    
    if((nudos[i+p+1]-nudos[i+1]) > 1.0e-8){
      right_nip <- ((nudos[i+p+1]-u)/(nudos[i+p+1]-nudos[i+1]))*N(i+1,p-1,u,nudos)
    }
    
    else{
      right_nip <- 0
    }
    
    
    #Sumando ambas partes conseguimos el valor total de nip
    nip <- left_nip + right_nip
    
  }
  
  return(nip)
}