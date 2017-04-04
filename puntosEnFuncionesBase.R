#######################################################################
###
### GENERA PUNTOS A PARTIR DE LAS FUNCIONES BASE DE LA  BSPLINE 
### (por defecto, 20 PUNTOS)
### @params: bspline (data.frame)
### @params: nPuntos (integer o NULL)
###
### Devolvemos puntos (data.frame) a partir de las funciones base de
### la bspline
### Cada punto de la funcion base N(i, p, u, nudos): 
### coordenadas (x, y) y color (i == indice de la funcion base) 
###
#######################################################################
puntosEnFuncionesBase <- function(bspline, nPuntos=NULL) {
	puntosControl <- bspline$puntosControl
	nudos <- bspline$nudos
	p <- bspline$p

	### CODIGO A REALIZAR:
    ### Generamos los puntos (x, y, color) y los almacenamos en la variable puntos
    puntos <- NULL
    
    ### lo que hay que guardar en puntos es el punto formado por:
    ### x = u
    ### y = N(i, p, u, nudos)
    ### color = i
    
    #### NO HAY QUE SUMAR... NO ES UN SUMATORIO... ES UNA LISTA DE PUNTOS

	# rownames(puntos) <- NULL
	# colnames(puntos) <- c("x", "y", "color")
	# puntos <- data.frame(puntos)
	# puntos$color <- factor(puntos$color)
		
	return(puntos)
}