#####################################################
# This file contains the functions needed to
# estimate the threshold
#####################################################

#################################################################
# The function loss.uu.arriba computes the estimate
# of the function l(u) defined in the paper
# datos : Matrix containing the Data set
#	: Fist column contains the covariates
# 	: second column contains the responses
#	: u the value above which the observations are taken
#################################################################

loss.uu.arriba<-function(u,datos) 
{
  xx<-datos[,1]
  yy<-datos[,2]
  xMu<-(xx>=u)
  yyu<-yy[xMu ]
  xxu<-xx[xMu]
  ajuste.u<-lm(yyu~xxu)
  predichos.u.arriba<-ajuste.u$fitted.values #Predicted values
  res.u.arriba<-yyu-predichos.u.arriba
  l.u.hat<-mean(res.u.arriba^2)
  salida <- l.u.hat
  salida
}


##############################################################################
# The function #estimador# computes the estimate
# of the threshold as defined in the paper 
# using a  penalization function 
#
# INPUT:
# datos 	: Matrix containing the Data set
#		: Fist column contains the covariates
# 		: second column contains the responses
# lambda 	: the penalization value which multiplies the function 
# 		   efe which is by default the identity
# corte_final 	: value to define the quantile (eta_1 in the paper)
# efe		: function of u to include in the penalization.
#		: the default is the identity function
# 		  Note that this entails that the covariates are assumed to be >=0
# 		  it can be changed defining a diferent function efe
#
# RESULTS
# grilla.u	: the considered grid of values for u
# l.hat		: the estimtor of the function l(u) defined in the paper
#		  computed over the grid grilla.u
# pl.hat	: penalized loss over the grid for the different values of lambda
# u.hat		: resulting estimator of the threshold
##############################################################################

identidad <-function(u){u}

estimador <- function(datos,lambda,corte_final=0.05, efe=identidad)
{

  xx<-sort(datos[,1])
  yy<-datos[order(datos[,1]),2]
  n <- length(xx)
  
  datos_ordenados<-cbind(xx,yy)

  #Select the possible values of u
  # cota <- datos_ordenados[,1][n*(1-corte_final)]
  # pepe<- ceiling(n*(1-corte_final))
  # cota <- xx[pepe]
  cota <- quantile(xx, (1-corte_final))
  
  
  grilla.u<- xx[xx<=cota]  
  efe.grilla.u <- efe(grilla.u)

  ################################################################
  # For each value in the grid grilla.u 
  # we consider a linear model for x>=u and compute the loss l(u)
  ################################################################


  loss.en.grilla<-sapply( grilla.u, loss.uu.arriba, datos_ordenados)
  
  ################################################################
  #compute the penalization for each value of u 
  ################################################################


  PL.en.grilla<-t(loss.en.grilla + t(outer(lambda, efe.grilla.u)))
  index.u <- apply(PL.en.grilla, 1, which.min)
  hat.u.grilla <- grilla.u[index.u]
  list(grilla.u=grilla.u, l.hat=loss.en.grilla, pl.hat=PL.en.grilla, u.hat=hat.u.grilla)
}


##############################################################################################
# The function #estimador_con_recta# computes the estimate
# of the threshold as defined in the paper 
# using a  penalization function and 
# the LS estimator of the slope and intercept for values larger than the threshold + psi
#
# INPUT:
# datos 	: Matrix containing the Data set
#		: Fist column contains the covariates
# 		: second column contains the responses
# lambda 	: the penalization value which multiplies the function 
# 		   efe which is by default the identity
# corte_final 	: value to define the quantile (eta_1 in the paper)
# efe		: function of u to include in the penalization.
#		: the default is the identity function
# 		  Note that this entails that the covariates are assumed to be >=0
# 		  it can be changed defining a diferent function efe
# psi		: the value to be added to the estimated threshold
#
# RESULTS
# grilla.u	: the considered grid of values for u
# l.hat		: the estimtor of the function l(u) defined in the paper
#		  computed over the grid grilla.u
# pl.hat	: penalized loss over the grid for the different values of lambda
# u.hat		: resulting estimator of the threshold
# a.hat		: intercept for the LS estimator when x>= u.hat+psi
# b.hat		: slope for the LS estimator when x>= u.hat+psi
##############################################################################

 

estimador_con_recta <- function(datos,lambda,corte_final=0.05,psi=0.01, efe=identidad)
{
   
  
  xx<-sort(datos[,1])
  yy<-datos[order(datos[,1]),2]
  n <- length(xx)
  
  datos_ordenados<-cbind(xx,yy)

  #Select the possible values of u
  # cota <- datos_ordenados[,1][n*(1-corte_final)]
  # pepe<- ceiling(n*(1-corte_final))
  # cota <- xx[pepe]
  cota <- quantile(xx, (1-corte_final))
  
  
  grilla.u<- xx[xx<=cota]  
  efe.grilla.u <- efe(grilla.u) 
  

  ################################################################
  # For each value in the grid grilla.u 
  # we consider a linear model for x>=u and compute the loss l(u)
  ################################################################


  loss.en.grilla<-sapply( grilla.u, loss.uu.arriba, datos_ordenados)
  
  ################################################################
  #compute the penalization for each value of u 
  ################################################################
  
  PL.en.grilla<-t(loss.en.grilla + t(outer(lambda, efe.grilla.u)))

  index.u <- apply(PL.en.grilla, 1, which.min)
  hat.u.grilla <- grilla.u[index.u]
  
  ################################################################
  #compute the  LS estimator for values of the covariates above 
  # hat.u.grilla+psi
  ################################################################
   
  xx_arriba <- xx[xx>hat.u.grilla+psi]
  yy_arriba <- yy[xx>hat.u.grilla+psi]
  
  ajuste_lineal <- lm(yy_arriba~xx_arriba)
  hat.a.u=ajuste_lineal$coefficients[1] # intercept
  hat.b.u=ajuste_lineal$coefficients[2] # slope
  
  
  list(grilla.u=grilla.u, l.hat=loss.en.grilla, pl.hat=PL.en.grilla, 
       u.hat=hat.u.grilla, a.hat=hat.a.u, b.hat=hat.b.u)
}


