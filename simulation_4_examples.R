rm(list=ls())
 
source('funciones_estimacion.R')
options("scipen"=100, "digits"=4)

########################################
# REGRESSION FUNCTION
########################################

gfunc <- function(x){
  gfunc <- ifelse(x<=0.5,4*x^2*(3-4*x),(4/3)*x*(4*x^2-10*x+7)-1)
  gfunc
} 

gfunc.prime <- function(x){
  gfunc.prime <- ifelse(x<=0.5,24*x-48*x^2,16*x^2-80*x/3+28/3)
  gfunc.prime
} 

r.delta <- function(x,u0=0.75,delta=0){
  r.delta <- ifelse(x<=u0,gfunc(x), (gfunc.prime(u0)+delta)*(x-u0)+gfunc(u0))
  r.delta
}


########################################
# FUNCTIONS TO PERFORM THE REPLICATIONS
########################################

corrida <- function(Nrep,nsamp,u0,delta,sigma,lambda)
{
  simu <- c()
  for(a in 1:Nrep)
  {
    #Generate data
    xx<-runif(nsamp,0,1)
    ee<-rnorm(nsamp,0,sd=sigma)
    yy<-r.delta(xx,u0,delta)+ee
    datos<-data.frame(x=xx,y=yy)
    
    # Save the estimator

    salida <- estimador(datos,lambda,corte_final=0.05)
    simu <- rbind(simu,salida$u.hat)  
  }
  simu
}    


########################################
# FUNCTIONS TO COMPUTE THE EMAE
########################################  

emae <- function(x,u0){
  emae <- mean(abs(x-u0))
  emae
}

 
###################
# SCENARIO 16
###################

  u0 <- 0.75
  delta <- -0.5
  sigma <- 0.01
  n <- seq(100,2000,100)
  lambda <- sort(c(0,10^seq(-10,2),0.5*10^seq(-10,2)))
 nrep <- 1000
 
 set.seed(2000)
  simu <- list()
  system.time(
    for( i in 1:length(n) ){
     cat(n[i])
      cat("\n")
      simu[[i]] <- corrida(nrep,n[i],u0,delta,sigma,lambda*(n[i]^(-0.4)))
   })
  
 results <- c()
  for( i in 1:length(n) ){
    results <- rbind(results, apply(simu[[i]],2,emae,u0))
  }
  
   write.table(results, file="escenario16.txt",row.names = FALSE,
            col.names = FALSE)




###################
# SCENARIO 17
###################

  u0 <- 0.75
  delta <- -1.5
  sigma <- 0.01
  n <- seq(100,2000,100)
  lambda <- sort(c(0,10^seq(-10,2),0.5*10^seq(-10,2)))
 nrep <- 1000
 
 set.seed(2000)
  simu <- list()
  system.time(
    for( i in 1:length(n) ){
     cat(n[i])
      cat("\n")
      simu[[i]] <- corrida(nrep,n[i],u0,delta,sigma,lambda*(n[i]^(-0.4)))
   })
  
 results <- c()
  for( i in 1:length(n) ){
    results <- rbind(results, apply(simu[[i]],2,emae,u0))
  }
  
   write.table(results, file="escenario17.txt",row.names = FALSE,
            col.names = FALSE)




###################
# SCENARIO 20
###################

  u0 <- 0.5
  delta <- 1
  sigma <- 0.01
  n <- seq(100,2000,100)
  lambda <- sort(c(0,10^seq(-10,2),0.5*10^seq(-10,2)))
 nrep <- 1000
 
 set.seed(2000)
  simu <- list()
  system.time(
    for( i in 1:length(n) ){
     cat(n[i])
      cat("\n")
      simu[[i]] <- corrida(nrep,n[i],u0,delta,sigma,lambda*(n[i]^(-0.4)))
   })
  
 results <- c()
  for( i in 1:length(n) ){
    results <- rbind(results, apply(simu[[i]],2,emae,u0))
  }
  
   write.table(results, file="escenario20.txt",row.names = FALSE,
            col.names = FALSE)



###################
# SCENARIO 21
###################

  u0 <- 0.5
  delta <- -2
  sigma <- 0.01
  n <- seq(100,2000,100)
  lambda <- sort(c(0,10^seq(-10,2),0.5*10^seq(-10,2)))
 nrep <- 1000
 
 set.seed(2000)
  simu <- list()
  system.time(
    for( i in 1:length(n) ){
     cat(n[i])
      cat("\n")
      simu[[i]] <- corrida(nrep,n[i],u0,delta,sigma,lambda*(n[i]^(-0.4)))
   })
  
 results <- c()
  for( i in 1:length(n) ){
    results <- rbind(results, apply(simu[[i]],2,emae,u0))
  }
  
   write.table(results, file="escenario21.txt",row.names = FALSE,
            col.names = FALSE)


