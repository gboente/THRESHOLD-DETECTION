rm(list=ls())
 
source('funciones_estimacion.R')
source('funciones_bxp.R')

options("scipen"=100, "digits"=4)


nrep <- 1000 
###########################
# u0=0.5 
# sigma=0.10 
###########################

u0 <- 0.75
sigma <- 0.10

###########################
# delta=-1
###########################

delta <- -1


beta <- gfunc.prime(u0)+delta
alfa <- -u0*beta+gfunc(u0)
c(alfa,beta)

###########################
# nsamp=200
###########################

nsamp <- 200

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11
 


t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

 
###########################
# nsamp=500
###########################

nsamp <- 500

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

 

###########################
# nsamp=1000
###########################

nsamp <- 1000

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

###########################
# delta=1
###########################

delta <- 1


beta <- gfunc.prime(u0)+delta
alfa <- -u0*beta+gfunc(u0)
c(alfa,beta)

###########################
# nsamp=200
###########################

nsamp <- 200

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

 
###########################
# nsamp=500
###########################

nsamp <- 500

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

 

###########################
# nsamp=1000
###########################

nsamp <- 1000

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

###########################
# delta=0
###########################

delta <- 0


beta <- gfunc.prime(u0)+delta
alfa <- -u0*beta+gfunc(u0)
c(alfa,beta)

###########################
# nsamp=200
###########################

nsamp <- 200

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

 
###########################
# nsamp=500
###########################

nsamp <- 500

 
set.seed(2000)


valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11

t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

 

###########################
# nsamp=1000
###########################

nsamp <- 1000

 
set.seed(2000)

 
valoresdec <- c(0.0001,  0.01, 0.02,   0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.4)
cuales <- 1:11


t1 <- Sys.time()
corrida.bxp(nrep,nsamp,u0,delta,sigma,valoresdec,cuales)
  
t2<-Sys.time()
t2-t1

  