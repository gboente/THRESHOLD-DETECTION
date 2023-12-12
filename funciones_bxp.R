

######### función de regresión ########

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


#########  función para hacer las Nrep repeticiones  ##########

corrida.new <- function(Nrep,nsamp,u0,delta,sigma,ctec){
  	simu <- c()
  	for(irep in 1:Nrep){
		print(irep)
    		#genero datos
    		xx<-runif(nsamp,0,1)
    		ee<-rnorm(nsamp,0,sd=sigma)
    		yy<-r.delta(xx,u0,delta)+ee
    		datos<-data.frame(x=xx,y=yy)
    		lambda <- ctec *(nsamp^(-0.4))
    		# uso el estimador
    		salida <- estimador(datos,lambda,corte_final=0.05)
  
    		umbral <- salida$u.hat
    		xMu<-(xx>=umbral)
    		yyu<-yy[xMu ]
    		xxu<-xx[xMu]
    		ajuste.u<-lm(yyu~xxu)
    		alfa.u <- ajuste.u$coef[1]
    		beta.u <- ajuste.u$coef[2]

    		to_write <- c(irep,nsamp,u0,delta,sigma,ctec,salida$u.hat, alfa.u, beta.u) 
    
     		simu <- rbind(simu, to_write )
   		file_name.LS <- paste("Umbrales-nsamp-",nsamp, "-100poru0-",100*u0,  
				"-delta-", delta,   "-100porsigma-",100*sigma, 
				"-c-",ctec,".txt", sep ='') 
    		write(to_write, file_name.LS , append = TRUE, ncol = length(to_write))
  }
  simu
}     

corrida.bxp <- function(nrep=1000,nsamp,u0,delta,sigma,valoresdec,cuales){
	beta <- gfunc.prime(u0)+delta
	alfa <- -u0*beta+gfunc(u0)
	print(c(alfa,beta))

	set.seed(2000)
 
	uhat<-betahat<-alfahat<-matrix(NA, nrow=nrep, ncol=length(valoresdec))

	for( ii in 1:length(valoresdec) ){
		ctec <- valoresdec[ii]
		print(ctec)
 		salida<- corrida.new(nrep,nsamp,u0,delta,sigma,ctec )
		uhat[,ii] <- salida[,7]
		betahat[,ii] <- salida[,9]
		alfahat[,ii] <- salida[,8]
	}
   
	########################################################
	# Save the boxplots
	########################################################

	nombre.plot<- paste("bxp_uhat-nsamp-",nsamp, "-100poru0-",100*u0,  
				"-delta-", delta,   "-100porsigma-",100*sigma, 
				"todoslosc.pdf", sep="")
	pdf(nombre.plot, bg='transparent')

	par(mar=c(5,4,3,2))
	
	pirulo<- rep(NA, length(valoresdec))
	donde <- 1:length(valoresdec)
	boxplot(uhat, names=pirulo,cex.lab=0.8)
	abline(h=u0,col="red",lwd=2)
	mtext(valoresdec, side=1,line=0.5, at=donde,cex=1.2,las=3)
	mtext("c", side=1,line=3.5, at=7,cex=1.2)
	mtext(expression(hat(u)), side=2,line=2.5, at=u0,cex=1.2)
	dev.off()


 	
	nombre.plot<- paste("bxp_uhat-nsamp-",nsamp, "-100poru0-",100*u0,  
				"-delta-", delta,   "-100porsigma-",100*sigma, 
				".pdf", sep="")
	pdf(nombre.plot, bg='transparent')

	par(mar=c(5,4,3,2))
	pirulo<- rep(NA, length(cuales))
	donde <- 1:length(cuales)
	
	boxplot(uhat[,cuales], names=pirulo,cex.lab=0.8)
	abline(h=u0,col="red",lwd=2)
	mtext(valoresdec[cuales], side=1,line=0.5, at=donde,cex=1.2,las=3)
	mtext("c", side=1,line=3.5, at=7,cex=1.2)
	mtext(expression(hat(u)), side=2,line=2.5, at=u0,cex=1.2)
	
	dev.off()


	nombre.plot<- paste("bxp_alfahat-nsamp-",nsamp, "-100poru0-",100*u0,  
				"-delta-", delta,   "-100porsigma-",100*sigma, 
				".pdf", sep="")
	pdf(nombre.plot, bg='transparent')

	pirulo<- rep(NA, length(cuales))
	donde <- 1:length(cuales)
	
	par(mar=c(5,4,3,2))
	boxplot(alfahat[,cuales], names=pirulo,cex.lab=0.8)
	abline(h=alfa,col="red",lwd=2)
	mtext(valoresdec[cuales], side=1,line=0.5, at=donde,cex=1.2,las=3)
	mtext("c", side=1,line=3.5, at=7,cex=1.2)
	mtext(expression(hat(alpha)), side=2,line=3, at=alfa,cex=1)

	dev.off()

	nombre.plot<- paste("bxp_betahat-nsamp-",nsamp, "-100poru0-",100*u0,  
				"-delta-", delta,   "-100porsigma-",100*sigma, 
				".pdf", sep="")
	pdf(nombre.plot, bg='transparent')


	pirulo<- rep(NA, length(cuales))
	donde <- 1:length(cuales)
	
	par(mar=c(5,4,3,2))
	boxplot(betahat[,cuales], names=pirulo,cex.lab=0.8)
	abline(h=beta,col="red",lwd=2)
	mtext(valoresdec[cuales], side=1,line=0.5, at=donde,cex=1.2,las=3)
	mtext("c", side=1,line=3.5, at=7,cex=1.2)
	mtext(expression(hat(beta)), side=2,line=3, at=beta,cex=1)

	dev.off()

}


leo.bxp <- function(nrep=1000,nsamp,u0,delta,sigma,valoresdec){
	beta <- gfunc.prime(u0)+delta
	alfa <- -u0*beta+gfunc(u0)
	print(c(alfa,beta))
 
	setwd("D:\\Graciela\\DOCTEX\\Modelos UMBRAL\\simulacion\\salidas-u0-alfa-beta")
	
        uhat<-betahat<-alfahat<-matrix(NA, nrow=nrep, ncol=length(valoresdec))

	for( ii in 1:length(valoresdec) ){
		ctec <- valoresdec[ii]
		print(ctec)
   		file_name<- paste("Umbrales-nsamp-",nsamp, "-100poru0-",100*u0,  
				"-delta-", delta,   "-100porsigma-",100*sigma, 
				"-c-",ctec,".txt", sep ='') 
    		salida<- read.table(file_name, header=FALSE) 

		if(salida[1,6]!=ctec){print("ERROR c no es el correcto")} 
		uhat[,ii] <- salida[,7]
		betahat[,ii] <- salida[,9]
		alfahat[,ii] <- salida[,8]
	}
   
	return(list(uhat=uhat, betahat=betahat, alfahat=alfahat))
}

 
