library(ggplot2)
library(SemiPar)

source('funciones_estimacion.R')

 
options("scipen"=100, "digits"=8)
library(ggplot2)
letra <- 30
ejes <- 25
 


#The LIDAR data contains 221 observations

data(lidar)

 
datos<-cbind(sort(720-lidar$range),lidar$logratio[order(720-lidar$range)])

plot(datos)
range(datos[,1])

lidar1<-lidar
 lidar1$range <- 720-lidar$range

 



nombre.plot <- 'LIDAR-invertido-lowess-white.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))
 

p  <- ggplot(data=lidar1,aes(x=range,y=logratio)) + geom_point(size=3)

p +  	geom_smooth(data =lidar1,method = "loess",
	 	formula=y~x, color="green4", se=FALSE,alpha=2, size=2) +
      labs(x="720-range", y="logratio") +
  	 theme_classic()+theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size = letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
   

dev.off() 

 


nombre.plot <- 'LIDAR-lowess-white.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))
 

p  <- ggplot(data=lidar,aes(x=range,y=logratio)) + geom_point(size=3)

p +  	geom_smooth(data =lidar,method = "loess",
	 	formula=y~x, color="green4", se=FALSE,alpha=2, size=2) +
      labs(x="range", y="logratio") +
  	 theme_classic()+theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size =letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
   

dev.off() 


nombre.plot <- 'LIDAR-lowess-lm-white.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))
 

p  <- ggplot(data=lidar,aes(x=range,y=logratio)) + geom_point(size=3)

p +  	geom_smooth(data =lidar,method = "loess",
	 	formula=y~x, color="green4", se=FALSE,alpha=2, size=2)+  	
 	geom_smooth(data =lidar,method = "lm",
	 	formula=y~x, color="red4", se=FALSE,alpha=2, size=2) +
      labs(x="range", y="logratio") +
  	 theme_classic()+theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size =letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
   

dev.off() 

XXtodos<-lidar$range 
YYtodos<-lidar$logratio 
summary(lm(YYtodos~XXtodos))
 


ene<- dim(datos)[1]
lambda <- seq(0,10,0.0001)
salida <- estimador(datos,lambda*ene^(-0.4),corte_final=0.02)
#plot(salida$grilla.u, salida$l.hat, type="l")
levels(as.factor(salida$u.hat))

uhat.orig<- - (salida$u.hat-720) 
  

 unique(salida$u.hat)
#323 170 152 149   0
unique(uhat.orig)
#397 550 568 571 720

tabela <- data.frame(lambda=lambda, uhat=salida$u.hat)


tabela.orig <- data.frame(lambda=lambda, uhat=uhat.orig)

 
 
 
  

nombre.plot <- 'Lidar-perfiles-c-white-2.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))

ggplot(data = tabela[1:50,], aes(x=lambda, y=uhat)) + geom_point(col="red4",size=3) +
   theme_classic()+
  labs(x="c", y=expression(hat(italic(u))[X])) +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
 panel.border = element_rect(colour = "black", fill=NA, size=1.2)
  )
 
dev.off()

nombre.plot <- 'Lidar-perfiles-c-white-2-new.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))

ggplot(data = tabela[1:30,], aes(x=lambda, y=uhat)) + geom_point(col="red4",size=3) +
   theme_classic()+
  labs(x="c", y=expression(hat(italic(u))[X])) +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
 panel.border = element_rect(colour = "black", fill=NA, size=1.2)
  )
 
dev.off()

uposibles <- unique(uhat.orig)
uposibles

corte<- 568 # uposibles[3]

XX<-lidar$range[lidar$range<=corte]
YY<-lidar$logratio[lidar$range<=corte]
summary(lm(YY~XX))
 

corte <- 560 # uposibles[3]-psi with psi=8

XX<-lidar$range[lidar$range<=corte]
YY<-lidar$logratio[lidar$range<=corte]
summary(lm(YY~XX))

 
 
 




corte<- 571 # uposibles[4]
XX<-lidar$range[lidar$range<=corte]
YY<-lidar$logratio[lidar$range<=corte]
summary(lm(YY~XX))
 



corte<- 549 ## uposibles[2]-psi with psi=1
XX<-lidar$range[lidar$range<=corte]
YY<-lidar$logratio[lidar$range<=corte]
summary(lm(YY~XX))
 

 


nombre.plot <- 'LIDAR-threshold-recta-549-white.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))
corte<- 549

p <- ggplot(lidar, aes(x=range,y=logratio)) + geom_point(size=3)

p + geom_smooth(data = subset(lidar, range<=corte),method='lm',
		formula=y~x, lwd=2, se=FALSE, col="red4", size=2) +
  	geom_vline(xintercept = corte, linetype="dashed", 
                color = "blue", size=2)     +
 	#geom_smooth(data =subset(lidar, range>(corte)),method = "loess",
	# 	formula=y~x, color="red4", se=FALSE,alpha=2, size=2) +
	#  theme_linedraw(base_size = 25) +
  	theme_classic()+
  	labs(x="range", y="logratio") +
  	theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size = letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
   

dev.off() 

 


########################################################


corte<- 550 #uposibles[2]

XX<-lidar$range[lidar$range<=corte]
YY<-lidar$logratio[lidar$range<=corte]
summary(lm(YY~XX))
  

 



nombre.plot <- 'LIDAR-threshold-recta-550-white.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))
corte<- 550

p <- ggplot(lidar, aes(x=range,y=logratio)) + geom_point(size=3)

p + geom_smooth(data = subset(lidar, range<=corte),method='lm',
		formula=y~x, lwd=2, se=FALSE, col="red4", size=2) +
  	geom_vline(xintercept = corte, linetype="dashed", 
                color = "blue", size=2)     +
 	#geom_smooth(data =subset(lidar, range>(corte)),method = "loess",
	# 	formula=y~x, color="red4", se=FALSE,alpha=2, size=2) +
	#  theme_linedraw(base_size = 25) +
  	theme_classic()+
  	labs(x="range", y="logratio") +
  	theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size = letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
   

dev.off() 

 
 

 
