
source('funciones_estimacion.R')

 
options("scipen"=100, "digits"=8)
library(ggplot2)
letra <- 30
ejes <- 25
    

###################################################################
# AIR QUALITY DATA 
# READ file
###################################################################

data(airquality)
 letra <- 30
 ejes <- 25


  x <- airquality
 x <- x[complete.cases(x), ]
  
 x <- x[, c('Ozone', 'Solar.R', 'Wind', 'Temp')]
 x

x1= as.vector(x$Wind)
y <- as.vector(x$Ozone)


#########################
# Linear regression fit
#########################

ajuste.lineal <- lm(y~x1) 
predicho.lineal <- ajuste.lineal$coef[1]+ajuste.lineal$coef[2]*x1
 
summary(ajuste.lineal)

 

datos <- data.frame(wind=x1, ozone=y)


################################################
# SOME PLOTS
################################################

nombre.plot <- 'ozone-data.pdf'
pdf(nombre.plot, bg='transparent')
 
out<- c(23, 34, 53, 68 , 77)
p <- ggplot(datos, aes(x=wind,y=ozone)) + geom_point(size=3)
p +  geom_point(data = datos[out,],colour="red")  +
theme_classic()+
  labs(x="Wind", y="Ozone") +
  theme(
    axis.title.x = element_text(size =letra),
    axis.title.y = element_text(size = letra),
    axis.text = element_text(size = ejes),
 panel.border = element_rect(colour = "black", fill=NA, size=1)

  )
dev.off()
 


########################################
# Plot data with lm and lowess fits 
########################################

nombre.plot <- 'ozone-lm-loess.pdf'
pdf(nombre.plot, bg='transparent')


p <- ggplot(datos, aes(x=wind,y=ozone)) + geom_point(size=3)
p +  
 geom_smooth(method='lm',formula=y~x, se=FALSE, col="blue3",lwd=2,,alpha=0.8) +
  geom_smooth(method = "loess",formula=y~x, color="green3",lwd=2,, se=FALSE,alpha=0.8) +
   theme_classic()+
  labs(x="Wind", y="Ozone") +
  theme(
    axis.title.x = element_text(size =letra),
    axis.title.y = element_text(size = letra),
    axis.text = element_text(size = ejes),
 panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()
 

 
########################################
# Plot data with lowess fit 
########################################

nombre.plot <- 'ozone-loess.pdf'
pdf(nombre.plot, bg='transparent')


p <- ggplot(datos, aes(x=wind,y=ozone)) + geom_point(size=3)
p +  
 #geom_smooth(method='lm',formula=y~x, se=FALSE, col="blue3",lwd=2,,alpha=0.8) +
  geom_smooth(method = "loess",formula=y~x, color="green4",lwd=2,, se=FALSE,alpha=0.8) +
   theme_classic()+
  labs(x="Wind", y="Ozone") +
  theme(
    axis.title.x = element_text(size =letra),
    axis.title.y = element_text(size = letra),
    axis.text = element_text(size = ejes),
 panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

dev.off()


###########################################################
# ESTIMATION WITH THE IDENTITY FUNCTION
###########################################################

ene<- dim(datos)[1]

quantile(x1,0.98)
#18.4

lambda0 <- 0
salida0 <- estimador(datos,lambda0*ene^(-0.4),corte_final=0.02)
 
salida0


lambda <- c(seq(0,10,0.001), seq(10.01, 150, 0.01), seq(150.1, 500, 0.1))
salida <- estimador(datos,lambda*ene^(-0.4),corte_final=0.02)


uposible<-unique(salida$u.hat)
uposible

# 18.4 16.6 15.5 10.9  4.6  2.3

largoues<- NULL

for(j in 1: length(uposible)){
	corte<- uposible[j]
	lx<-length(x1[corte<=x1])
	largoues <- rbind(largoues, c(corte,lx))

}
largoues
######################
# 18.4    3
# 16.6    5
# 15.5    8
# 10.9   43
#  4.6  106
#  2.3  111
######################


tabela <- data.frame(lambda=lambda, uhat=salida$u.hat)

length(lambda)


nombre.plot <- 'ozone-perfiles-c.pdf'
pdf(nombre.plot, bg='transparent')


par(mar=c(5,5,4,4))

ggplot(data = tabela , aes(x=lambda, y=uhat)) + geom_point(col="red4",size=3) +
   theme_classic()+  
  labs(x="c", y=expression(hat(italic(u)))) +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
 panel.border = element_rect(colour = "black", fill=NA, size=1.2)
  )
 
dev.off()

j=1
corte<- uposible[j]
corte
#18.4
XX<-x1[corte<=x1]
YY<-y[corte<=x1]
summary(lm(YY~XX))


######################################################################################

j=j+1
corte<- uposible[j]
corte
#16.6
XX<-x1[corte<=x1]
YY<-y[corte<=x1]
summary(lm(YY~XX))

 

#######################################################################################

j=j+1
corte<- uposible[j]
corte
#15.5
XX<-x1[corte<=x1]
YY<-y[corte<=x1]
summary(lm(YY~XX))
 

############################################################################################

j=j+1
corte<- uposible[j]
corte
#10.9
XX<-x1[corte<=x1]
YY<-y[corte<=x1]
summary(lm(YY~XX))
 


#####################################
# PLOT
#####################################



nombre.plot <- paste('ozone-threshold-', 10*corte, '-white.pdf', sep="")
pdf(nombre.plot, bg='transparent')


p <- ggplot(datos, aes(x=wind,y=ozone)) + geom_point(size=3)

p + geom_smooth(data = subset(datos, corte<=wind),method='lm',
		formula=y~x, lwd=2, se=FALSE, col="red4", size=2) +
  	geom_vline(xintercept = corte, linetype="dashed", 
                color = "blue", size=2)     +
 	#geom_smooth(data =subset(datos,  wind<=corte),method = "loess",
	# 	formula=y~x, color="red4", se=FALSE,alpha=2, size=2) +
	#geom_smooth(data = datos ,method = "loess",
	# 	formula=y~x, color="green4", se=FALSE,alpha=2, size=2) +
	#  theme_linedraw(base_size = 25) +
  	theme_classic()+
  	labs(x="Wind", y="Ozone") +
  	theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size = letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
dev.off()

  



nombre.plot <- paste('ozone-threshold-', 10*corte, '-withlowess-dashed-white.pdf', sep="")
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))

p <- ggplot(datos, aes(x=wind,y=ozone)) + geom_point(size=3)

p + geom_smooth(data = subset(datos, corte<=wind),method='lm',
		formula=y~x, lwd=2, se=FALSE, col="red4", size=2) +
  	geom_vline(xintercept = corte, linetype="dashed", 
                color = "blue", size=2)     +
 	#geom_smooth(data =subset(datos,  wind<=corte),method = "loess",
	# 	formula=y~x, color="red4", se=FALSE,alpha=2, size=2) +
	geom_smooth(data = datos ,method = "loess",
	 	formula=y~x, color="green4",  linetype="dashed",  se=FALSE,alpha=2, size=2) +
     #  theme_linedraw(base_size = 25) +
  	theme_classic()+
  	labs(x="Wind", y="Ozone") +
  	theme(
    	axis.title.x = element_text(size =letra),
    	axis.title.y = element_text(size = letra),
    	axis.text = element_text(size = ejes),
 	panel.border = element_rect(colour = "black", fill=NA, size=1)
 	 )
dev.off() 



###################################################
# LS with a value larger than the threshold
###################################################


psi <- 1
corte=uposible[4]+psi ## uposible[4]+psi with psi=1 #11.9

XX<-x1[corte<=x1]
YY<-y[corte<=x1]
summary(lm(YY~XX))
 


##########################################################################

j=j+1
corte<- uposible[j]
corte
#4.6
XX<-x1[corte<=x1]
YY<-y[corte<=x1]
summary(lm(YY~XX))

  



