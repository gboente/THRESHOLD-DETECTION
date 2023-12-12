library(ggplot2)

###########################################
# REGRESSION FUNCTION
###########################################

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

######################################################
#PLOTS OF  r_{u0,delta}
######################################################
 
u0 <- 0.5
delta <- 0
 
nombre.plot='r_u0_50_delta_0-new2.pdf'
pdf(nombre.plot, bg='transparent')


par(mar=c(5,5,4,4))
ggplot() + 
  stat_function(fun = r.delta, args=list(u0=u0,delta=delta),size=1.5,col="black") + 
  xlim(0,1) + ylim(-0.1,1.1) +
theme_classic()+ theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
 panel.border = element_rect(colour = "black", fill=NA, size=1.2))+
      labs(x="x",y="y") 

dev.off() 



u0 <- 0.5
delta <- -1
 
nombre.plot='r_u0_50_delta_-1-new2.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,4))
ggplot() + 
  stat_function(fun = r.delta, args=list(u0=u0,delta=delta),size=1.5,col="black") + 
  xlim(0,1) + ylim(-0.1,1.1) +
theme_classic()+ theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
 panel.border = element_rect(colour = "black", fill=NA, size=1.2))+
      labs(x="x",y="y") 

dev.off() 
 




u0 <- 0.5
delta <- 1
 
nombre.plot='r_u0_50_delta_1-new2.pdf'
pdf(nombre.plot, bg='transparent')

par(mar=c(5,5,4,3))
ggplot() + 
  stat_function(fun = r.delta, args=list(u0=u0,delta=delta),size=1.5,col="black") + 
  xlim(0,1) + ylim(-0.1,1.5) +
theme_classic()+ theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 25),
 panel.border = element_rect(colour = "black", fill=NA, size=1.2))+
      labs(x="x",y="y") 

dev.off() 


