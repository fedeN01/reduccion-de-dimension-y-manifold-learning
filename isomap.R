install.packages('lle')
library(lle)
library(vegan)
library(ggplot2)
library(scatterplot3d)

#SWISS ROLL a mano
swissroll<-c()
i<-3
j<-1
  while(i<10){
    x<-runif(1,i*pi/2,(i+1)*pi/2)
    y<-runif(1,0,15)
    swissroll<-append(swissroll,c(x*cos(x),y,x*sin(x)))
    j<-j+1
    if(j>140){
      j<-1
      i<-i+1
    }
  }

swissroll<-matrix(data=swissroll,nrow=980,ncol=3,byrow=TRUE)
colores<-c(rep('red',140),rep('orange',140),rep('yellow',140),rep('green',140),rep('cyan',140),rep('blue',140),rep('purple',140))

s3d<-scatterplot3d(swissroll,pch=16,angle=75,color=colores)
swissroll_dist<-dist(swissroll, method = "euclidean")

#ISOMAP
res<-isomap(d=swissroll_dist,k=7, ndim=2)
proyectados<-scores(res)
ggplot(data=as.data.frame(proyectados),mapping=aes(x=Dim1,y=Dim2))+geom_point(col=colores)

#MDS
swissroll_mds2<-cmdscale(swissroll_dist,k=2)
swissroll_mds2<-as.data.frame(swissroll_mds2)
ggplot(data=swissroll_mds2,mapping=aes(x=V1,y=V2))+geom_point(col=colores)


#LLE
res2<-lle(swissroll,k=10, m=2,nnk=TRUE)
proyectados<-scores(res2$Y)
ggplot(data=as.data.frame(proyectados),mapping=aes(x=proyectados[,1],y=proyectados[,2]))+geom_point(col=colores)
