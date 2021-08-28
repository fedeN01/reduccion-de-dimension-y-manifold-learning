library(ggplot2)
library(scatterplot3d)

vuelos<-read.csv(file.choose(),header=TRUE,sep=",")
nombre_ciudades<-names(vuelos)
vuelos<-as.dist(vuelos)

#MDS a dimensión 2
vuelos_mds2<-cmdscale(vuelos,k=2)
vuelos_mds2<-as.data.frame(vuelos_mds2)
ggplot(vuelos_mds2,aes(x=V1,
                     y=V2,
                     label=nombre_ciudades))+
  geom_text(alpha=0.8,size=3,col="steelblue")

#MDS a dimensión 3
vuelos_mds3 <- cmdscale(vuelos, k = 3)
vuelos_mds3<-as.data.frame(vuelos_mds3)
s3d<-scatterplot3d(vuelos_mds3, type = "h", pch = 19, lty.hplot = 2)
text(s3d$xyz.convert(vuelos_mds3[, 1:3]), labels = nombre_ciudades,
     cex= 0.7, col = "steelblue")

#Comparo
max(abs(vuelos-dist(vuelos_mds2)))
max(abs(vuelos-dist(vuelos_mds3)))

#------------------------------------------------
library(mlbench)
data(BreastCancer)

pacientes<-BreastCancer[,-1]
pacientes_dist<-dist(pacientes[,-10], method = "euclidean")

dos_colores<-c('lightgreen','red')
colores<-dos_colores[pacientes$`Class`]

#MDS a dimensión 2
pacientes_mds2<-cmdscale(pacientes_dist,k=2)
pacientes_mds2<-as.data.frame(pacientes_mds2)
ggplot(data=pacientes_mds2,mapping=aes(x=V1,y=V2,color=pacientes$`Class`))+geom_point()+ theme(legend.title = element_blank())


#MDS a dimensión 3
pacientes_mds3<-cmdscale(pacientes_dist, k = 3)
pacientes_mds3<-as.data.frame(pacientes_mds3)
s3d<-scatterplot3d(pacientes_mds3,type="h",pch=16,angle=75,color=colores)
legend("topright", legend = levels(pacientes$`Class`),
       col =dos_colores, pch = 16)

