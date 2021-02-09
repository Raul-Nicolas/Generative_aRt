library(gplots)
library(tidyverse)
library(ggplot2)
library(viridis)
library(data.table)
library(cowplot)
library(tidyr)
library(dplyr)
#pdf("Rttest.pdf")
my_palette = viridis(256)


spreader <- function(boxdim1, threshold, rounds){
  
  rojo=matrix(runif(boxdim1^2), ncol = boxdim1)
  rojo[rojo<threshold]<-0
  rojo[rojo>threshold]<-1
  
  for(cont in 1:rounds){
    newrojo = matrix(rep(0, boxdim1^2),  ncol = boxdim1)
    for(i in 1:(boxdim1-1)){
      for(j in 1:(boxdim1-1)){
        newrojo[i,j] = (sample(rojo[(i-1):(i+1), (j-1):(j+1)],1) + rojo[i,j])/2
      }
      newrojo[i,j+1] = (sample(rojo[(i-1):(i+1), j:(boxdim1)],1)  + rojo[i,j+1])/2
    }
    for(j in 1:(boxdim1-1)){
      newrojo[boxdim1,j] =( sample(rojo[(boxdim1-1):(boxdim1), j-1:j+1],1) +  rojo[boxdim1,j])/2
    }
    newrojo[i+1,j+1] = (sample(rojo[(boxdim1-1):boxdim1, boxdim1-1:boxdim1],1) + rojo[i+1,j+1] )/2
    rojo = newrojo
  }
  return(rojo)
}


rojo = spreader(100, 0.99, 100)
verde=  spreader(100, 0.9, 10)
azul =  spreader(100, 0.9, 10)

meltrojo = melt(rojo)
colnames(meltrojo) = c("x","y", "R")
meltverde = melt(verde)
colnames(meltverde) = c("x","y", "G")
meltazul = melt(azul)
colnames(meltazul) = c("x","y", "B")

rg = inner_join(meltrojo,meltverde, by = c("x","y"))
rgb = inner_join(rg,meltazul, by = c("x","y"))

ggplot(data=rgb, aes(x=x, y=y)) + 
  geom_tile(fill = rgb(rgb$R,rgb$G,rgb$B)) +
  # scale_color_identity() + 
  theme(legend.position = "none") +
  theme_nothing()+
  scale_color_manual(values = rgb(rgb$R,rgb$G,rgb$B))


#dev.off()

my_palette = viridis(256)
gray = (rojo+verde+azul)/3
meltgray = melt(gray)
colnames(meltgray) = c("x","y", "gray")

ggplot(data=meltgray, aes(x=x, y=y, fill = gray)) + 
  geom_tile() +
  theme(legend.position = "none") +
  theme_nothing()+
  scale_fill_viridis(option = "B")
