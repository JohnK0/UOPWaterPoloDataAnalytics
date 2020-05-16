library(magrittr)
library(dplyr) 
shotTracker=read.csv("shotTracker88.csv")

xy=read.csv("xyCoord.csv")


shotTracker$x=NA
shotTracker$y=NA

for (i in 1:length(xy$Position)){
  shotTracker[grepl(xy$Position[i],shotTracker$Position),c("x","y")]=xy[i,c("x","y")]
}


shotTracker$xj=shotTracker$x+runif(length(shotTracker$Position),min=-0.5,max=0.5)
shotTracker$yj=shotTracker$y+runif(length(shotTracker$Position),min=-0.5,max=0.5)

shotTracker=mutate(shotTracker,distance=sqrt(x^2+y^2),angle=atan(x/y))

write.csv(shotTracker,"Men_Shots2.csv")
