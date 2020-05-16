source("R Code/dataModel.R")

#### Heat Maps ####
x <- seq(-5,5,by=0.05)
y <- seq(0,8,by=0.05)
mod_fit2 <- glm(as.factor(Goal)~distance+I(abs(angle))+Hand+Type+Defense+Off.Scenario,  
								data=dataset, family="binomial")
#### Heat maps based on data ####


# Overall 
g1=ggplot(dataset,aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[5:12]) + 
	scale_y_continuous(breaks=c(2,5),limits = c(0,10))+scale_x_continuous(breaks=c(-1.5,1.5),limits=c(-5,5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("")+ylab("")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Shots")
g2=ggplot(filter(dataset,Result=="Goal"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[5:12]) + 
	scale_y_continuous(breaks=c(2,5),limits = c(0,10))+scale_x_continuous(breaks=c(-1.5,1.5),limits=c(-5,5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("")+ylab("")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Goals")

plot_grid(g1,g2,nrow=1)


# By Hand 
g1=ggplot(filter(dataset,Hand %in% c("Right")),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(limits=c(0,0.1), colors=rev(rainbow(12))[4:12]) + 
	scale_y_reverse(breaks=c(2,5),limits = c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),limits=c(5,-5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("")+ylab("")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Right Hand Shots")
g2=ggplot(filter(dataset,Result=="Goal",Hand %in% c("Right")),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(limits=c(0,0.1), colors=rev(rainbow(12))[4:12]) + 
	scale_y_reverse(breaks=c(2,5),limits = c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),limits=c(5,-5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("")+ylab("")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Right Hand Goals")
g3=ggplot(filter(dataset,Hand %in% c("Left")),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(limits=c(0,0.12), colors=rev(rainbow(12))[4:12]) + 
	scale_y_reverse(breaks=c(2,5),limits = c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),limits=c(5,-5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("")+ylab("")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Left Hand Shots")
g4=ggplot(filter(dataset,Result=="Goal",Hand %in% c("Left")),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(limits=c(0,0.12), colors=rev(rainbow(12))[4:12]) + 
	scale_y_reverse(breaks=c(2,5),limits = c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),limits=c(5,-5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("")+ylab("")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Left Hand Goals")
library(cowplot)
plot_grid(g1,g2,g3,g4, ncol=2, nrow=2)


# By Offensive SCenario 

g1=ggplot(filter(dataset,Off.Scenario=="Penalty"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[5:12],guide="none") + 
	scale_y_continuous(breaks=c(2,5),limits = c(0,10))+scale_x_continuous(breaks=c(-1.5,1.5),limits=c(-5,5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("Meters Left/Right from Center Cage")+ylab("Vertical Distance from Center Cage")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Shots")+facet_grid(.~Off.Scenario)

g2=ggplot(filter(dataset,Result=="Goal",Off.Scenario=="Penalty"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[5:12],guide="none") + 
	scale_y_continuous(breaks=c(2,5),limits = c(0,10))+scale_x_continuous(breaks=c(-1.5,1.5),limits=c(-5,5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("Meters Left/Right from Center Cage")+ylab("Vertical Distance from Center Cage")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Goals")+facet_grid(.~Off.Scenario)

plot_grid(g1,g2,nrow=2)


# Offense and Defense

g1=ggplot(filter(dataset,Off.Scenario!="Penalty",Defense!="Blocker at Distance"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[5:12],guide="none") + 
	scale_y_continuous(breaks=c(2,5),limits = c(0,10))+scale_x_continuous(breaks=c(-1.5,1.5),limits=c(-5,5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("Meters Left/Right from Center Cage")+ylab("Vertical Distance from Center Cage")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Shots")+facet_grid(Off.Scenario~Defense)


g2=ggplot(filter(dataset,Off.Scenario!="Penalty",Result=="Goal",Defense!="Blocker at Distance"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[5:12],guide="none") + 
	scale_y_continuous(breaks=c(2,5),limits = c(0,10))+scale_x_continuous(breaks=c(-1.5,1.5),limits=c(-5,5))+
	theme(axis.ticks=element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"),panel.background = element_rect(fill = rev(rainbow(12))[4]),panel.grid.major = element_line(color="black")) + 
	stat_density2d(color="white")+xlab("Meters Left/Right from Center Cage")+ylab("Vertical Distance from Center Cage")+coord_fixed()+
	theme(strip.placement = "outside", plot.title = element_text(hjust = 0.5))+ggtitle(label="Goals")+facet_grid(Off.Scenario~Defense)

plot_grid(g1,g2,nrow=1)

### Figure 2 ####

shotTrackerXY= filter(dataset,Hand=="Right")
shots=length(shotTrackerXY$Position)
goals=sum(shotTrackerXY$Result=="Goal")
g1=ggplot(shotTrackerXY,aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[4:12],guide="none") +
	scale_y_reverse(breaks=c(2,5),expand = c(0, 0),limits=c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),expand = c(0, 0),limits=c(5,-5),position = "top")+
	theme(axis.ticks=element_blank(),
				plot.margin = unit(c(0, 0, 0, 0), "cm"),
				panel.background = element_rect(fill = rev(rainbow(12))[4]),
				panel.grid.major = element_line(color="black"),
				panel.grid.minor=element_blank(),
				axis.text=element_text(size=8,margin=margin(0,0,0,0)),
				axis.title=element_text(size=8,margin=margin(0,0,0,0))) + 
	stat_density2d(color="white")+xlab("Right Hand Shots")+ylab("")+coord_fixed()


g2=ggplot(filter(shotTrackerXY,Result=="Goal"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[4:12],guide="none")+
	scale_y_reverse(breaks=c(2,5),expand = c(0, 0),limits=c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),expand = c(0, 0),limits=c(5,-5),position = "top")+
	theme(axis.ticks.y=element_blank(),
				plot.margin = unit(c(0, 0, 0, 0), "cm"),
				panel.background = element_rect(fill = rev(rainbow(12))[4]),
				panel.grid.major = element_line(color="black"),
				panel.grid.minor=element_blank(),
				axis.text=element_text(size=8,margin=margin(0,0,0,0)),
				axis.title=element_text(size=8,margin=margin(0,0,0,0))) + 
	stat_density2d(color="white")+xlab("Right Hand Goals")+ylab("")+coord_fixed()

shotTrackerXY= filter(dataset,Hand=="Left")
shots=length(shotTrackerXY$Position)
goals=sum(shotTrackerXY$Result=="Goal")
g3=ggplot(shotTrackerXY,aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[4:12],guide="none") +
	scale_y_reverse(breaks=c(2,5),expand = c(0, 0),limits=c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),expand = c(0, 0),limits=c(5,-5),position = "top")+
	theme(axis.ticks=element_blank(),
				plot.margin = unit(c(0, 0, 0, 0), "cm"),
				panel.background = element_rect(fill = rev(rainbow(12))[4]),
				panel.grid.major = element_line(color="black"),
				panel.grid.minor=element_blank(),
				axis.text=element_text(size=8,margin=margin(0,0,0,0)),
				axis.title=element_text(size=8,margin=margin(0,0,0,0))) + 
	stat_density2d(color="white")+xlab("Left Hand Shots")+ylab("")+coord_fixed()


g4=ggplot(filter(shotTrackerXY,Result=="Goal"),aes(x=xj,y=yj))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colors=rev(rainbow(12))[4:12],guide="none")+
	scale_y_reverse(breaks=c(2,5),expand = c(0, 0),limits=c(8,0))+
	scale_x_reverse(breaks=c(-1.5,1.5),expand = c(0, 0),limits=c(5,-5),position = "top")+
	theme(axis.ticks.y=element_blank(),
				plot.margin = unit(c(0, 0, 0, 0), "cm"),
				panel.background = element_rect(fill = rev(rainbow(12))[4]),
				panel.grid.major = element_line(color="black"),
				panel.grid.minor=element_blank(),
				axis.text=element_text(size=8,margin=margin(0,0,0,0)),
				axis.title=element_text(size=8,margin=margin(0,0,0,0))) + 
	stat_density2d(color="white")+xlab("Left Hand Goals")+ylab("")+coord_fixed()

# 400 x 325
plot_grid(g1,g2,g3,g4)

### Figure 3 ####
x <- seq(-5,5,by=0.05)
y <- seq(0,8,by=0.05)
mod_fit2 <- glm(as.factor(Goal)~distance+I(abs(angle))+Hand+Type+Defense+Off.Scenario,  
								data=dataset, family="binomial")

grid <- expand.grid(x=x, y=y)
grid=mutate(grid,distance=sqrt(x^2+y^2),angle=atan(x/y))
n=dim(grid)[1]
newData=data.frame(yj=grid$y,xj=grid$x,distance=grid$distance,angle=grid$angle,Hand=rep("Left",n),Type=rep("Skip",n),Defense=rep("Uncontested",n),Off.Scenario=rep("Counter",n))
newData2=data.frame(yj=grid$y,xj=grid$x,distance=grid$distance,angle=grid$angle,Hand=rep("Left",n),Type=rep("Skip",n),Defense=rep("Field Blocker",n),Off.Scenario=rep("Counter",n))
newData3=data.frame(yj=grid$y,xj=grid$x,distance=grid$distance,angle=grid$angle,Hand=rep("Left",n),Type=rep("Skip",n),Defense=rep("Pressured",n),Off.Scenario=rep("Counter",n))
newData4=data.frame(yj=grid$y,xj=grid$x,distance=grid$distance,angle=grid$angle,Hand=rep("Left",n),Type=rep("Skip",n),Defense=rep("Contested",n),Off.Scenario=rep("Counter",n))
newdata=rbind(newData,newData2,newData3,newData4)
newdataB=newdata
newdataB$Off.Scenario="Power Play"
newdataC=newdata
newdataC$Off.Scenario="Even"
newdata=rbind(newdata,newdataB,newdataC)
newdata$prob=predict(mod_fit2,newdata=newdata,type = "response")
ggplot(newdata,aes(x=xj,y=yj))+geom_raster(aes(fill=prob))+
	scale_fill_gradientn(limits=c(0,1),colors=rev(rainbow(12))[4:12])+
	scale_y_reverse(breaks=c(2,5),expand = c(0, 0),name="Vertical Distance from Goal Line")+
	scale_x_reverse(breaks=c(-1.5,1.5),position="top",expand = c(0, 0),name="Horizontal Displacement from Centre Cage")+
	theme(axis.ticks=element_blank(),panel.grid = element_blank(), 
				axis.title=element_text(size=8),
				axis.text=element_text(size=8),
				strip.text=element_text(size=8),
				panel.grid.major = element_line(color="black"),
				panel.border = element_blank(),  panel.background = element_rect(fill = NA),
				panel.ontop = TRUE,plot.margin=grid::unit(c(0,0,0,0), "mm")) +
	facet_grid(Off.Scenario~Defense,switch="x")+theme(strip.placement = "outside",
																										plot.title = element_text(hjust = 0.5))+ggtitle(label="Women's Left-Handed Skip")
