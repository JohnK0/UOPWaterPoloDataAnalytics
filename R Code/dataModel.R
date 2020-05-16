source("R Code/dataExploration.R")

#### Logistic Model ####
library(caret)
library(lattice)
library(ggplot2)
set.seed(39)
Train=createDataPartition(dataset$Goal,p=0.6,list=FALSE)
training=dataset[Train,]
testing=dataset[-Train,]

training = training %>% mutate(Goal=factor(training$Goal),
															 absangle=I(abs(angle)),
															 Side=as.factor(I(sign(angle))))
levels(training$Side)=c("Left","Right")

testing = testing %>% mutate(Goal=factor(testing$Goal),
														 absangle=I(abs(angle)),
														 Side=as.factor(I(sign(angle))))
levels(testing$Side)=c("Left","Right")

mod_fitT <- train(as.factor(Goal)~distance+absangle+Hand+Side+Type+Defense+Off.Scenario+Gender,  
									data=training , method="glm", family="binomial")
summary(mod_fitT)

# One problem with this is that it does a really nice job of predicting
# Misses, but not so well at Goals. I think using ROC or maybe a different RF model
# WOuld Improve this..

testing$pred=predict(mod_fitT,newdata=testing,type="raw")
confusionMatrix(testing$pred,as.factor(testing$Goal),positive="TRUE")


# Model for paper using full dataset:

mod_fit <- glm(as.factor(Goal)~distance+I(abs(angle))+
							 	as.factor(I(sign(angle)))+Hand+Type+
							 	Defense+Off.Scenario+Gender,  
							 data=dataset, family="binomial")
varImp(mod_fit)
summary(mod_fit)




# Angle: Distance model Acccuracy 72.16% (0.7017, 0.7409) for CI vs NIF of 66.76%

acc=table(testing$pred,testing$Goal)
sum(diag(acc))/sum(acc)

fisher.test(acc)


mod_fit <- glm(as.factor(Goal)~distance+I(abs(angle))+as.factor(I(sign(angle)))+Hand+Skip+Defense+Off.Scenario+Gender,  
							 data=dataset, family="binomial")
mod_fit2<-update(mod_fit,.~.+Hand*as.factor(I(sign(angle))))
mod_fit2<-update(mod_fit,.~.+distance*I(abs(angle)))
mod_fit3<-update(mod_fit2,.~.+Off.Scenario*Skip)
mod_fit3<-update(mod_fit,.~.+(yj+I(abs(xj)))^2)
varImp(mod_fit)
anova(mod_fit,mod_fit2,test="LRT")
anova(mod_fit2,mod_fit3,test="LRT")
summary(mod_fit2)
library(car)
vif(mod_fit2)

dataset$Shot.Quality=predict(mod_fit,data=dataset,type="response")

summary(mod_fit)
newdata=data.frame(distance=mean(dataset$distance),angle=mean(dataset$angle),Hand="Left",Skip=FALSE,
									 Defense="Uncontested",
									 Off.Scenario="Even", Gender="Male")
predict(mod_fit,newdata,type="response")

exp(coef(mod_fit))
(2.7794/2.0833-1)*100
round(100*(1 - exp(pi/180*coef(mod_fit))),0)

round(100*(exp(abs(coef(mod_fit)))-1),0)

# n=length(oddsCh)
# d=data.frame(Scenario=c("Move Back 1m","Move Left/Right 1m","Right Hander","Not Skip Shot","Blocker at Distance","Field Blocker","Pressured",
#                         "Contested","Group B Tactic","Group C Tactic","Group D Tactic"),Odds_Percent_Decrease=round(oddsCh[2:n],1))
# row.names(d)=1:(n-1)
# d

#### Expected Goals ####

shotTracker=dataset
shotTracker$Shot.Quality=predict(mod_fit,newdata=shotTracker,type="response")