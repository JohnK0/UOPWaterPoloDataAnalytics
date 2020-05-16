#### Load and Clean ####

# This chunk creates some new variables for use in the analysis based on the 
# output of the mergeXY file. 

shotTracker=read.csv("Men_Shots.csv")

library(dplyr)
# Cleans out some mislabeled Hand and Defense observations
possDef=c("Blocker at Distance", "Field Block", "Contested","Field Blocker","Pressured","Uncontested", "Other")
shotTrackerClean=filter(shotTracker,Hand %in% c("Right","Left"),
                        Defense %in% possDef) %>%
  droplevels() %>% mutate(Goal=Result=="Goal",
                          Skip=Type=="Skip",
                          distance=sqrt(xj^2+yj^2),angle=atan(xj/yj),
                          Off.Scenario=
                            as.factor(ifelse(Tactic=="Penalty","Penalty",
                                             ifelse(Tactic %in% c("6 on 5","6 on 4","5 on 5","Quick"),
                                                    "Power Play",ifelse(Tactic %in% c("Counter","Second Wave","First Wave"), "Counter","Even")))))

# Reorder Factor Levels
levels(shotTrackerClean$Defense)=c("Field Blocker", "Contested", "Field Blocker",
                                   "Field Blocker","Other", "Pressured",
                                   "Uncontested")

levels(shotTrackerClean$Type)=c("Normal","Backhand","Lob","Normal",
                                "Normal","Skip","Normal")
shotTrackerClean=
  mutate(shotTrackerClean$Defense=factor(shotTrackerClean$Defense,levels=levels(shotTrackerClean$Defense)[c(4,2,5,1,3)]),
         shotTrackerClean$Hand=factor(shotTrackerClean$Hand,levels=levels(shotTrackerClean$Hand)[c(2,1)]),
         shotTrackerClean$Off.Scenario=factor(shotTrackerClean$Off.Scenario,levels = levels(shotTrackerClean$Off.Scenario)[c(2,1,4,3)]))

write.csv(shotTrackerClean,"Shot Tracker/shotTrackerClean.csv")

print("Defense")
table(shotTrackerClean$Defense)
print("Offensive Scenario")
table(shotTrackerClean$Off.Scenario)

print("prints the mean and standard deviation of the distance and angle")

c(mean(abs(shotTrackerClean$distance)),sd(abs(shotTrackerClean$distance)))
c(mean(abs(shotTrackerClean$angle)),sd(abs(shotTrackerClean$angle)))


sum(shotTrackerClean$Goal==TRUE)/length(shotTrackerClean$X)   


print("Prints the results of the LEFT hand")
colSums(table(shotTrackerClean$Result,shotTrackerClean$Hand))
table(shotTrackerClean$Result,shotTrackerClean$Hand)



