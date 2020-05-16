library(magrittr)
library("dplyr") 

#### dataClean class ####
dataClean <- setRefClass("dataClean",
												 fields = list(
												 	dataset = "data.frame",
												 	fileName = "character"),
												 
												 method = list(
												 	initialize = function(fileName = NA_character_) {
												 		.self$fileName <<- fileName
												 	},
												 	
												 	readFile = function() {
												 		dataset <<- read.csv(fileName)
												 		dataset <<- dataset[c("Offensive.Team", "Position", "Location", "Tactic", "Type", "Hand", "Result", "Defense", "x", "y", "xj", "yj")]
												 	},
												 	
												 	combineDatasets = function(fileName, fileName2) {
												 		dataset1 <- read.csv(fileName)
												 		dataset2 <- read.csv(fileName2)
												 		dataset <<- rbind(dataset1, dataset2)
												 	},
												 	
												 	fixMislabels = function(def) {
												 		# Cleans out some mislabeled Hand and Defense observations
												 		dataset <<- filter(dataset,Hand %in% c("Right","Left"),
												 											 Defense %in% def) %>%
												 			droplevels() %>% mutate(Goal=Result=="Goal",
												 															Skip=Type=="Skip",
												 															distance=sqrt(xj^2+yj^2),angle=atan(xj/yj),
												 															Off.Scenario=
												 																as.factor(ifelse(Tactic=="Penalty","Penalty",
												 																								 ifelse(Tactic %in% c("6 on 5","6 on 4","5 on 5","Quick"),
												 																								 			 "Power Play",ifelse(Tactic %in% c("Counter","Second Wave","First Wave"), "Counter","Even")))))
												 	},
												 	
												 	reNameFactor = function(blockTypes, vector, default) {
												 		types <- list(levels(vector))
												 		for (i in 1:length(types[[1]])) {
												 			if (!(types[[1]][[i]] %in% blockTypes)) {
												 				types[[1]][[i]] <- default
												 			}
												 		}
												 		return (types[[1]])
												 	},
												 	
												 	reOrderFactors = function(vector, list) {
												 		return (factor(vector, levels = list))
												 	},
												 	
												 	cleanDataset = function() {
												 		###Here, assuming that Field Block and Field Blocker are the same defense types###
												 		def=c("Blocker at Distance", "Field Block", "Contested","Field Blocker","Pressured","Uncontested", "Other")
												 		fixMislabels(def)
												 		block = c("Field Blocker", "Contested","Uncontested", "Pressured", "Other")
												 		type = c("Normal","Backhand","Lob","Skip")
												 		hands = c("Left", "Right")
												 		off.scenario = c("Even", "Counter", "Power Play", "Penalty")
												 		levels(dataset$Defense) <<- reNameFactor(block, dataset$Defense, "Field Blocker")
												 		levels(dataset$Type) <<- reNameFactor(type, dataset$Type, "Normal")
												 		dataset$Defense <<- reOrderFactors(dataset$Defense, block)
												 		dataset$Type <<- reOrderFactors(dataset$Type, type)
												 		dataset$Hand <<- reOrderFactors(dataset$Hand, hands)
												 		dataset$Off.Scenario <<- reOrderFactors(dataset$Off.Scenario, off.scenario)
												 	}
												 )
)