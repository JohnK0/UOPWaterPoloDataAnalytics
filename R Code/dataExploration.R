source("dataClean.R")

#### Men ####
m = dataClean(fileName = "Men_Shots.csv")
m$readFile()
m$cleanDataset()
m$dataset = m$dataset[complete.cases(m$dataset),]
m$dataset$Gender = factor("Male")
men_dataset_size = nrow(m$dataset) 
print("Created Men's dataset")

#### Women ####
w = dataClean(fileName = "Women_Shots.csv")
w$readFile()
w$cleanDataset()
m$dataset = m$dataset[complete.cases(m$dataset),]
w$dataset$Gender = factor("Female")
women_dataset_size = nrow(w$dataset) 
print("Created Women's dataset")

#### Combined Dataset ####
dataset = rbind(m$dataset, w$dataset)
print("Combined datasets")

#### Exploration ####
by_percentage = function(table, men_dataset_size, women_dataset_size) {
	for (row in 1:nrow(table)) {
		table[row,1] <- round(table[row,1]/men_dataset_size,2)
		table[row,2] <- round(table[row,2]/women_dataset_size,2)
	}
	return (table)
}

print("Explores datasets' ratios")
# Gender #
table(dataset$Gender)
# Defense #
defense = by_percentage(table(dataset$Defense, dataset$Gender), men_dataset_size, women_dataset_size)
# Offensive Scenario #
off.scenario = by_percentage(table(dataset$Off.Scenario, dataset$Gender), men_dataset_size, women_dataset_size)
# Hand #
hand = by_percentage(table(dataset$Hand, dataset$Gender), men_dataset_size, women_dataset_size)
# Type #
type = by_percentage(table(dataset$Type, dataset$Gender), men_dataset_size, women_dataset_size)
# Goals #
goals = by_percentage(table(dataset$Goal, dataset$Gender), men_dataset_size, women_dataset_size)

# prints the mean and standard deviation of the distance and angle #

c(mean(abs(dataset$distance)),sd(abs(dataset$distance)))
c(mean(abs(dataset$angle)),sd(abs(dataset$angle)))

sum(dataset$Goal==TRUE)/length(dataset$X)   

# Prints the results of the LEFT hand #
colSums(table(dataset$Result,dataset$Hand, dataset$Gender))
table(dataset$Result,dataset$Hand)

