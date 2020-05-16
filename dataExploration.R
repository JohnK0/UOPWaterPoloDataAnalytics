source("dataClean.R")

#### Men ####
m = dataClean(fileName = "Men_Shots.csv")
m$readFile()
m$cleanDataset()
m$dataset$Gender = factor("Male")
print("Created Men's dataset")

#### Women ####
w = dataClean(fileName = "Women_Shots.csv")
w$readFile()
w$cleanDataset()
w$dataset$Gender = factor("Female")
print("Created Women's dataset")

#### Combined Dataset ####
dataset = rbind(m$dataset, w$dataset)
print("Combined datasets")

#### Exploration ####
print("Explores datasets")
# Gender #
table(dataset$Gender)
# Defense #
table(dataset$Defense)
# Offensive Scenario #
table(dataset$Off.Scenario)

# prints the mean and standard deviation of the distance and angle #

c(mean(abs(dataset$distance)),sd(abs(dataset$distance)))
c(mean(abs(dataset$angle)),sd(abs(dataset$angle)))

sum(dataset$Goal==TRUE)/length(dataset$X)   

# Prints the results of the LEFT hand #
colSums(table(dataset$Result,dataset$Hand))
table(dataset$Result,dataset$Hand)