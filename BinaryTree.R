library(ISLR)
library(tree)

attach(Carseats)

High = ifelse(Sales >=8, 'Yes', "No")
Carseats = data.frame(Carseats, High)

set.seed(2)
Carseats = Carseats[,-1]
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]
testing_data = Carseats[test]
testing_High = High[test]

tree_model =  tree(High~., training_data)
plot(tree_model)
text(tree_model, pretty = 0)

tree_pred = predict(tree_model, testing_data, type = "class")

mean(tree_pred != testing_High)

#Pruning
#Cross validation
set.seed(2)
cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type = "b")

prune_model = prune.misclass(tree_model, best = 9)
plot(prune_model)
text(prune_model, pretty = 0)

#Predictions
tree_pred = predict(prune_model, testing_data, type = "class")
mean(tree_pred != testing_High)


###########
setwd("/Users/p.wainschtein/Documents/DEPICT/DEPICT/PD")
assoc <- read.csv("associations.csv")
assoc <- assoc[order(assoc$p.value.isva1),]
assoc <- data.frame(assoc, getNearestTranscript(hm450[as.vector(assoc$X)]))
assoc <- data.frame(assoc, getNearestTSS(hm450[as.vector(assoc$X)]))
names(assoc)[1]<-"Probe"
write.csv(assoc, file = "EWAS_Genes_TSS.csv", row.names = FALSE)
