setwd("/Users/taylorsexton/Desktop/Evolution/Tasks/Project")
install.packages("readxl")
library("readxl")
read_excel("/Users/taylorsexton/Desktop/Evolution/Tasks/Project/Relationships in Squirrels/SizeAndShapeData.xlsx")
my_data <- read_excel("/Users/taylorsexton/Desktop/Evolution/Tasks/Project/Relationships in Squirrels/SizeAndShapeData.xlsx")
squirrel_data <- read_excel("~/Desktop/Evolution/Tasks/Project/Relationships in Squirrels/WholeSquirrelData.xlsx")
squirrel_data2 <- read_excel("~/Desktop/Evolution/Tasks/Project/Relationships in Squirrels/SquirrelData.xlsx")
location_data <- read_excel("~/Desktop/Evolution/Tasks/Project/Relationships in Squirrels/LocationData.xlsx")
#Graph
library("ggplot2")
summary(squirrel_data)
str(squirrel_data)
ggplot(squirrel_data, aes(Species,CS))+
  geom_point()+
  ggtitle("Mandibular Centroid Sizes of Different Squirrel Species")+
  labs(y="Centroid Size", x="Species")
barplot(squirrel_data2, main="Average Mandibular Sizes of Different Squirrel Species", ylab="Average Centroid Size", x=lab="Species", las=2)

#Dunn Test
library(readr)
install.packages("dunn.test")
library(dunn.test)
dunn.test(x= squirrel_data$CS, g= squirrel_data$Species)

data.frame(Species = squirrel_data$Species, CS = squirrel_data$CS)
dunn_results <- dunn.test(x = squirrel_data$CS, g = squirrel_data$Species, method ="bonferroni")
print(dunn_results)


pairs_df <- as.data.frame(pairs)
pairs
colnames(pairs_df) <- c("Species", "CS", "Adjusted p-value")
pairs_df
library(dplyr)
pairs <- dunn_results$results[, 1:2]
pairs$'Adjusted p-value' <- dunn_results$adjusted.p
colnames(pairs) <- c("Species", "CS", "Adjusted p-value")
pairs

#Statistical Test **Is there a significant difference between the genera
kruskal.test(CS~Species, data=squirrel_data)

#Kruskal-Wallis Rank Sum Test
  #chi-squared=1655.5
  #df= 183
  #p-value <2.2e-16 = 0.00000000000000022
#Since the p-value is less than 0.05, there is a significant difference in the centroid sizes between the species of squirrels.


#Hypothesis
#There is a significant difference between genera in the mandibular centroid size in the squirrels. 
#Null Hypothesis would be there is not a significant difference.
#Reject Null Hypothesis










