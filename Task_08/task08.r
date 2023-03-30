setwd("/Users/taylorsexton/Desktop/Evolution/Tasks/Task_08")
library(phytools)
#Questions 1-3
trees <- list()
births <- c()
Fractions <- c()
random <- c()
random2 <- c()
treelist <- c()
for (i in 1:100){
  births[i]<- runif(1)
  Fractions[i]<- runif(1)
  trees[[i]] <- pbtree(n=100, b=births[i], d=births[i]*Fractions[i])
  random[[i]] <- births[i]
  random2[[i]] <- (Fractions[i])
  treelist[[i]] <- mean(trees[[i]]$edge.length)
}
#Question 4
# The net diversification increases as the log of the number of tips increases. The increase is an exponential curve.
div_rate <- sapply(trees, function(x) length(x$tip.label))
log_tips <- log(sapply(trees, function(x) length(x$tip.label)))
library(ggplot2)
ggplot(data = data.frame(div_rate, log_tips), aes(x= log_tips, y= div_rate))+
  geom_point()+
  xlab("Log Number of Tips")+
  ylab("Net Diversification")+
  ggtitle("Net Diversification vs. Log Number of Tips")
dev.off()
#Question 5
#This graph shows a slight decrease towards 0 in branch length as the speciation rate approaches 1.
speciation_rate <- sapply(births, function(x)x)
avg_branchlength <- sapply(trees, function(x) mean(x$edge.length, na.rm=TRUE))
ggplot(data = data.frame(speciation_rate, avg_branchlength), aes(x= speciation_rate, y=avg_branchlength))+
  geom_point()+
  xlab("Speciation Rate")+
  ylab("Average Branch Length")+
  ggtitle("Speciation Rate vs. Average Branch Length")
dev.off()
#Question 6
# The correlation between speciation rate and average branch length is -0.1702541 which means there is a weak negative relationship between the two variables.
cor(speciation_rate, avg_branchlength)
#Question 7
largest_tree <- trees[[which.max(sapply(trees, length))]]
Tree <- largest_tree
plot(Tree)
dev.off()
rates <- c()
traits <- list()
for( i in 1:100){
  rates[i]<- runif(1)
  traits[[i]]<- fastBM(tree=Tree, sig2=rates[i])
}

#Question 8
mean_traits <- sapply(traits, mean)
cor(mean_traits, rates)
#The correlation is -0.08890789 which means there is a weak negative correlation. The graph shows a negative correlation, but it seems more scattered.
plot(rates, mean_traits, xlab="Rates", ylab="Mean Trait")
dev.off()
#Question 9
var_traits <- sapply(traits, var)
cor(var_traits, rates)
# The correlation is 0.7809559 which means there is a strong positive correlation. The graph also shows a positive correlation.
plot(rates, var_traits, xlab="Rates", ylab="Variance of Traits")
dev.off()
#Question 10
cor(traits[[1]], traits[[2]])
#The correlation is 0.03994475 which means there is a weak correlation. 
traitMat <- cbind(traits[[1]], traits[[4]])
plot(traitMat)
dev.off()

#Extra Credit
phylomorphospace(Tree, traitMat, xlab="Trait 1", ylab= "Trait 2")
dev.off()

