setwd('~/Desktop/Evolution/Tasks/Task_02')
Data1 <- read.csv('https://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
Data2 <- read.csv('https://jonsmitchell.com/data/cyrus.csv', stringsAsFactors=F)
write.csv(Data1, 'rawdata.csv', quote=F)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3, 4]
Data1[1:5, 1:3]
Feeds <-(Data1[,9]=='bottle')
berenMilk <- Data1[Feeds,]
head(berenMilk)
Feeds <- which(Data1[,'event'] == 'bottle')
Feeds <- which(Data1$event == 'bottle')
dayID <- apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data1$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2 <- Data1
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds] , beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col= 'red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
source("https://jonsmitchell.com/code/plotFxn02b.R")
Mass <- which(Data1[,9]== 'trait_mass')
berenMass <- Data1[Mass,]
head(berenMass)
Mass <- which(Data1[,'event']== 'trait_mass')
Mass <- which(Data1$event == 'trait_mass')
dayID <-apply(Data1, 1, function(x) paste(x[1:3], collaspe='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m%d", origin = "2019-04-18")
head(Data1)
beren2 <-Data1
beren3 <-beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Mass <-which(beren3$event == "trait_mass")
avgMass <-mean(beren3$value[Mass])
avgMass <- tapply(beren3$value[Mass], beren3$age[Mass], mean)
varMass <- tapply(beren3$value[Mass], beren3$age[Mass], var)
totalMass <- tapply(beren3$value[Mass], beren3$age[Mass], sum)
numMass <- tapply(beren3$value[Mass], beren3$age[Mass], length)
cor(beren3$value[Mass], beren3$age[Mass])
cor.test(beren3$value[Mass], beren3$age[Mass])
berenCor <- cor.test(beren3$value[Mass], beren3$age[Mass])
summary(berenCor)
berenANOVA <- aov(beren3$value[Mass] ~ beren3$age[Mass])
boxplot( beren3$value[Mass] ~ beren3$age[Mass], xlab= "age(days)", ylab= "mass(kg)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="age in days", ylab="mass in kg")
abline(h=mean(totalMass), lty=2, col='red')
pdf("r02b-totalMassByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="age in day", ylab="mass in kg")
title("Beren Age vs Mass", xlab="age in day", ylab="mass in kg")
abline(h=mean(totalMass), lty=2, col='red')
head(Data2)
Mass <- which(Data2[,9]== 'trait_mass')
cyrusMass <- Data2[Mass,]
head(cyrusMass)
cMass <- which(Data2[,'event']== 'trait_mass')
cMass <- which(Data2$event=='trait_mass')
dayID <- apply(Data2, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin="2022-04-12")
Data2$age <- dateID - dateID[which(Data2$event=='birth')]
head(Data2)
cyrus2 <- Data2
cyrus3 <- cyrus2[order(cyrus2$age),]
cMass <-which(cyrus3$event == 'trait_mass')
avgcMass <-mean(cyrus3$value[cMass])
avgcMass <- tapply(cyrus3$value[cMass], cyrus3$age[cMass], mean)
varcMass <- tapply(cyrus3$value[cMass], cyrus3$age[cMass], var)
totalcMass <- tapply(cyrus3$value[cMass], cyrus3$age[cMass],sum)
numcMass <- tapply(cyrus3$value[cMass], cyrus3$age[cMass], length)
cor(cyrus3$value[cMass], cyrus3$age[cMass])
cor.test(cyrus3$value[cMass], cyrus3$age[cMass])
cyrusCor <- cor.test(cyrus3$value[cMass], cyrus3$age[cMass])
summary(cyrusCor)
cyrusANOVA <- aov(cyrus3$value[cMass] ~ cyrus3$age[cMass])
boxplot(cyrus3$value[cMass] ~ cyrus3$age[cMass], xlab= "age(days)", ylab= "mass(g)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalcMass)), totalcMass, type='b', pch=16, xlab="age in days", ylab="mass in grams")
abline(h=mean(totalcMass), lty=2, col='purple')
pdf("r02b-totalMassByDayCyrus.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalcMass)), totalcMass, type="b", pch=16, xlab="age in days", ylab="mass in grams")
title("Cyrus Age vs Mass", xlab="age in days", ylab="mass in grams")
abline(h=mean(totalcMass), lty=2, col='purple')
dev.off()


boxplot( beren3$value[Mass] ~ beren3$age[Mass], xlab= "age(days)", ylab= "mass(kg)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="age in days", ylab="mass in kg", ylim=c(0,20), col='green')
points(cyrus3$age[cMass], cyrus3$value[cMass]/1000, pch=16, col='purple')
title("Beren and Cyrus Age vs Mass")
legend(1000, 6, legend=c("Beren", "Cyrus"), col=c("green","purple"), lty=2:2, cex=0.8, title="Children", text.font=4)
dev.off()

#Extra Credit
unique(cyrus3$event)
which(cyrus3$event == 'trait_mass')
cMass <- which(cyrus3$event == 'trait_mass')
avgcMass <-mean(cyrus3$value[cMass])
avgGrowth <- tapply(cyrus3$value[cMass], cyrus3$age[cMass], mean)
varGrowth <- tapply(cyrus3$value[cMass], cyrus3$age[cMass], var)
totalGrowth <- tapply(cyrus3$value[cMass], cyrus3$age[cMass],sum)
numGrowth <- tapply(cyrus3$value[cMass], cyrus3$age[cMass], length)
cor(cyrus3$value[cMass], cyrus3$age[cMass])
cor.test(cyrus3$value[cMass], cyrus3$age[cMass])
cyrusreg <- lm(cyrus3$value[Mass] ~ cyrus3$age[Mass])
summary(cyrusreg)
attributes(cyrusreg)
plot(cyrus3$value[cMass]/1000 ~ cyrus3$age[Mass], col='pink', pch=16, xlab="Age (days)", ylab="Mass (kg)", ylim=c(0,15))

#I predict Cyrus will weigh 