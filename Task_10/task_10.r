setwd('~/Users/taylorsexton/Desktop/Evolution/Tasks/Task_10')
x <- rnorm(n = 100, mean = 0, sd = 2)
x
y <- x*5+2+runif(100, min = 0, max = 0.1)
y
model <- lm(y~x)
summary(model)
plot(model)
slope <- vector("numeric", 100)
intercept <- vector("numeric", 100)
z <- vector("numeric", 100)
for (i in 1:100){
  x <- rnorm(100, mean=0, sd=2)
  y <- x * 5 + 2 + runif(100, min=0, max=0.1)
  z[i] <- runif(1, min=0.5, max=2)
  model <- lm(y~x)
  intercept[i] <- coef(model)[1]*z[i]+2
  slope[i] <- coef(model)[2]*z[i]
}
plot(z, slope, xlab = "Z Estimated Slope")
abline(lm(slope~z), col = "purple")
#Extra Credit 1
n <- 10000
prize <- sample(c("1", "2", "3"), size = n, replace = TRUE)
opened_door <- ifelse(prize == "1", sample(c("2", "3"), size = n, replace = TRUE), ifelse(prize == "2", "3", "2"))
closed_door <- ifelse(opened_door == "2", "3", "2")
same_door <- sum(prize =="1")/n
diff_door <- sum(prize == closed_door)/n
win_frequency <- c(same_door, diff_door)
barplot(win_frequency, names.arg = c("Same Door", "Different Door"), ylab ="Frequency of Wins (10,000 Runs)", ylim = c(0, 0.8), main = "Chance of Winning the Grand Prize", col = "turquoise")
#Extra Credit 2
library(meme)
image <- "https://cdn.pixabay.com/photo/2022/03/18/23/05/horse-7077606_1280.jpg"
meme(image, "Dr. Mitchell when he grades my exam", size=2)
