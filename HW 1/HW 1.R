library(ISLR)
data(College)
college <- read.csv("College.csv", header=T)

head(college[, 1:5])

rownames <- college[,1]
fix(college)
college <- college[,-1]
fix(college)

college$Private<-as.factor(college$Private)

summary(college)

pairs(college[, 1:10])

plot(college$Private, college$Outstate, xlab = "Private University",
     ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")

Elite <-rep("No", nrow(college))
Elite [college$Top10perc >50] = "Yes"
Elite <-as.factor(Elite)
college <- data.frame(college, Elite)
fix(college)
summary(college$Elite)

plot(college$Elite, college$Outstate, xlab = "Elite University",
     ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")

par(mfrow = c(2,2))
hist(college$Books, col = 2, xlab = "Books", ylab = "Count")
hist(college$PhD, col = 3, xlab = "PhD", ylab = "Count")
hist(college$Grad.Rate, col = 4, xlab = "Grad Rate", ylab = "Count")
hist(college$perc.alumni, col = 6, xlab = "% alumni", ylab = "Count")

summary(college$PhD)
