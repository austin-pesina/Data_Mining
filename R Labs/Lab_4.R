library (ISLR)
names(Smarket)

pairs(Smarket)
cor(Smarket)
#error 'x" must be numeric

cor(Smarket[,-9])


attach(Smarket)
plot(Volume)


#Logistic Regressions


glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family = binomial)
summary(glm.fit)

#direction and magnitude of each
coef(glm.fit)
#    or
summary(glm.fit)$coef

summary(glm.fit)$coef[,4]


#Probability stock market goes up or down
glm.probs <- predict(glm.fit, type='response')
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down", 1250)
glm.pred[glm.probs > 0.5]="Up"
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred==Direction)

train <-(Year < 2005)
head(train)
tail(train)
Smarket.2005 <- Smarket[!train,]
dim(Smarket)
dim(Smarket.2005)
Direction.2005 <-Direction[!train]

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, subset = train, family = binomial)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5]="Up"
table (glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)




#Linear Discriminant Analysis

library(MASS)

lda.fit<- lda(Direction ~ Lag1 +Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$poserior[,1]>0.9)



#Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~ Lag1 +Lag2, data = Smarket, subset = train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
table (qda.class, Direction.2005)
mean(qda.class==Direction.2005)


#KNN

library(class)

train.x <- cbind(Lag1, Lag2)[train,]
test.x <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

dim(train.x)
dim(test.x)
length(train.Direction)

set.seed(1)
knn.pred <- knn(train.x, test.x, train.Direction, k=1)
table(knn.pred, Direction.2005)

knn.pred <- knn(train.x, test.x, train.Direction, k=3)
table(knn.pred, Direction.2005)

knn.pred <- knn(train.x, test.x, train.Direction, k=10)
table(knn.pred, Direction.2005)
