#####################################
# Weekly Analysis ########
#####################################
#install.packages("ISLR")
#library(ISLR)
attach(Weekly)
head(Weekly)
summary(Weekly)
head(Direction)
contrasts(Direction)
mlg<-glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
         data = Weekly, 
         family = binomial)
summary(mlg)

predictions <- predict(mlg,type = "response")
UoD <- as.vector(ifelse(predictions > 0.5,"Up","Down"))
head(UoD)
tab <- table(UoD,Direction)
tab

malclasse <- tab[1,2]+tab[2,1]
bienclasses <- tab[2,2]+tab[1,1]
ratioMal <- malclasse/(malclasse+bienclasses) *100
ratioBien <- bienclasses/(malclasse+bienclasses) *100
ratioMal
ratioBien

mean(UoD == Direction)

###################################################
#####
table(Weekly$Year)
secWeek <- Weekly[which(Year<2009),]
mlg_test<-glm(Direction ~ Lag2, data = secWeek, family = binomial)
summary(mlg_test)

predictions_test <- as.vector(predict(mlg_test,type="response"))
UoD_test <- as.vector(ifelse(predictions_test > 0.5,"Up","Down"))


tab <- table(secWeek$Direction,UoD_test)

malclasse <- tab[1,2]+tab[2,1]
bienclasses <- tab[2,2]+tab[1,1]
malclasse/(malclasse+bienclasses) *100
bienclasses/(malclasse+bienclasses) *100

mean(UoD_test == secWeek$Direction)
#################
library(MASS)
z <-lda(Direction~Lag2)
w <- predict(z,Direction)$class
tab <- table(w,Direction)

malclasse <- tab[1,2]+tab[2,1]
bienclasses <- tab[2,2]+tab[1,1]
malclasse/(malclasse+bienclasses) *100
bienclasses/(malclasse+bienclasses) *100
###################
#
z <- qda(Direction~Lag2)
w <- predict(z,Direction)$class
tab <- table(w,Direction)

malclasse <- tab[1,2]+tab[2,1]
bienclasses <- tab[2,2]+tab[1,1]
malclasse/(malclasse+bienclasses) *100
bienclasses/(malclasse+bienclasses) *100
##########################
library(class)
train = c(Lag2)
test = c(Lag2)
length(train)
length(test)
length(Lag2)
ppv <- knn(train=Lag2,
           test=Lag2,
           cl = Direction,
           prob = T,
           k=2)
klag <- as.vector(Lag2)
dir <- ifelse(Direction=="Up",1,0)
detach(Weekly)
