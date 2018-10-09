##############################################################################################
######### Bayes Classifier #########
##############################################################################################

# 1 - on tire une realisation de bernoulli
install.packages("mvtnorm")
library(mvtnorm)


##########################
# S'amuser a changer les parametres
mu1 <- c(1.5,1)
mu2 <- c(1,1.5)

sigma1 <- diag(c(0.07,0.06))
sigma2 <- diag(c(0.05,0.05))
##########################
p <- 1/3
n <- 2500
Y <- rbinom(n,1,p)+1
Y <- sort(Y)
groupe1 <- Y[which(Y==1)]
groupe2 <- Y[which(Y==2)]

colnames(Y) <- c("Données","Bayes")
head(Y)


rea1 <- rmvnorm(length(groupe1),mu1,sigma1)
rea2 <- rmvnorm(length(groupe2),mu2,sigma2)

X1 <- c(rea1[,1],rea2[,1])
X2 <- c(rea1[,2],rea2[,2])
lesX <- cbind(X1,X2)

X <- matrix(data = c(rep(1,length(X1)),X1,X2), ncol = 3)

Betachap <- solve(t(X)%*%X) %*% t(X) %*% Y
head(Y)
lm(Y ~ X1+X2)
Betachap
Y <-cbind(Y,rep(0,length(Y)))
plot(X[,2:3])
plot(subset(X[,2:3],Y[,1]==1),col="green3",pch=20, xlab = "X1", ylab = "X2",xlim = c(-.5,3.5), ylim = c(-0.1,3.5))
points(subset(X[,2:3],Y[,1]==2),col="red",pch=17)


# Calculer et representer le classifieur de bayes


g1 <- (X1[which(Betachap[1] + Betachap[2]*X1 + Betachap[3]*X2 < 3/2)])

g <- Betachap[1] + Betachap[2]*X1 + Betachap[3]*X2 < 3/2
for(i in 1:length(g))
{
  if(g[i])
    {Y[i,2]<-1}
  else{Y[i,2]<-2}
}

points(subset(X[,2:3],Y[,2]==1),col="blue3",pch=20)
points(subset(X[,2:3],Y[,2]==2),col="yellow3",pch=17)

toutlesX1 <- seq(-5,25,0.1)
lines( ( (3/2) - Betachap[1]-Betachap[3]*toutlesX1) / Betachap[2],toutlesX1,col="blue")

  # Cas Bayésien
        #1 si E(Y=1 | (X1,X2)=x1,x2) < 3/2
        #2 sinon



##############################################################################################
### New Datas ! ###
##############################################################################################
library(class)
################################
ntrain <- 500
Ytrain <- rbinom(ntrain,1,p)+1
Ytrain <- sort(Ytrain)
groupe1test <- Y[which(Ytrain==1)]
groupe2test <- Y[which(Ytrain==2)]

echtrain1 <- rmvnorm(length(groupe1test),mu1,sigma1)
echtrain2 <- rmvnorm(length(groupe2test),mu2,sigma2)

Xtrain1 <- c(echtrain1[,1],echtrain2[,1])
Xtrain2 <- c(echtrain1[,2],echtrain2[,2])
lesXtrain <- cbind(Xtrain1,Xtrain2)
ratio.old <- NULL
ratio <- NULL
nbfaux <- NULL
for(i in 1:25)
{
  for(j in 1:10) {
    ppv <- knn(lesXtrain,lesX,Ytrain,k=i)


    classement <- (ppv == Y[,1])
    nbfaux[j] <- length(classement[which(classement == F)])
  }
    nbfauxmoy <- mean(nbfaux)
    ratio <- nbfauxmoy/n
    ratio
    ratio.old[i] <- ratio
}
min(ratio.old)
indice <- order(ratio.old)[1]
indice




ppv.optimal <- knn(lesXtrain,lesX,Ytrain,k=indice)
table(ppv.optimal,Y[,1])

plot(subset(X[,2:3],Y[,1]==1),col="green3",pch=20,cex=0.75, xlab = "X1", ylab = "X2",xlim = c((min(lesX)-0.1),(max(lesX)+0.1)), ylim = c((min(lesX)-0.1),(max(lesX)+0.1)))
points(subset(X[,2:3],Y[,1]==2),col="red",pch=19,cex=0.75)

plot(subset(X[,2:3],ppv.optimal==1),col="green3",pch=20,cex=0.65, xlab = "X1", ylab = "X2",xlim = c((min(lesX)-0.1),(max(lesX)+0.1)), ylim = c((min(lesX)-0.1),(max(lesX)+0.1)))
points(subset(X[,2:3],ppv.optimal==2),col="blue",pch=20,cex=0.65)

points(subset(X[,2:3],ppv.optimal!=Y[,1]),col="red",pch=21,cex=0.95)
lines( ( (3/2) - Betachap[1]-Betachap[3]*toutlesX1) / Betachap[2],toutlesX1,col="brown")


