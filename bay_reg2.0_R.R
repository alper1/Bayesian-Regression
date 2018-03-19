library(MCMCglmm)
library(MASS)
library(car)
library(VIM)
library(mlbench)
library(ggplot2)
library(labstatR)

data(BostonHousing)
head(BostonHousing)
dim(BostonHousing)
names(BostonHousing)
str(BostonHousing)
summary(BostonHousing)
pairs(BostonHousing[,-4])

boxplot(BostonHousing$crim)
hist(BostonHousing$crim,xlab ="Response Variabile")



ggplot(data=BostonHousing, aes(BostonHousing$crim)) + 
  geom_histogram(col="orange",
                 fill="blue",binwidth=10,
                 alpha = .2)+
  labs(title="Histogram for Crim") +
  labs(x="Crim", y="Frequency") +theme(plot.title = element_text(hjust = 0.5))
skew(BostonHousing$crim)

ggplot(data=BostonHousing, aes(log(BostonHousing$crim))) + 
  geom_histogram(breaks=seq(-6, 5, by=1), 
                 col="orange",
                 aes(y =..density..),
                 fill="blue", 
                 alpha=.2) +
  scale_fill_gradient("Count", low="orange", high="blue") +
  geom_density(col="orange")+ labs(title="Histogram for log(Crim)") +
  labs(x="log(Crim)", y="Frequency") +theme(plot.title = element_text(hjust = 0.5))



formula <- formula("log(crim) ~ zn + indus + nox + age + rad + ptratio + b + lstat")


hist(log(BostonHousing$crim),xlab = "Response Variabile")

bc<-boxcox(crim ~ .,data=BostonHousing)
bc$x[which.max(bc$y)]

#frequentist
freq.reg <- lm(log(crim) ~ .,data=BostonHousing)
freq.reg <- stepAIC(freq.reg,k = 0.8 * log(dim(BostonHousing)[1]))
#why have we chance the parameter of the penalization?

summary(freq.reg)
par(mfrow=c(2,2))
plot(freq.reg)
predict(freq.reg)
par(mfrow=c(1,1))
plot(BostonHousing$crim, predict(freq.reg),pch=16) #not linear
predict(freq.reg,interval="confidence")
predict(freq.reg,interval="prediction")
confint(freq.reg,level=.95)
summary(freq.reg)

set.seed(1)
bayes.reg <- MCMCglmm(formula,data=BostonHousing)
par(mfrow=c(1,1))
plot(bayes.reg) #it seem that mcmc convergeds

par(mfrow=c(1,2))

beta=bayes.reg$Sol

Acf(bayes.reg$VCV)
title(main="ACF of the Posterior distribution")

colMeans(beta) #estimates of the coeff, we MUST check the density of each
#parameters in order to get the right estimate:
#for example if the density of one param is not symmetric, the mean is not
#a good estimate
summary(bayes.reg)

HPDinterval(beta,.95)
sigma2=bayes.reg$VCV
posterior.mode(beta)
posterior.mode(sigma2)


apply(beta, 2,median)
apply(sigma2, 2,median)
plot(bayes.reg)
summary(bayes.reg)


predict(bayes.reg)
plot(BostonHousing$crim, predict(bayes.reg),pch=16)
predict(bayes.reg,interval="confidence")
predict(bayes.reg,interval="prediction")

SCT <- sum((log(BostonHousing$crim)-mean(log(BostonHousing$crim)))^2)
yhat = predict(bayes.reg,BostonHousing)
R2 = 1-sum((log(BostonHousing$crim) - yhat)^2)/SCT
R2


library(glmmLasso)
#lm with no random effects 
#lambda is a tuning parameters
lm1=glmmLasso(formula,rnd=NULL,data=BostonHousing,lambda=10)
lm1$coefficients

summary(lm1)


# train-val-test set

spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(
  seq(nrow(BostonHousing)), 
  nrow(BostonHousing)*cumsum(c(0,spec)),
  labels = names(spec)
))
res = split(BostonHousing, g)
names(res)


R2 <- 0.5
best <- c(R2,0)
SCT <- sum((log(res$validate$crim)-mean(log(res$validate$crim)))^2)
for(L in seq(0.01,20,0.01)){
  lm1=glmmLasso(formula,rnd=NULL,data=res$train,lambda=L)
  yhat = predict(lm1,res$validate)
  R2 = 1-sum((log(res$validate$crim) - yhat)^2)/SCT
  if(best[1]<R2){best<-c(R2,L)}
}
best

lm1=glmmLasso(formula,rnd=NULL,data=res$train,lambda=best[2])
SCT <- sum((log(res$test$crim)-mean(log(res$test$crim)))^2)
yhat = predict(lm1,res$test)
R2 = 1-sum((log(res$test$crim) - yhat)^2)/SCT
R2


summary(lm1)







