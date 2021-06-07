library(MASS)
library(car)
library(leaps)
library(lmtest)
mydata <- read.csv("D:/Study/Statistics/Project/Project.csv")
head(mydata)
summary(mydata)
str(mydata)

#Selecting the required data points in the data-frame
data <- mydata[,3:10]
summary(data)

#Standardizing the data
data[1:8] <- scale(data[1:8])
summary(data)


str(data)
par(mfrow = c(2, 2))
#abline(m1, col="red")


pairs(data, panel = panel.smooth)
round(cor(data),digits = 2)  # rounded to 2 decimals

#Regressing against all the dependent variables
m0 <- lm(HDI~., data=data)
summary(m0)
plot(m0)
vif(m0)
bptest(m0)

par(mfrow=c(2,2))

#Adding interactions
model <- lm(HDI~GDP*Water*Internet+I(GDP^2)+I(Water^2)+I(Internet^2), data=data)

#Using the step function to minimize AIC
modelstep <- step(model)
summary(modelstep)
par(mfrow=c(2,2))
plot(modelstep)
vif(modelstep)
bptest(modelstep)
cooks.distance(modelstep)>1
durbinWatsonTest(modelstep)

#Removing GDP^2 term
modelstep <- lm(HDI ~ GDP + Water + Internet + I(Internet^2) + 
                  GDP:Water + GDP:Internet + Water:Internet + 
                  GDP:Water:Internet, data = data)
summary(modelstep)
par(mfrow=c(2,2))
plot(modelstep)
bptest(modelstep)
vif(modelstep)
max(cooks.distance(modelstep))
anova(modelstep)

durbinWatsonTest(modelstep)
shapiro.test(modelstep$residuals)


ggplot(data, aes(modelstep$residuals)) +
  geom_histogram(aes(y = stat(density))) +
  geom_density(col = "red")



